/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use std::collections::VecDeque;
use std::convert::TryFrom;
use std::io;

use devtools_traits::{ConsoleMessage, LogLevel, ScriptToDevtoolsControlMsg};
use js::jsapi::{self, ESClass, Heap, JSTracer, PropertyDescriptor};
use js::jsval::{JSVal, UndefinedValue};
use js::rust::wrappers::{
    GetBuiltinClass, GetPropertyKeys, JS_GetOwnPropertyDescriptorById, JS_GetPropertyById,
    JS_IdToValue, JS_ValueToSource,
};
use js::rust::{describe_scripted_caller, HandleValue, IdVector, RootedGuard, RootedTraceableBox, RootedTraceableSet, Trace};
use js::gc::Traceable;

use crate::dom::bindings::conversions::jsstring_to_str;
use crate::dom::bindings::inheritance::Castable;
use crate::dom::bindings::str::DOMString;
use crate::dom::globalscope::GlobalScope;
use crate::dom::workerglobalscope::WorkerGlobalScope;
use crate::script_runtime::JSContext;

/// The maximum object depth logged by console methods.
const MAX_LOG_DEPTH: usize = 10;

// https://developer.mozilla.org/en-US/docs/Web/API/Console
pub struct Console(());

impl Console {
    #[allow(unsafe_code)]
    fn send_to_devtools(global: &GlobalScope, level: LogLevel, message: String) {
        if let Some(chan) = global.devtools_chan() {
            let caller =
                unsafe { describe_scripted_caller(*GlobalScope::get_cx()) }.unwrap_or_default();
            let console_message = ConsoleMessage {
                message,
                logLevel: level,
                filename: caller.filename,
                lineNumber: caller.line as usize,
                columnNumber: caller.col as usize,
            };
            let worker_id = global
                .downcast::<WorkerGlobalScope>()
                .map(|worker| worker.get_worker_id());
            let devtools_message = ScriptToDevtoolsControlMsg::ConsoleAPI(
                global.pipeline_id(),
                console_message,
                worker_id,
            );
            chan.send(devtools_message).unwrap();
        }
    }
}

// In order to avoid interleaving the stdout output of the Console API methods
// with stderr that could be in use on other threads, we lock stderr until
// we're finished with stdout. Since the stderr lock is reentrant, there is
// no risk of deadlock if the callback ends up trying to write to stderr for
// any reason.
fn with_stderr_lock<F>(f: F)
where
    F: FnOnce(),
{
    let stderr = io::stderr();
    let _handle = stderr.lock();
    f()
}

#[allow(unsafe_code)]
unsafe fn handle_value_to_string(cx: *mut jsapi::JSContext, value: HandleValue) -> String {
    rooted!(in(cx) let mut js_string = std::ptr::null_mut::<jsapi::JSString>());
    js_string.set(JS_ValueToSource(cx, value));
    return jsstring_to_str(cx, *js_string).to_string();
}

#[allow(unsafe_code)]
fn stringify_handle_value(value: HandleValue) -> DOMString {
    struct ValueInfo {
        key: Option<String>,
        value: Box<Heap<JSVal>>,
        object_class: Option<ESClass>,
    }
    unsafe impl Trace for ValueInfo {
        unsafe fn trace(&self, trc: *mut JSTracer) {
            self.value.trace(trc);
        }
    }

    unsafe {
        let cx = *GlobalScope::get_cx();
        if value.is_string() {
            return jsstring_to_str(cx, value.to_string());
        }

        let mut parents = vec![VecDeque::new()];
        parents[0].push_back(ValueInfo {
            value: Heap::boxed(*value),
            key: None,
            object_class: None,
        });
        let mut str = String::new();
        'outer: loop {
            let current_value = parents.last().unwrap().back().unwrap();
            if parents.len() >= MAX_LOG_DEPTH {
                parents.last().unwrap().pop_back();
                str.push_str("...");
            } else if current_value.value.get().is_undefined() {
                parents.last().unwrap().pop_back();
                // This produces a better value than "(void 0)" from JS_ValueToSource.
                str.push_str("undefined");
            } else if !current_value.value.get().is_object() {
                parents.last().unwrap().pop_back();
                str.push_str(&handle_value_to_string(cx, current_value.value.get()));
            } else if parents.iter().find(|parent| parent[0].value.get().asBits_ == current_value.value.get().asBits_).is_some() {
                parents.last().unwrap().pop_back();
                str.push_str("[circular]");
            } else {
                let mut current_value = parents.last_mut().unwrap().back_mut().unwrap();
                rooted!(in(cx) let mut obj = current_value.value.get().to_object());
                let (str_text, children) = (|| {
                    let mut object_class = ESClass::Other;
                    if !GetBuiltinClass(cx, obj.handle().into(), &mut object_class as *mut _) {
                        return ("/* invalid */".to_string(), VecDeque::new());
                    }
                    if object_class != ESClass::Array && object_class != ESClass::Object {
                        return (handle_value_to_string(cx, current_value.value.get()), VecDeque::new());
                    }
                    current_value.object_class = Some(object_class);
        
                    let mut ids = IdVector::new(cx);
                    if !GetPropertyKeys(
                        cx,
                        obj.handle().into(),
                        jsapi::JSITER_OWNONLY | jsapi::JSITER_SYMBOLS,
                        ids.handle_mut(),
                    ) {
                        return ("/* invalid */".to_string(), VecDeque::new());
                    }
                    let mut explicit_keys = object_class == ESClass::Object;
                    let mut props = VecDeque::with_capacity(ids.len());
                    for id in &*ids {
                        rooted!(in(cx) let id = *id);
                        rooted!(in(cx) let mut desc = PropertyDescriptor::default());
        
                        let mut is_none = false;
                        if !JS_GetOwnPropertyDescriptorById(
                            cx,
                            obj.handle().into(),
                            id.handle().into(),
                            desc.handle_mut().into(),
                            &mut is_none,
                        ) {
                            return ("/* invalid */".to_string(), VecDeque::new());
                        }
        
                        rooted!(in(cx) let mut property = UndefinedValue());
                        if !JS_GetPropertyById(cx, obj.handle(), id.handle(), property.handle_mut())
                        {
                            return ("/* invalid */".to_string(), VecDeque::new());
                        }
        
                        if !explicit_keys {
                            if id.is_int() {
                                if let Ok(id_int) = usize::try_from(id.to_int()) {
                                    explicit_keys = props.len() != id_int;
                                } else {
                                    explicit_keys = false;
                                }
                            } else {
                                explicit_keys = false;
                            }
                        }
                        let key = if explicit_keys {
                            if id.is_string() || id.is_symbol() || id.is_int() {
                                rooted!(in(cx) let mut key_value = UndefinedValue());
                                let raw_id: jsapi::HandleId = id.handle().into();
                                if !JS_IdToValue(cx, *raw_id.ptr, key_value.handle_mut()) {
                                    return ("/* invalid */".to_string(), VecDeque::new());
                                }
                                Some(handle_value_to_string(cx, key_value.handle()))
                            } else {
                                return ("/* invalid */".to_string(), VecDeque::new());
                            }
                        } else {
                            None
                        };
                        props.push_back(ValueInfo {
                            key,
                            value: property.handle(),
                            object_class: Some(object_class),
                        });
                    }
                    let opener = if object_class == ESClass::Array {
                        "[".to_string()
                    } else {
                        "{".to_string()
                    };
                    (opener, props)
                })();
                parents.push(children);
            };
            loop {
                let last_parent = if let Some(last_parent) = parents.last_mut() {
                    last_parent
                } else {
                    break 'outer;
                };
                if last_parent.is_empty() {
                    parents.pop();
                    if let Some(last_parent) = parents.last_mut() {
                        let finished_object = last_parent.pop_front().unwrap();
                        str.push(match finished_object.object_class {
                            Some(ESClass::Array) => ']',
                            Some(_) => '}',
                            None => unreachable!(),
                        });
                    } else {
                        break 'outer;
                    }
                }
            }
        }
        DOMString::from(str)
    }
}

fn stringify_handle_values(messages: Vec<HandleValue>) -> DOMString {
    DOMString::from(itertools::join(
        messages.into_iter().map(stringify_handle_value),
        " ",
    ))
}

fn console_messages(global: &GlobalScope, messages: Vec<HandleValue>, level: LogLevel) {
    let message = stringify_handle_values(messages);
    console_message(global, message, level)
}

fn console_message(global: &GlobalScope, message: DOMString, level: LogLevel) {
    with_stderr_lock(move || {
        let prefix = global.current_group_label().unwrap_or_default();
        let message = format!("{}{}", prefix, message);
        println!("{}", message);
        Console::send_to_devtools(global, level, message);
    })
}

#[allow(non_snake_case)]
impl Console {
    // https://developer.mozilla.org/en-US/docs/Web/API/Console/log
    pub fn Log(_cx: JSContext, global: &GlobalScope, messages: Vec<HandleValue>) {
        console_messages(global, messages, LogLevel::Log)
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/Console/clear
    pub fn Clear(global: &GlobalScope) {
        let message: Vec<HandleValue> = Vec::new();
        console_messages(global, message, LogLevel::Clear)
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/Console
    pub fn Debug(_cx: JSContext, global: &GlobalScope, messages: Vec<HandleValue>) {
        console_messages(global, messages, LogLevel::Debug)
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/Console/info
    pub fn Info(_cx: JSContext, global: &GlobalScope, messages: Vec<HandleValue>) {
        console_messages(global, messages, LogLevel::Info)
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/Console/warn
    pub fn Warn(_cx: JSContext, global: &GlobalScope, messages: Vec<HandleValue>) {
        console_messages(global, messages, LogLevel::Warn)
    }
    // Directly logs a DOMString, without processing the message
    pub fn internal_warn(global: &GlobalScope, message: DOMString) {
        console_message(global, message, LogLevel::Warn)
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/Console/error
    pub fn Error(_cx: JSContext, global: &GlobalScope, messages: Vec<HandleValue>) {
        console_messages(global, messages, LogLevel::Error)
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/Console/assert
    pub fn Assert(_cx: JSContext, global: &GlobalScope, condition: bool, message: HandleValue) {
        if !condition {
            let message = if message.is_undefined() {
                DOMString::from("no message")
            } else {
                stringify_handle_value(message)
            };
            let message = DOMString::from(format!("Assertion failed: {}", message));
            console_message(global, message, LogLevel::Error)
        };
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/Console/time
    pub fn Time(_cx: JSContext, global: &GlobalScope, label: HandleValue) {
        let label = stringify_handle_value(label);
        if let Ok(()) = global.time(label.clone()) {
            let message = DOMString::from(format!("{}: timer started", label));
            console_message(global, message, LogLevel::Log);
        }
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/Console/timeEnd
    pub fn TimeEnd(_cx: JSContext, global: &GlobalScope, label: HandleValue) {
        let label = stringify_handle_value(label);
        if let Ok(delta) = global.time_end(&label) {
            let message = DOMString::from(format!("{}: {}ms", label, delta));
            console_message(global, message, LogLevel::Log);
        }
    }

    // https://console.spec.whatwg.org/#group
    pub fn Group(_cx: JSContext, global: &GlobalScope, messages: Vec<HandleValue>) {
        global.push_console_group(stringify_handle_values(messages));
    }

    // https://console.spec.whatwg.org/#groupcollapsed
    pub fn GroupCollapsed(_cx: JSContext, global: &GlobalScope, messages: Vec<HandleValue>) {
        global.push_console_group(stringify_handle_values(messages));
    }

    // https://console.spec.whatwg.org/#groupend
    pub fn GroupEnd(global: &GlobalScope) {
        global.pop_console_group();
    }
}
