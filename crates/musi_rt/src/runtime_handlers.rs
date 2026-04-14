use std::cell::RefCell;
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process::Command;
use std::rc::Rc;
use std::str::from_utf8;
use std::sync::OnceLock;
use std::time::{Instant, SystemTime, UNIX_EPOCH};

use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64_STANDARD;
use musi_foundation::runtime as foundation_runtime;
use musi_native::NativeHost;
use musi_vm::{EffectCall, Value, VmError, VmErrorKind};

type RandomStateCell = Rc<RefCell<u64>>;

mod env;
mod process;
mod time_random;

pub fn register_runtime_handlers(host: &mut NativeHost) {
    env::register(host);
    process::register(host);
    time_random::register(host);
    register_log_io_handlers(host);
    register_fs_handlers(host);
    register_text_handlers(host);
    register_path_handlers(host);
    register_json_handlers(host);
    register_encoding_handlers(host);
}

fn register_log_io_handlers(host: &mut NativeHost) {
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::LOG_INFO_OP,
        |effect, args| {
            let [Value::String(message)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid logInfo args"));
            };
            eprintln!("[musi:runtime] {}", message.as_ref());
            Ok(Value::Unit)
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::LOG_WRITE_OP,
        |effect, args| {
            let [Value::Int(level), Value::String(message)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid logWrite args"));
            };
            eprintln!("[std:{level}] {}", message.as_ref());
            Ok(Value::Unit)
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::IO_PRINT_OP,
        |effect, args| write_stream(effect, args, StreamKind::Stdout, false),
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::IO_PRINT_LINE_OP,
        |effect, args| write_stream(effect, args, StreamKind::Stdout, true),
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::IO_PRINT_ERROR_OP,
        |effect, args| write_stream(effect, args, StreamKind::Stderr, false),
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::IO_PRINT_ERROR_LINE_OP,
        |effect, args| write_stream(effect, args, StreamKind::Stderr, true),
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::IO_READ_LINE_OP,
        |effect, args| {
            if !args.is_empty() {
                return Err(invalid_runtime_effect(effect, "invalid ioReadLine args"));
            }
            Ok(Value::string(read_line(effect)?))
        },
    );
}

fn register_fs_handlers(host: &mut NativeHost) {
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::FS_READ_TEXT_OP,
        |effect, args| {
            let [Value::String(path)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid fsReadText args"));
            };
            let text = fs::read_to_string(path.as_ref()).map_err(|error| {
                invalid_runtime_effect(effect, format!("fsReadText failed (`{error}`)"))
            })?;
            Ok(Value::string(text))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::FS_WRITE_TEXT_OP,
        |effect, args| {
            let [Value::String(path), Value::String(text)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid fsWriteText args"));
            };
            fs::write(path.as_ref(), text.as_ref()).map_err(|error| {
                invalid_runtime_effect(effect, format!("fsWriteText failed (`{error}`)"))
            })?;
            Ok(Value::Unit)
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::FS_EXISTS_OP,
        |effect, args| {
            let [Value::String(path)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid fsExists args"));
            };
            Ok(Value::Int(i64::from(Path::new(path.as_ref()).exists())))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::FS_APPEND_TEXT_OP,
        |effect, args| {
            let [Value::String(path), Value::String(text)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid fsAppendText args"));
            };
            let result = fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(path.as_ref())
                .and_then(|mut file| file.write_all(text.as_bytes()));
            Ok(Value::Int(i64::from(result.is_ok())))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::FS_REMOVE_OP,
        |effect, args| {
            let [Value::String(path)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid fsRemove args"));
            };
            Ok(Value::Int(i64::from(
                fs::remove_file(path.as_ref()).is_ok(),
            )))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::FS_CREATE_DIR_ALL_OP,
        |effect, args| {
            let [Value::String(path)] = args else {
                return Err(invalid_runtime_effect(
                    effect,
                    "invalid fsCreateDirAll args",
                ));
            };
            Ok(Value::Int(i64::from(
                fs::create_dir_all(path.as_ref()).is_ok(),
            )))
        },
    );
}

fn register_text_handlers(host: &mut NativeHost) {
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::TEXT_LENGTH_OP,
        |effect, args| {
            let [Value::String(value)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid textLength args"));
            };
            Ok(Value::Int(saturating_usize_to_i64(value.chars().count())))
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::TEXT_TRIM_OP,
        |effect, args| {
            transform_string_arg(effect, args, "textTrim", |value| value.trim().to_owned())
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::TEXT_TO_LOWERCASE_OP,
        |effect, args| transform_string_arg(effect, args, "textToLowerCase", str::to_lowercase),
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::TEXT_TO_UPPERCASE_OP,
        |effect, args| transform_string_arg(effect, args, "textToUpperCase", str::to_uppercase),
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::TEXT_CONTAINS_OP,
        |effect, args| {
            text_predicate(effect, args, "textContains", |value, needle| {
                value.contains(needle)
            })
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::TEXT_STARTS_WITH_OP,
        |effect, args| {
            text_predicate(effect, args, "textStartsWith", |value, needle| {
                value.starts_with(needle)
            })
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::TEXT_ENDS_WITH_OP,
        |effect, args| {
            text_predicate(effect, args, "textEndsWith", |value, needle| {
                value.ends_with(needle)
            })
        },
    );
}

fn register_path_handlers(host: &mut NativeHost) {
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::PATH_JOIN_OP,
        |effect, args| {
            let [Value::String(left), Value::String(right)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid pathJoin args"));
            };
            Ok(Value::string(
                Path::new(left.as_ref())
                    .join(right.as_ref())
                    .display()
                    .to_string(),
            ))
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::PATH_NORMALIZE_OP,
        |effect, args| {
            let [Value::String(value)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid pathNormalize args"));
            };
            Ok(Value::string(
                Path::new(value.as_ref())
                    .components()
                    .as_path()
                    .display()
                    .to_string(),
            ))
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::PATH_DIRNAME_OP,
        |effect, args| {
            let [Value::String(value)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid pathDirname args"));
            };
            let dirname = Path::new(value.as_ref())
                .parent()
                .map(|path| path.display().to_string())
                .unwrap_or_default();
            Ok(Value::string(dirname))
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::PATH_BASENAME_OP,
        |effect, args| {
            let [Value::String(value)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid pathBasename args"));
            };
            let basename = Path::new(value.as_ref())
                .file_name()
                .map(|name| name.to_string_lossy().into_owned())
                .unwrap_or_default();
            Ok(Value::string(basename))
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::PATH_EXTNAME_OP,
        |effect, args| {
            let [Value::String(value)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid pathExtname args"));
            };
            let extname = Path::new(value.as_ref())
                .extension()
                .map(|ext| format!(".{}", ext.to_string_lossy()))
                .unwrap_or_default();
            Ok(Value::string(extname))
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::PATH_IS_ABSOLUTE_OP,
        |effect, args| {
            let [Value::String(value)] = args else {
                return Err(invalid_runtime_effect(
                    effect,
                    "invalid pathIsAbsolute args",
                ));
            };
            Ok(Value::Int(i64::from(
                Path::new(value.as_ref()).is_absolute(),
            )))
        },
    );
}

fn register_json_handlers(host: &mut NativeHost) {
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::JSON_IS_VALID_OP,
        |effect, args| {
            let [Value::String(source)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid jsonIsValid args"));
            };
            Ok(Value::Int(i64::from(
                serde_json::from_str::<serde_json::Value>(source).is_ok(),
            )))
        },
    );

    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::JSON_NORMALIZE_OP,
        |effect, args| {
            let [Value::String(source)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid jsonNormalize args"));
            };
            Ok(Value::string(normalize_json(source, effect)?))
        },
    );
}

fn register_encoding_handlers(host: &mut NativeHost) {
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::HEX_ENCODE_OP,
        |effect, args| {
            let [Value::String(source)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid hexEncode args"));
            };
            Ok(Value::string(hex_encode(source.as_bytes())))
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::HEX_DECODE_OP,
        |effect, args| decode_utf8_encoded(effect, args, "hexDecode", decode_hex),
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::HEX_IS_VALID_OP,
        |effect, args| {
            let [Value::String(source)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid hexIsValid args"));
            };
            Ok(Value::Int(i64::from(decode_hex(source.as_ref()).is_ok())))
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::BASE64_ENCODE_OP,
        |effect, args| {
            let [Value::String(source)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid base64Encode args"));
            };
            Ok(Value::string(BASE64_STANDARD.encode(source.as_bytes())))
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::BASE64_DECODE_OP,
        |effect, args| {
            decode_utf8_encoded(effect, args, "base64Decode", |source| {
                BASE64_STANDARD
                    .decode(source)
                    .map_err(|error| format!("base64Decode failed (`{error}`)").into())
            })
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::BASE64_IS_VALID_OP,
        |effect, args| {
            let [Value::String(source)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid base64IsValid args"));
            };
            Ok(Value::Int(i64::from(
                BASE64_STANDARD.decode(source.as_ref()).is_ok(),
            )))
        },
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::UTF8_ENCODE_OP,
        |effect, args| transform_string_arg(effect, args, "utf8Encode", str::to_owned),
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::UTF8_DECODE_OP,
        |effect, args| transform_string_arg(effect, args, "utf8Decode", str::to_owned),
    );
    host.register_effect_handler(
        foundation_runtime::EFFECT,
        foundation_runtime::UTF8_IS_VALID_OP,
        |effect, args| {
            let [Value::String(bytes)] = args else {
                return Err(invalid_runtime_effect(effect, "invalid utf8IsValid args"));
            };
            Ok(Value::Int(i64::from(from_utf8(bytes.as_bytes()).is_ok())))
        },
    );
}

fn invalid_runtime_effect(effect: &EffectCall, reason: impl Into<Box<str>>) -> VmError {
    VmError::new(VmErrorKind::EffectRejected {
        effect: effect.effect_name().into(),
        op: Some(effect.op_name().into()),
        reason: reason.into(),
    })
}

fn current_unix_millis(effect: &EffectCall) -> Result<i64, VmError> {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|error| {
            invalid_runtime_effect(effect, format!("timeNowUnixMs failed (`{error}`)"))
        })?;
    Ok(i64::try_from(now.as_millis()).unwrap_or(i64::MAX))
}

fn run_shell_command(command: &str, effect: &EffectCall) -> Result<i64, VmError> {
    let status = if cfg!(windows) {
        Command::new("cmd")
            .args([windows_shell_flag().as_str(), command])
            .status()
    } else {
        Command::new("sh").args(["-c", command]).status()
    }
    .map_err(|error| invalid_runtime_effect(effect, format!("processRun failed (`{error}`)")))?;
    Ok(i64::from(status.code().unwrap_or(-1)))
}

enum StreamKind {
    Stdout,
    Stderr,
}

fn write_stream(
    effect: &EffectCall,
    args: &[Value],
    stream: StreamKind,
    line: bool,
) -> Result<Value, VmError> {
    let [Value::String(text)] = args else {
        let op = match (stream, line) {
            (StreamKind::Stdout, false) => "ioPrint",
            (StreamKind::Stdout, true) => "ioPrintLine",
            (StreamKind::Stderr, false) => "ioPrintError",
            (StreamKind::Stderr, true) => "ioPrintErrorLine",
        };
        return Err(invalid_runtime_effect(effect, format!("invalid {op} args")));
    };
    match (stream, line) {
        (StreamKind::Stdout, true) => println!("{}", text.as_ref()),
        (StreamKind::Stderr, true) => eprintln!("{}", text.as_ref()),
        (StreamKind::Stdout, false) => {
            print!("{}", text.as_ref());
            io::stdout().flush().map_err(|error| {
                invalid_runtime_effect(effect, format!("ioPrint failed (`{error}`)"))
            })?;
        }
        (StreamKind::Stderr, false) => {
            eprint!("{}", text.as_ref());
            io::stderr().flush().map_err(|error| {
                invalid_runtime_effect(effect, format!("ioPrintError failed (`{error}`)"))
            })?;
        }
    }
    Ok(Value::Unit)
}

fn read_line(effect: &EffectCall) -> Result<String, VmError> {
    let mut line = String::new();
    let _bytes = io::stdin().read_line(&mut line).map_err(|error| {
        invalid_runtime_effect(effect, format!("ioReadLine failed (`{error}`)"))
    })?;
    if line.ends_with('\n') {
        let _ = line.pop();
        if line.ends_with('\r') {
            let _ = line.pop();
        }
    }
    Ok(line)
}

fn transform_string_arg(
    effect: &EffectCall,
    args: &[Value],
    op_name: &str,
    f: impl FnOnce(&str) -> String,
) -> Result<Value, VmError> {
    let [Value::String(value)] = args else {
        return Err(invalid_runtime_effect(
            effect,
            format!("invalid {op_name} args"),
        ));
    };
    Ok(Value::string(f(value.as_ref())))
}

fn text_predicate(
    effect: &EffectCall,
    args: &[Value],
    op_name: &str,
    f: impl FnOnce(&str, &str) -> bool,
) -> Result<Value, VmError> {
    let [Value::String(value), Value::String(needle)] = args else {
        return Err(invalid_runtime_effect(
            effect,
            format!("invalid {op_name} args"),
        ));
    };
    Ok(Value::Int(i64::from(f(value.as_ref(), needle.as_ref()))))
}

fn normalize_json(source: &str, effect: &EffectCall) -> Result<String, VmError> {
    let parsed: serde_json::Value = serde_json::from_str(source).map_err(|error| {
        invalid_runtime_effect(effect, format!("jsonNormalize failed (`{error}`)"))
    })?;
    serde_json::to_string(&parsed).map_err(|error| {
        invalid_runtime_effect(effect, format!("jsonNormalize failed (`{error}`)"))
    })
}

fn decode_utf8_encoded(
    effect: &EffectCall,
    args: &[Value],
    op_name: &str,
    decoder: impl FnOnce(&str) -> Result<Vec<u8>, Box<str>>,
) -> Result<Value, VmError> {
    let [Value::String(source)] = args else {
        return Err(invalid_runtime_effect(
            effect,
            format!("invalid {op_name} args"),
        ));
    };
    let bytes =
        decoder(source.as_ref()).map_err(|reason| invalid_runtime_effect(effect, reason))?;
    let text = String::from_utf8(bytes)
        .map_err(|error| invalid_runtime_effect(effect, format!("{op_name} failed (`{error}`)")))?;
    Ok(Value::string(text))
}

fn hex_encode(bytes: &[u8]) -> String {
    const HEX_DIGITS: &[u8; 16] = b"0123456789abcdef";
    let mut out = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        out.push(char::from(HEX_DIGITS[usize::from(byte >> 4)]));
        out.push(char::from(HEX_DIGITS[usize::from(byte & 0x0f)]));
    }
    out
}

fn decode_hex(source: &str) -> Result<Vec<u8>, Box<str>> {
    if !source.len().is_multiple_of(2) {
        return Err("hexDecode failed (`odd-length input`)".into());
    }
    let mut bytes = Vec::with_capacity(source.len() / 2);
    let chars: Vec<char> = source.chars().collect();
    let mut index = 0;
    while index < chars.len() {
        let chunk = [chars[index], chars[index + 1]]
            .into_iter()
            .collect::<String>();
        let byte = u8::from_str_radix(&chunk, 16)
            .map_err(|_| format!("hexDecode failed (`invalid byte `{chunk}``)"))?;
        bytes.push(byte);
        index += 2;
    }
    Ok(bytes)
}

fn random_seed() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |duration| {
            let nanos = duration.as_nanos();
            u64::try_from(nanos).unwrap_or(u64::MAX)
        })
}

fn next_random_int(state: &RandomStateCell) -> i64 {
    let mut value = state.borrow_mut();
    *value = value
        .wrapping_mul(6_364_136_223_846_793_005)
        .wrapping_add(1);
    i64::from_ne_bytes(value.to_ne_bytes()) & i64::MAX
}

fn random_int_in_range(state: &RandomStateCell, lower_bound: i64, upper_bound: i64) -> i64 {
    if upper_bound <= lower_bound {
        return lower_bound;
    }
    let span = u64::try_from(upper_bound - lower_bound).unwrap_or(0);
    if span == 0 {
        return lower_bound;
    }
    let raw = u64::try_from(next_random_int(state)).unwrap_or(0);
    let offset = i64::try_from(raw % span).unwrap_or(0);
    lower_bound + offset
}

fn random_float01(state: &RandomStateCell) -> f64 {
    let raw_bits = u32::try_from(next_random_int(state) & i64::from(u32::MAX)).unwrap_or(u32::MAX);
    f64::from(raw_bits) / f64::from(u32::MAX)
}

fn saturating_usize_to_i64(value: usize) -> i64 {
    i64::try_from(value).unwrap_or(i64::MAX)
}

fn monotonic_origin() -> &'static Instant {
    static ORIGIN: OnceLock<Instant> = OnceLock::new();
    ORIGIN.get_or_init(Instant::now)
}

fn windows_shell_flag() -> String {
    ['/', 'C'].into_iter().collect()
}
