use crate::errors::MusiResult;
use crate::value::Value;

pub fn write(args: &[Value]) -> MusiResult<Value> {
    for arg in args {
        print!("{arg}");
    }
    Ok(Value::Unit)
}

pub fn writeln(args: &[Value]) -> MusiResult<Value> {
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{arg}");
    }
    println!();
    Ok(Value::Unit)
}
