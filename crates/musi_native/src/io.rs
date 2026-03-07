use musi_macros::musi_module;

#[musi_module]
pub mod io {
    pub fn writeln(s: &str) -> () {
        println!("{s}");
    }

    pub fn write(s: &str) -> () {
        print!("{s}");
    }

    pub fn read_line() -> String {
        use std::io::BufRead as _;
        let stdin = std::io::stdin();
        let mut line = String::new();
        let _ = stdin.lock().read_line(&mut line);
        if line.ends_with('\n') {
            let _ = line.pop();
            if line.ends_with('\r') {
                let _ = line.pop();
            }
        }
        line
    }
}
