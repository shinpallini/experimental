use std::fmt;
use anyhow::{Context, Result};
enum MyError {
    Io(std::io::Error),
    Num(std::num::ParseIntError),
}

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MyError::Io(cause) => write!(f, "I/O Error: {}", cause),
            MyError::Num(cause) => write!(f, "Parse Error: {}", cause),
        }
    }
}

impl From<std::io::Error> for MyError {
    fn from(cause: std::io::Error) -> Self {
        Self::Io(cause)
    }
}

fn get_int_from_file() -> Result<i32> {
    let path = "number.txt";

    // 明示的に変数をbindしないときは、map, map_errの中に指定する関数の第一引数に自動的に適用する
    let num_str = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read string from {}", path))?;
    num_str
        .trim()
        .parse::<i32>()
        .map(|t| t * 2)
        .context("failed to parse string")
}

fn main() {
    match get_int_from_file() {
        Ok(x) => println!("{}", x),
        Err(e) => println!("{:#?}", e)
    }
}