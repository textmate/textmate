//! Theme preview: lifetimes, traits, macros, patterns, and ownership.
use std::collections::HashMap;
use std::fmt::{self, Display};

const DEFAULT_LIMIT: usize = 1_024;
static APP_NAME: &str = "TextMate";

#[derive(Debug, Clone, PartialEq)]
pub struct Theme<'a> {
    pub name: &'a str,
    dark: bool,
    colors: HashMap<String, String>,
}

impl<'a> Theme<'a> {
    /// Build a theme with an empty palette.
    pub fn new(name: &'a str) -> Self {
        Self { name, dark: false, colors: HashMap::new() }
    }

    pub fn color(&self, key: &str) -> Option<&str> {
        self.colors.get(key).map(String::as_str)
    }

    pub fn set_color(&mut self, key: &str, value: &str) -> Result<(), String> {
        if !value.starts_with('#') {
            return Err(format!("Invalid color: {value}"));
        }
        self.colors.insert(key.to_owned(), value.to_owned());
        Ok(())
    }
}

impl Display for Theme<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.name, if self.dark { "dark" } else { "light" })
    }
}

fn summarize<T: Display>(values: &[T]) -> String {
    values.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")
}

fn main() -> Result<(), String> {
    let mut theme = Theme::new(APP_NAME);
    theme.set_color("accent", "#58a6ff")?;
    let values = [0xff_u32, 0b1010, 42, DEFAULT_LIMIT as u32];
    let ratio = 3.14159_f64;
    let literal = r#"A raw string with "quotes" and \slashes"#;
    let escaped = "Tab:\t newline:\n star:\u{2605}";

    match theme.color("accent") {
        Some(color) if color.len() == 7 => println!("Accent: {color}"),
        Some(_) => eprintln!("Unexpected color format"),
        None => println!("No accent"),
    }
    for (index, value) in values.iter().enumerate() {
        println!("{index}: {value:#x}");
    }
    println!("{}: {} {ratio} {literal} {escaped}", theme, summarize(&values));
    Ok(())
}
