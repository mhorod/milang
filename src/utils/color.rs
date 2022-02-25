use std::borrow::Cow;
use std::fmt;

pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    White,
    True { r: u8, g: u8, b: u8 },
}

pub enum Style {
    Bold,
}

impl Color {
    fn to_fg_str(&self) -> Cow<'static, str> {
        match *self {
            Color::Black => "30".into(),
            Color::Red => "31".into(),
            Color::Green => "32".into(),
            Color::Yellow => "33".into(),
            Color::White => "37".into(),
            Color::True { r, g, b } => format!("38;2;{};{};{}", r, g, b).into(),
        }
    }
}

impl Style {
    fn to_code_str(&self) -> Cow<'static, str> {
        match *self {
            Style::Bold => "1".into(),
        }
    }
}

pub struct ColoredString {
    text: String,
    fg: Option<Color>,
    bg: Option<Color>,
    styles: Vec<Style>,
}

impl ColoredString {
    fn ansi_sequence(&self) -> String {
        let mut result = String::from("\x1b[");
        let mut style_str: String = self.styles.iter().map(|s| s.to_code_str() + ";").collect();

        if !self.has_color() {
            style_str.pop();
        }

        result.push_str(&style_str);
        match &self.fg {
            Some(c) => result.push_str(&c.to_fg_str()),
            None => {}
        }
        result.push('m');
        result
    }

    fn is_plain(&self) -> bool {
        self.fg.is_none() && self.bg.is_none() && self.styles.len() == 0
    }

    fn has_color(&self) -> bool {
        self.fg.is_some() || self.bg.is_some()
    }
}

impl fmt::Display for ColoredString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_plain() {
            f.write_str(&self.text)?;
        } else {
            f.write_str(&self.ansi_sequence())?;
            f.write_str(&self.text)?;
            f.write_str("\x1b[0m")?; // Reset sequence
        }
        Ok(())
    }
}

// Types that can be colored into ColoredString
pub trait Colorize {
    fn color(self, color: Color) -> ColoredString;
    fn add_style(self, style: Style) -> ColoredString;
    fn red(self) -> ColoredString
    where
        Self: Sized,
    {
        self.color(Color::Red)
    }
    fn bold(self) -> ColoredString
    where
        Self: Sized,
    {
        self.add_style(Style::Bold)
    }
}

impl<'a> Colorize for &'a str {
    fn color(self, color: Color) -> ColoredString {
        ColoredString {
            text: String::from(self),
            fg: Some(color),
            bg: None,
            styles: Vec::new(),
        }
    }

    fn add_style(self, style: Style) -> ColoredString {
        ColoredString {
            text: String::from(self),
            fg: None,
            bg: None,
            styles: vec![style],
        }
    }
}

impl Colorize for ColoredString {
    fn color(mut self, color: Color) -> ColoredString {
        self.fg = Some(color);
        self
    }
    fn add_style(mut self, style: Style) -> ColoredString {
        self.styles.push(style);
        self
    }
}
