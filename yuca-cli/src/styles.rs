use owo_colors::{OwoColorize, Stream, Style, Styled, SupportsColorsDisplay};

pub fn info() -> Style {
    Style::new().italic()
}

pub fn error_label() -> Style {
    Style::new().bright_red().bold()
}

pub fn error_body() -> Style {
    Style::new().bright_red()
}

pub fn tree_opening() -> Style {
    Style::new().bright_green().bold()
}

pub fn property_star() -> Style {
    Style::new().blue().bold()
}

pub fn label() -> Style {
    Style::new().bold()
}

pub fn device_index() -> Style {
    Style::new().yellow().bold()
}

pub fn number() -> Style {
    Style::new().yellow()
}

// A literal string value for a property (e.g. "USB PD").
pub fn literal() -> Style {
    Style::new().bright_cyan()
}

// Things like 'true' / 'false' and additional qualifiers on a literal.
pub fn special() -> Style {
    Style::new().magenta()
}

pub fn dim() -> Style {
    Style::new().bright_black()
}

pub fn event_add() -> Style {
    Style::new().green().bold()
}

pub fn event_change() -> Style {
    Style::new().cyan().bold()
}

pub fn event_remove() -> Style {
    Style::new().magenta().bold()
}

pub trait StyleApply: OwoColorize {
    fn style_if_supported<'a>(
        &'a self,
        stream: Stream,
        style: Style,
    ) -> SupportsColorsDisplay<'a, Self, Styled<&'a Self>, impl Fn(&'a Self) -> Styled<&'a Self>>;
}

impl<T: OwoColorize> StyleApply for T {
    fn style_if_supported<'a>(
        &'a self,
        stream: Stream,
        style: Style,
    ) -> SupportsColorsDisplay<'a, Self, Styled<&'a Self>, impl Fn(&'a Self) -> Styled<&'a Self>>
    {
        self.if_supports_color(stream, move |s| s.style(style))
    }
}
