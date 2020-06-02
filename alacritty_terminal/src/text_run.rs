use bitflags::bitflags;

use std::cmp::{Eq, PartialEq};
use std::hash::{Hash, Hasher};

use crate::ansi::Color;
use crate::grid::Indexed;
use crate::index::{Column, Line, Point};
use crate::term::cell::{Cell, Flags};
use crate::term::color::Rgb;
use crate::term::render::RenderableCell;

#[derive(Copy, Debug, Clone, Default)]
pub struct Glyph {
    pub tex_id: std::os::raw::c_uint, // GLuint
    pub multicolor: bool,
    pub top: i16,
    pub left: i16,
    pub width: i16,
    pub height: i16,
    pub uv_bot: f32,
    pub uv_left: f32,
    pub uv_width: f32,
    pub uv_height: f32,
}

bitflags! {
    pub struct UIFlags: u8 {
        const SELECTED       = 0b00001;
        const SEARCH_MATCHED = 0b00010;
        const BLOCK          = 0b00100;
        const CURSOR         = 0b01000;
        const FOCUSED_MATCH  = 0b10000;
    }
}

#[derive(Debug)]
pub struct RunStart {
    pub line: Line,
    pub column: Column,
    pub fg: Color,
    pub bg: Color,
    pub uiflags: UIFlags,
    pub flags: Flags,
}

impl RunStart {
    /// Compare cell and check if it belongs to the same run.
    #[inline]
    pub fn belongs_to_text_run(&self, cell: &Indexed<&Cell>, uiflags: UIFlags) -> bool {
        self.line == cell.line
            && self.fg == cell.fg
            && self.bg == cell.bg
            && self.flags == cell.flags
            && self.uiflags == uiflags
    }
}

/// Represents a set of renderable cells that all share the same rendering properties.
/// The assumption is that if two cells are in the same TextRun they can be sent off together to
/// be shaped. This allows for ligatures to be rendered but not when something breaks up a ligature
/// (e.g. selection highlight) which is desired behavior.
#[derive(Debug)]
pub struct TextRun {
    /// A run never spans multiple lines.
    pub line: Line,
    /// Span of columns the text run covers.
    pub span: (Column, Column),
    /// Sequence of characters.
    pub content: (String, Vec<Option<Vec<char>>>),
    /// Foreground color of text run content.
    pub fg: Rgb,
    /// Background color of text run content.
    pub bg: Rgb,
    /// Background color opacity of the text run.
    pub bg_alpha: f32,
    /// Attributes of this text run.
    pub flags: Flags,
    /// cached glyph and cell for rendering.
    pub data: Option<Vec<(RenderableCell, Glyph)>>,
}

impl Hash for TextRun {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.span.1 - self.span.0).hash(state);
        self.content.hash(state);
        self.flags.hash(state);
    }
}

impl PartialEq for TextRun {
    fn eq(&self, other: &Self) -> bool {
        (self.span.1 - self.span.0) == (other.span.1 - other.span.0)
            && self.content == other.content
            && self.flags == other.flags
    }
}

impl Eq for TextRun {}

impl TextRun {
    #[inline]
    pub fn update_to_data(&mut self, other: &mut Self) {
        if let Some(data) = &mut other.data {
            if other.line != self.line {
                other.line = self.line;
                for (cell, _) in data.iter_mut() {
                    cell.line = self.line;
                }
            }
            let start = other.span.0;
            if start != self.span.0 {
                other.span = self.span;
                for (cell, _) in data.iter_mut() {
                    cell.column = self.span.0 + cell.column - start;
                }
            }
            if other.fg != self.fg {
                other.fg = self.fg;
                for (cell, _) in data.iter_mut() {
                    cell.fg = self.fg;
                }
            }
            if other.bg != self.bg {
                other.bg = self.bg;
                for (cell, _) in data.iter_mut() {
                    cell.bg = self.bg;
                }
            }
            if other.bg_alpha.to_bits() != self.bg_alpha.to_bits() {
                other.bg_alpha = self.bg_alpha;
                for (cell, _) in data.iter_mut() {
                    cell.bg_alpha = self.bg_alpha;
                }
            }
        }
    }

    /// Returns dummy RenderableCell containing no content with positioning and color information
    /// from this TextRun.
    #[inline]
    fn dummy_cell_at(&self, col: Column) -> RenderableCell {
        RenderableCell {
            line: self.line,
            column: col,
            character: ' ',
            zerowidth: None,
            fg: self.fg,
            bg: self.bg,
            bg_alpha: self.bg_alpha,
            flags: self.flags,
            is_match: false,
        }
    }

    /// First cell in the TextRun
    #[inline]
    pub fn start_cell(&self) -> RenderableCell {
        self.dummy_cell_at(self.span.0)
    }

    /// First point covered by this TextRun
    #[inline]
    pub fn start_point(&self) -> Point {
        Point { line: self.line, col: self.span.0 }
    }

    /// End point covered by this TextRun
    #[inline]
    pub fn end_point(&self) -> Point {
        Point { line: self.line, col: self.span.1 }
    }
}
