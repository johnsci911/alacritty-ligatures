use std::cmp::max;
use std::iter;
use std::iter::Peekable;
use std::mem;
use std::ops::RangeInclusive;

use crate::ansi::{Color, CursorShape, NamedColor};
use crate::config::Config;
use crate::grid::{Dimensions, DisplayIter, Grid, GridCell, Indexed};
use crate::index::{Column, Direction, Line, Point};
use crate::selection::SelectionRange;
use crate::term::cell::{Cell, Flags};
use crate::term::color::{self, CellRgb, Rgb, DIM_FACTOR};
use crate::term::search::RegexIter;
use crate::term::LatestCol;
use crate::term::{Term, TermMode};
use crate::text_run::{RunStart, TextRun, UIFlags};

/// Minimum contrast between a fixed cursor color and the cell's background.
pub const MIN_CURSOR_CONTRAST: f64 = 1.5;

/// Maximum number of linewraps followed outside of the viewport during search highlighting.
const MAX_SEARCH_LINES: usize = 100;

/// Renderable terminal content.
///
/// This provides the terminal cursor and an iterator over all non-empty cells.
pub struct RenderableContent<'a, T, C> {
    term: &'a Term<T>,
    config: &'a Config<C>,
    display_iter: DisplayIter<'a, Cell>,
    selection: Option<SelectionRange<Line>>,
    search: RenderableSearch<'a>,
    cursor: Option<RenderableCursor>,
    cursor_shape: CursorShape,
    cursor_point: Point,
    grid: &'a Grid<Cell>,
    run_start: Option<RunStart>,
    latest_col: Option<LatestCol>,
    buffer_text: String,
    buffer_extra: Vec<Option<Vec<char>>>,
    viewport_match: Option<RangeInclusive<Point>>,
}

impl<'a, T, C> RenderableContent<'a, T, C> {
    pub fn new(
        term: &'a Term<T>,
        config: &'a Config<C>,
        show_cursor: bool,
        viewport_match: Option<RangeInclusive<Point>>,
    ) -> Self {
        // Cursor position.
        let vi_mode = term.mode.contains(TermMode::VI);
        let mut cursor_point = if vi_mode {
            term.vi_mode_cursor.point
        } else {
            let mut point = term.grid.cursor.point;
            point.line += term.grid.display_offset();
            point
        };

        // Cursor shape.
        let cursor_shape = if !show_cursor
            || (!term.mode.contains(TermMode::SHOW_CURSOR) && !vi_mode)
            || cursor_point.line >= term.screen_lines()
        {
            cursor_point.line = Line(0);
            CursorShape::Hidden
        } else if !term.is_focused && config.cursor.unfocused_hollow {
            CursorShape::HollowBlock
        } else {
            let cursor_style = term.cursor_style.unwrap_or(term.default_cursor_style);

            if vi_mode {
                term.vi_mode_cursor_style.unwrap_or(cursor_style).shape
            } else {
                cursor_style.shape
            }
        };

        Self {
            display_iter: term.grid.display_iter(),
            selection: term.visible_selection(),
            search: RenderableSearch::new(term),
            cursor: None,
            cursor_shape,
            cursor_point,
            config,
            term,
            grid: &term.grid,
            run_start: None,
            latest_col: None,
            buffer_text: String::new(),
            buffer_extra: Vec::new(),
            viewport_match,
        }
    }

    /// Get the terminal cursor.
    pub fn cursor(mut self) -> Option<RenderableCursor> {
        // Drain the iterator to make sure the cursor is created.
        while self.next().is_some() && self.cursor.is_none() {}

        self.cursor
    }

    /// Assemble the information required to render the terminal cursor.
    ///
    /// This will return `None` when there is no cursor visible.
    fn renderable_cursor(
        &mut self,
        cell: &Indexed<&Cell>,
        uiflags: UIFlags,
    ) -> Option<RenderableCursor> {
        if self.cursor_shape == CursorShape::Hidden {
            return None;
        }

        let start = RunStart {
            line: cell.line,
            column: cell.column,
            fg: cell.fg,
            bg: cell.bg,
            uiflags,
            flags: cell.flags,
        };
        let (fg, bg, _) = RenderableCell::color_to_rgb(self.config, &self.term.colors, &start);

        // Expand across wide cell when inside wide char or spacer.
        let is_wide = if cell.flags.contains(Flags::WIDE_CHAR_SPACER) {
            self.cursor_point.col -= 1;
            true
        } else {
            cell.flags.contains(Flags::WIDE_CHAR)
        };

        // Cursor colors.
        let color = if self.term.mode.contains(TermMode::VI) {
            self.config.colors.vi_mode_cursor
        } else {
            self.config.colors.cursor
        };
        let mut cursor_color = if self.term.color_modified[NamedColor::Cursor as usize] {
            CellRgb::Rgb(self.term.colors[NamedColor::Cursor])
        } else {
            color.background
        };
        let mut text_color = color.foreground;

        // Invert the cursor if it has a fixed background close to the cell's background.
        if matches!(
            cursor_color,
            CellRgb::Rgb(color) if color.contrast(bg) < MIN_CURSOR_CONTRAST
        ) {
            cursor_color = CellRgb::CellForeground;
            text_color = CellRgb::CellBackground;
        }

        // Convert from cell colors to RGB.
        let text_color = text_color.color(fg, bg);
        let cursor_color = cursor_color.color(fg, bg);

        Some(RenderableCursor {
            point: self.cursor_point,
            shape: self.cursor_shape,
            cursor_color,
            text_color,
            is_wide,
        })
    }

    /// Get the next textrun as the cell below the cursor.
    fn cursor_cell(&mut self, cell: Indexed<&Cell>, uiflags: &mut UIFlags) -> TextRun {
        // Handle cell below cursor.
        let mut run_start = RunStart {
            line: cell.line,
            column: cell.column,
            fg: cell.fg,
            bg: cell.bg,
            uiflags: *uiflags,
            flags: cell.flags,
        };
        if self.cursor_shape == CursorShape::Block {
            run_start.uiflags |= UIFlags::BLOCK | UIFlags::CURSOR;
            if let Some(cursor) = self.cursor {
                run_start.fg = Color::Spec(cursor.text_color);
                run_start.bg = Color::Spec(cursor.cursor_color);
            }
        }

        let width = if cell.flags.contains(Flags::WIDE_CHAR) { 2 } else { 1 };
        let latest_col = (cell.column, width);
        let buffer = (cell.c.to_string(), vec![cell.zerowidth().map(|z| z.to_vec())]);
        self.build_text_run(run_start, latest_col, buffer)
    }

    /// Check if current run ends at incoming Cell.
    /// Run will not include incoming Cell if it ends.
    fn is_end_of_run(&self, cell: &Indexed<&Cell>, uiflags: UIFlags) -> bool {
        // does cell not belong to run
        if let Some(run_start) = &self.run_start {
            if !run_start.belongs_to_text_run(cell, uiflags) {
                return true;
            }
        }
        // is column not adjacent
        if let Some((col, width)) = self.latest_col {
            col + width != cell.column && cell.column + width != col
        } else {
            false
        }
    }

    /// Add content of cell to pending TextRun buffer
    fn buffer_content(&mut self, cell: &Indexed<&Cell>) {
        // Render tab as spaces in case the font doesn't support it.
        if cell.c == '\t' {
            self.buffer_text.push(' ');
        } else {
            self.buffer_text.push(cell.c);
        }
        self.buffer_extra.push(cell.zerowidth().map(|z| z.to_vec()));
    }

    /// Empty out pending buffer producing owned collections that can be moved into a TextRun.
    fn drain_buffer(&mut self) -> (String, Vec<Option<Vec<char>>>) {
        (self.buffer_text.split_off(0), self.buffer_extra.split_off(0))
    }

    /// Start a new run by setting latest_col, run_start and buffering content of cell.
    /// Returns the previous runs run_start and latest_col data if available.
    fn start_run(
        &mut self,
        cell: Indexed<&Cell>,
        uiflags: UIFlags,
    ) -> (Option<RunStart>, Option<LatestCol>) {
        self.buffer_content(&cell);
        let width = if cell.flags.contains(Flags::WIDE_CHAR) { 2 } else { 1 };
        let latest = self.latest_col.replace((cell.column, width));
        let start = self.run_start.replace(RunStart {
            line: cell.line,
            column: cell.column,
            fg: cell.fg,
            bg: cell.bg,
            uiflags,
            flags: cell.flags,
        });
        (start, latest)
    }

    /// Create a run of chars from the current state of `TextRunIter`.
    /// This is a destructive operation, the iterator will be in a new run state after it's
    /// completion.
    fn produce_char_run(&mut self, cell: Indexed<&Cell>, uiflags: UIFlags) -> Option<TextRun> {
        let prev_buffer = self.drain_buffer();
        let (start_opt, latest_col_opt) = self.start_run(cell, uiflags);
        let start = start_opt?;
        let latest_col = latest_col_opt?;
        Some(self.build_text_run(start, latest_col, prev_buffer))
    }

    /// Build a TextRun instance from passed state of TextRunIter
    fn build_text_run(
        &self,
        start: RunStart,
        (latest, width): LatestCol,
        buffer: (String, Vec<Option<Vec<char>>>),
    ) -> TextRun {
        let end_column = latest + width - 1;
        let (fg, bg, bg_alpha) =
            RenderableCell::color_to_rgb(self.config, &self.term.colors, &start);
        TextRun {
            line: start.line,
            span: (start.column, end_column),
            content: (buffer.0, buffer.1),
            fg,
            bg,
            bg_alpha,
            flags: start.flags,
            data: None,
        }
    }

    /// Check selection state of a cell.
    #[inline]
    fn is_selected(&self, point: Point) -> bool {
        self.selection.map_or(false, |selection| {
            let grid = self.term.grid();
            selection.contains_cell(grid, point, self.cursor_point, self.cursor_shape)
        })
    }
}

impl<'a, T, C> Iterator for RenderableContent<'a, T, C> {
    type Item = TextRun;

    /// Gets the next renderable cell.
    ///
    /// Skips empty (background) cells and applies any flags to the cell state
    /// (eg. invert fg and bg colors).
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let mut output = None;
        loop {
            if self.cursor_point == self.display_iter.point() {
                if !self.buffer_text.is_empty() || !self.buffer_extra.is_empty() {
                    let start = self.run_start.take()?;
                    let latest_col = self.latest_col.take()?;
                    // Save leftover buffer and empty it
                    let prev_buffered = self.drain_buffer();
                    return Some(self.build_text_run(start, latest_col, prev_buffered));
                }
                // Handle cell at cursor position.
                if let Some(cell) = self.display_iter.next() {
                    let selected = self.is_selected(self.cursor_point);
                    let buffer_point = self.grid.visible_to_buffer(self.cursor_point);
                    let search_matched = self.search.advance(buffer_point);
                    let mut uiflags = UIFlags::empty();
                    if selected {
                        uiflags |= UIFlags::SELECTED;
                    }
                    if search_matched {
                        uiflags |= UIFlags::SEARCH_MATCHED;
                        if let Some(viewport_match) = &self.viewport_match {
                            if viewport_match.contains(&self.cursor_point) {
                                uiflags |= UIFlags::FOCUSED_MATCH;
                            }
                        }
                    }
                    // Store the cursor which should be rendered.
                    self.cursor = self.renderable_cursor(&cell, uiflags);
                    return Some(self.cursor_cell(cell, &mut uiflags));
                } else {
                    break;
                }
            } else if let Some(cell) = self.display_iter.next() {
                let point = Point::new(cell.line, cell.column);
                let selected = self.is_selected(point);
                let search_matched = self.search.advance(self.grid.visible_to_buffer(point));
                let has_wide_char_spacer = cell.flags.contains(Flags::WIDE_CHAR_SPACER);
                // skip empty cells and wide char spacers.
                if (cell.is_empty() || has_wide_char_spacer) && !selected && !search_matched {
                    continue;
                }
                let mut uiflags = UIFlags::empty();
                if selected {
                    uiflags |= UIFlags::SELECTED;
                }
                if search_matched {
                    uiflags |= UIFlags::SEARCH_MATCHED;
                    if let Some(viewport_match) = &self.viewport_match {
                        if viewport_match.contains(&point) {
                            uiflags |= UIFlags::FOCUSED_MATCH;
                        }
                    }
                }
                if self.latest_col.is_none() || self.run_start.is_none() {
                    // Initial state, this should only be hit on the first next() call of
                    // iterator
                    self.run_start = Some(RunStart {
                        line: cell.line,
                        column: cell.column,
                        fg: cell.fg,
                        bg: cell.bg,
                        uiflags,
                        flags: cell.flags,
                    });
                } else if self.is_end_of_run(&cell, uiflags) {
                    // If we find a run break, return what we have so far and start a new run.
                    output = self.produce_char_run(cell, uiflags);
                    break;
                }
                // Build up buffer and track the latest column we've seen
                let width = if cell.flags.contains(Flags::WIDE_CHAR) { 2 } else { 1 };
                self.latest_col = Some((cell.column, width));
                self.buffer_content(&cell);
            } else {
                break;
            }
        }
        // Check for any remaining buffered content and return it as a text run.
        // This is a destructive operation, it will return None after it executes once.
        output.or_else(|| {
            if !self.buffer_text.is_empty() || !self.buffer_extra.is_empty() {
                let start = self.run_start.take()?;
                let latest_col = self.latest_col.take()?;
                // Save leftover buffer and empty it
                let prev_buffered = self.drain_buffer();
                Some(self.build_text_run(start, latest_col, prev_buffered))
            } else {
                None
            }
        })
    }
}

/// Cell ready for rendering.
#[derive(Clone, Debug)]
pub struct RenderableCell {
    pub character: char,
    pub zerowidth: Option<Vec<char>>,
    pub line: Line,
    pub column: Column,
    pub fg: Rgb,
    pub bg: Rgb,
    pub bg_alpha: f32,
    pub flags: Flags,
    pub is_match: bool,
}

impl RenderableCell {
    #[inline]
    fn color_to_rgb<C>(
        config: &Config<C>,
        colors: &color::List,
        cell: &RunStart,
    ) -> (Rgb, Rgb, f32) {
        // Lookup RGB values.
        let mut fg_rgb = Self::compute_fg_rgb(config, colors, cell.fg, cell.flags);
        let mut bg_rgb = Self::compute_bg_rgb(colors, cell.bg);
        let mut bg_alpha = if cell.flags.contains(Flags::INVERSE) {
            mem::swap(&mut fg_rgb, &mut bg_rgb);
            1.0
        } else {
            Self::compute_bg_alpha(cell.bg)
        };
        if cell.uiflags.contains(UIFlags::SELECTED) {
            let config_bg = config.colors.selection.background;
            let selected_fg = config.colors.selection.foreground.color(fg_rgb, bg_rgb);
            bg_rgb = config_bg.color(fg_rgb, bg_rgb);
            fg_rgb = selected_fg;
            if fg_rgb == bg_rgb && !cell.flags.contains(Flags::HIDDEN) {
                // Reveal inversed text when fg/bg is the same.
                fg_rgb = colors[NamedColor::Background];
                bg_rgb = colors[NamedColor::Foreground];
                bg_alpha = 1.0;
            } else if config_bg != CellRgb::CellBackground {
                bg_alpha = 1.0;
            }
        } else if cell.uiflags.contains(UIFlags::SEARCH_MATCHED) {
            // Highlight the cell if it is part of a search match.
            let config_bg = config.colors.search.matches.background;
            let matched_fg = config.colors.search.matches.foreground.color(fg_rgb, bg_rgb);
            bg_rgb = config_bg.color(fg_rgb, bg_rgb);
            fg_rgb = matched_fg;

            if config_bg != CellRgb::CellBackground {
                bg_alpha = 1.0;
            }
        }
        if cell.uiflags.contains(UIFlags::FOCUSED_MATCH) {
            let colors = config.colors.search.focused_match;
            let matched_fg = colors.foreground.color(fg_rgb, bg_rgb);
            bg_rgb = colors.background.color(fg_rgb, bg_rgb);
            fg_rgb = matched_fg;
            bg_alpha = 1.0;
        }

        (fg_rgb, bg_rgb, bg_alpha)
    }

    /// Position of the cell.
    pub fn point(&self) -> Point {
        Point::new(self.line, self.column)
    }

    /// Check if cell contains any renderable content.
    #[allow(dead_code)]
    fn is_empty(&self) -> bool {
        self.bg_alpha == 0.
            && !self.flags.intersects(Flags::UNDERLINE | Flags::STRIKEOUT | Flags::DOUBLE_UNDERLINE)
            && self.character == ' '
            && self.zerowidth.is_none()
    }

    /// Get the RGB color from a cell's foreground color.
    #[inline]
    fn compute_fg_rgb<C>(config: &Config<C>, colors: &color::List, fg: Color, flags: Flags) -> Rgb {
        match fg {
            Color::Spec(rgb) => match flags & Flags::DIM {
                Flags::DIM => rgb * DIM_FACTOR,
                _ => rgb,
            },
            Color::Named(ansi) => {
                match (config.draw_bold_text_with_bright_colors, flags & Flags::DIM_BOLD) {
                    // If no bright foreground is set, treat it like the BOLD flag doesn't exist.
                    (_, Flags::DIM_BOLD)
                        if ansi == NamedColor::Foreground
                            && config.colors.primary.bright_foreground.is_none() =>
                    {
                        colors[NamedColor::DimForeground]
                    },
                    // Draw bold text in bright colors *and* contains bold flag.
                    (true, Flags::BOLD) => colors[ansi.to_bright()],
                    // Cell is marked as dim and not bold.
                    (_, Flags::DIM) | (false, Flags::DIM_BOLD) => colors[ansi.to_dim()],
                    // None of the above, keep original color..
                    _ => colors[ansi],
                }
            },
            Color::Indexed(idx) => {
                let idx = match (
                    config.draw_bold_text_with_bright_colors,
                    flags & Flags::DIM_BOLD,
                    idx,
                ) {
                    (true, Flags::BOLD, 0..=7) => idx as usize + 8,
                    (false, Flags::DIM, 8..=15) => idx as usize - 8,
                    (false, Flags::DIM, 0..=7) => NamedColor::DimBlack as usize + idx as usize,
                    _ => idx as usize,
                };

                colors[idx]
            },
        }
    }

    /// Get the RGB color from a cell's background color.
    #[inline]
    fn compute_bg_rgb(colors: &color::List, bg: Color) -> Rgb {
        match bg {
            Color::Spec(rgb) => rgb,
            Color::Named(ansi) => colors[ansi],
            Color::Indexed(idx) => colors[idx],
        }
    }

    /// Compute background alpha based on cell's original color.
    ///
    /// Since an RGB color matching the background should not be transparent, this is computed
    /// using the named input color, rather than checking the RGB of the background after its color
    /// is computed.
    #[inline]
    fn compute_bg_alpha(bg: Color) -> f32 {
        if bg == Color::Named(NamedColor::Background) {
            0.
        } else {
            1.
        }
    }
}

/// Cursor storing all information relevant for rendering.
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct RenderableCursor {
    shape: CursorShape,
    cursor_color: Rgb,
    text_color: Rgb,
    is_wide: bool,
    point: Point,
}

impl RenderableCursor {
    pub fn color(&self) -> Rgb {
        self.cursor_color
    }

    pub fn shape(&self) -> CursorShape {
        self.shape
    }

    pub fn is_wide(&self) -> bool {
        self.is_wide
    }

    pub fn point(&self) -> Point {
        self.point
    }
}

type MatchIter<'a> = Box<dyn Iterator<Item = RangeInclusive<Point<usize>>> + 'a>;

/// Regex search highlight tracking.
struct RenderableSearch<'a> {
    iter: Peekable<MatchIter<'a>>,
}

impl<'a> RenderableSearch<'a> {
    /// Create a new renderable search iterator.
    fn new<T>(term: &'a Term<T>) -> Self {
        // Avoid constructing search if there is none.
        if term.regex_search.is_none() {
            let iter: MatchIter<'a> = Box::new(iter::empty());
            return Self { iter: iter.peekable() };
        }

        let viewport_end = term.grid().display_offset();
        let viewport_start = viewport_end + term.screen_lines().0 - 1;

        // Compute start of the first and end of the last line.
        let start_point = Point::new(viewport_start, Column(0));
        let mut start = term.line_search_left(start_point);
        let end_point = Point::new(viewport_end, term.cols() - 1);
        let mut end = term.line_search_right(end_point);

        // Set upper bound on search before/after the viewport to prevent excessive blocking.
        if start.line > viewport_start + MAX_SEARCH_LINES {
            if start.line == 0 {
                // Do not highlight anything if this line is the last.
                let iter: MatchIter<'a> = Box::new(iter::empty());
                return Self { iter: iter.peekable() };
            } else {
                // Start at next line if this one is too long.
                start.line -= 1;
            }
        }
        end.line = max(end.line, viewport_end.saturating_sub(MAX_SEARCH_LINES));

        // Create an iterater for the current regex search for all visible matches.
        let iter: MatchIter<'a> = Box::new(
            RegexIter::new(start, end, Direction::Right, &term)
                .skip_while(move |rm| rm.end().line > viewport_start)
                .take_while(move |rm| rm.start().line >= viewport_end),
        );

        Self { iter: iter.peekable() }
    }

    /// Advance the search tracker to the next point.
    ///
    /// This will return `true` if the point passed is part of a search match.
    fn advance(&mut self, point: Point<usize>) -> bool {
        while let Some(regex_match) = &self.iter.peek() {
            if regex_match.start() > &point {
                break;
            } else if regex_match.end() < &point {
                let _ = self.iter.next();
            } else {
                return true;
            }
        }
        false
    }
}
