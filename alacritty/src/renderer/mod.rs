use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::hash::BuildHasherDefault;
use std::io;
use std::mem::size_of;
use std::ptr;

use bitflags::bitflags;
use crossfont::RasterizeExt;
use crossfont::{
    self, BitmapBuffer, Error as RasterizerError, FontDesc, FontKey, GlyphKey, KeyType, Rasterize,
    RasterizedGlyph, Rasterizer, Size, Slant, Style, Weight, PLACEHOLDER_GLYPH,
};
use log::{error, info};
use rustc_hash::FxHasher;
use unicode_width::UnicodeWidthChar;

use alacritty_terminal::index::Point;
use alacritty_terminal::term::cell::Flags;
use alacritty_terminal::term::color::Rgb;
use alacritty_terminal::term::render::RenderableCell;
use alacritty_terminal::term::SizeInfo;
pub use alacritty_terminal::text_run::Glyph;
use alacritty_terminal::text_run::TextRun;

use crate::config::font::{Font, FontDescription};
use crate::config::ui_config::{Delta, UIConfig};
use crate::gl;
use crate::gl::types::*;
use crate::renderer::rects::{RectRenderer, RenderRect};

pub mod rects;

// Shader source.
static TEXT_SHADER_F: &str = include_str!("../../res/text.f.glsl");
static TEXT_SHADER_V: &str = include_str!("../../res/text.v.glsl");

/// `LoadGlyph` allows for copying a rasterized glyph into graphics memory.
pub trait LoadGlyph {
    /// Load the rasterized glyph into GPU memory.
    fn load_glyph(&mut self, rasterized: &RasterizedGlyph) -> Glyph;

    /// Clear any state accumulated from previous loaded glyphs.
    ///
    /// This can, for instance, be used to reset the texture Atlas.
    fn clear(&mut self);
}

#[derive(Debug)]
pub enum Error {
    ShaderCreation(ShaderCreationError),
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::ShaderCreation(err) => err.source(),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::ShaderCreation(err) => {
                write!(f, "There was an error initializing the shaders: {}", err)
            },
        }
    }
}

impl From<ShaderCreationError> for Error {
    fn from(val: ShaderCreationError) -> Self {
        Error::ShaderCreation(val)
    }
}

/// Text drawing program.
///
/// Uniforms are prefixed with "u", and vertex attributes are prefixed with "a".
#[derive(Debug)]
pub struct TextShaderProgram {
    /// Program id.
    id: GLuint,

    /// Projection scale and offset uniform.
    u_projection: GLint,

    /// Cell dimensions (pixels).
    u_cell_dim: GLint,

    /// Background pass flag.
    ///
    /// Rendering is split into two passes; 1 for backgrounds, and one for text.
    u_background: GLint,
}

/// Naïve glyph cache.
///
/// Currently only keyed by `char`, and thus not possible to hold different
/// representations of the same code point.
pub struct GlyphCache {
    /// Cache of buffered glyphs.
    cache: HashMap<GlyphKey, Glyph, BuildHasherDefault<FxHasher>>,

    /// Rasterizer for loading new glyphs.
    rasterizer: Rasterizer,

    /// Regular font.
    font_key: FontKey,

    /// Bold font.
    bold_key: FontKey,

    /// Italic font.
    italic_key: FontKey,

    /// Bold italic font.
    bold_italic_key: FontKey,

    /// Font size.
    font_size: crossfont::Size,

    /// Glyph offset.
    glyph_offset: Delta<i8>,

    /// Font metrics.
    metrics: crossfont::Metrics,
}

struct GlyphKeyIter<I> {
    iter: I,
    chars: Vec<Option<char>>,
    font_size: crossfont::Size,
    font_key: FontKey,
}

impl<I> Iterator for GlyphKeyIter<I>
where
    I: Iterator<Item = (u32, u32)>,
{
    type Item = GlyphKey;

    #[inline]
    fn next(&mut self) -> Option<GlyphKey> {
        let (codepoint, cluster) = match self.iter.next() {
            Some(v) => v,
            None => return None,
        };

        // Codepoint of 0 indicates a missing or undefined glyph
        let id: KeyType = if codepoint == 0 {
            self.chars[cluster as usize]
                .unwrap_or_else(|| panic!("Could not find cluster {}", cluster))
                .into()
        } else {
            codepoint.into()
        };

        Some(GlyphKey { id, font_key: self.font_key, size: self.font_size })
    }
}

impl GlyphCache {
    pub fn new<L>(
        mut rasterizer: Rasterizer,
        font: &Font,
        loader: &mut L,
    ) -> Result<GlyphCache, crossfont::Error>
    where
        L: LoadGlyph,
    {
        let (regular, bold, italic, bold_italic) = Self::compute_font_keys(font, &mut rasterizer)?;

        // Need to load at least one glyph for the face before calling metrics.
        // The glyph requested here ('m' at the time of writing) has no special
        // meaning.

        rasterizer.get_glyph(GlyphKey {
            id: PLACEHOLDER_GLYPH,
            font_key: regular,
            size: font.size(),
        })?;

        let metrics = rasterizer.metrics(regular, font.size())?;

        let mut cache = Self {
            cache: HashMap::default(),
            rasterizer,
            font_size: font.size(),
            font_key: regular,
            bold_key: bold,
            italic_key: italic,
            bold_italic_key: bold_italic,
            glyph_offset: font.glyph_offset,
            metrics,
        };

        cache.load_common_glyphs(loader);

        Ok(cache)
    }

    fn load_glyphs_for_font<L: LoadGlyph>(&mut self, font: FontKey, loader: &mut L) {
        let size = self.font_size;

        // Cache all ascii characters.
        for i in 32u8..=126u8 {
            self.get(GlyphKey { font_key: font, id: KeyType::Char(i as char), size }, loader, true);
        }
    }

    /// Computes font keys for (Regular, Bold, Italic, Bold Italic).
    fn compute_font_keys(
        font: &Font,
        rasterizer: &mut Rasterizer,
    ) -> Result<(FontKey, FontKey, FontKey, FontKey), crossfont::Error> {
        let size = font.size();

        // Load regular font.
        let regular_desc = Self::make_desc(&font.normal(), Slant::Normal, Weight::Normal);

        let regular = Self::load_regular_font(rasterizer, &regular_desc, size)?;

        // Helper to load a description if it is not the `regular_desc`.
        let mut load_or_regular = |desc: FontDesc| {
            if desc == regular_desc {
                regular
            } else {
                rasterizer.load_font(&desc, size).unwrap_or(regular)
            }
        };

        // Load bold font.
        let bold_desc = Self::make_desc(&font.bold(), Slant::Normal, Weight::Bold);

        let bold = load_or_regular(bold_desc);

        // Load italic font.
        let italic_desc = Self::make_desc(&font.italic(), Slant::Italic, Weight::Normal);

        let italic = load_or_regular(italic_desc);

        // Load bold italic font.
        let bold_italic_desc = Self::make_desc(&font.bold_italic(), Slant::Italic, Weight::Bold);

        let bold_italic = load_or_regular(bold_italic_desc);

        Ok((regular, bold, italic, bold_italic))
    }

    fn load_regular_font(
        rasterizer: &mut Rasterizer,
        description: &FontDesc,
        size: Size,
    ) -> Result<FontKey, crossfont::Error> {
        match rasterizer.load_font(description, size) {
            Ok(font) => Ok(font),
            Err(err) => {
                error!("{}", err);

                let fallback_desc =
                    Self::make_desc(&Font::default().normal(), Slant::Normal, Weight::Normal);
                rasterizer.load_font(&fallback_desc, size)
            },
        }
    }

    fn make_desc(desc: &FontDescription, slant: Slant, weight: Weight) -> FontDesc {
        let style = if let Some(ref spec) = desc.style {
            Style::Specific(spec.to_owned())
        } else {
            Style::Description { slant, weight }
        };
        FontDesc::new(desc.family.clone(), style)
    }

    /// Get a glyph from the font.
    ///
    /// If the glyph has never been loaded before, it will be rasterized and inserted into the
    /// cache.
    ///
    /// # Errors
    ///
    /// This will fail when the glyph could not be rasterized. Usually this is due to the glyph
    /// not being present in any font.
    fn get<L>(&mut self, glyph_key: GlyphKey, loader: &mut L, show_missing: bool) -> Glyph
    where
        L: LoadGlyph,
    {
        // Try to load glyph from cache.
        if let Some(glyph) = self.cache.get(&glyph_key) {
            return *glyph;
        };

        // Rasterize glyph.
        let glyph = match self.rasterizer.get_glyph(glyph_key) {
            Ok(rasterized) => self.load_glyph(loader, rasterized),
            // Load fallback glyph.
            Err(RasterizerError::MissingGlyph(rasterized)) if show_missing => {
                // Use `\0` as "missing" glyph to cache it only once.
                let missing_key = GlyphKey { id: KeyType::Char('\0'), ..glyph_key };
                if let Some(glyph) = self.cache.get(&missing_key) {
                    *glyph
                } else {
                    // If no missing glyph was loaded yet, insert it as `\0`.
                    let glyph = self.load_glyph(loader, rasterized);
                    self.cache.insert(missing_key, glyph);

                    glyph
                }
            },
            Err(_) => self.load_glyph(loader, Default::default()),
        };

        // Cache rasterized glyph.
        *self.cache.entry(glyph_key).or_insert(glyph)
    }

    /// Load glyph into the atlas.
    ///
    /// This will apply all transforms defined for the glyph cache to the rasterized glyph before
    /// insertion.
    fn load_glyph<L>(&self, loader: &mut L, mut glyph: RasterizedGlyph) -> Glyph
    where
        L: LoadGlyph,
    {
        glyph.left += i32::from(self.glyph_offset.x);
        glyph.top += i32::from(self.glyph_offset.y);
        glyph.top -= self.metrics.descent as i32;

        // The metrics of zero-width characters are based on rendering
        // the character after the current cell, with the anchor at the
        // right side of the preceding character. Since we render the
        // zero-width characters inside the preceding character, the
        // anchor has been moved to the right by one cell.
        match glyph.character {
            KeyType::Char(c) if c.width() == Some(0) => {
                glyph.left += self.metrics.average_advance as i32;
            },
            // If it's glyphindex, we assume its width is not zero.
            // might be wrong, and need some provement.
            _ => {},
        };

        // Add glyph to cache.
        loader.load_glyph(&glyph)
    }

    /// Clear currently cached data in both GL and the registry.
    pub fn clear_glyph_cache<L: LoadGlyph>(&mut self, loader: &mut L) {
        loader.clear();
        self.cache = HashMap::default();

        self.load_common_glyphs(loader);
    }

    pub fn update_font_size<L: LoadGlyph>(
        &mut self,
        font: &Font,
        dpr: f64,
        loader: &mut L,
    ) -> Result<(), crossfont::Error> {
        // Update dpi scaling.
        self.rasterizer.update_dpr(dpr as f32);

        // Recompute font keys.
        let (regular, bold, italic, bold_italic) =
            Self::compute_font_keys(font, &mut self.rasterizer)?;

        self.rasterizer.get_glyph(GlyphKey {
            id: PLACEHOLDER_GLYPH,
            font_key: regular,
            size: font.size(),
        })?;
        let metrics = self.rasterizer.metrics(regular, font.size())?;

        info!("Font size changed to {:?} with DPR of {}", font.size(), dpr);

        self.font_size = font.size();
        self.font_key = regular;
        self.bold_key = bold;
        self.italic_key = italic;
        self.bold_italic_key = bold_italic;
        self.metrics = metrics;

        self.clear_glyph_cache(loader);

        Ok(())
    }

    pub fn font_metrics(&self) -> crossfont::Metrics {
        self.metrics
    }

    /// Prefetch glyphs that are almost guaranteed to be loaded anyways.
    fn load_common_glyphs<L: LoadGlyph>(&mut self, loader: &mut L) {
        self.load_glyphs_for_font(self.font_key, loader);
        self.load_glyphs_for_font(self.bold_italic_key, loader);
        self.load_glyphs_for_font(self.italic_key, loader);
        self.load_glyphs_for_font(self.bold_italic_key, loader);
    }

    /// Calculate font metrics without access to a glyph cache.
    pub fn static_metrics(font: Font, dpr: f64) -> Result<crossfont::Metrics, crossfont::Error> {
        let mut rasterizer = Rasterizer::new(dpr as f32, font.use_thin_strokes, font.ligatures)?;
        let regular_desc = GlyphCache::make_desc(&font.normal(), Slant::Normal, Weight::Normal);
        let regular = Self::load_regular_font(&mut rasterizer, &regular_desc, font.size())?;
        rasterizer.get_glyph(GlyphKey {
            font_key: regular,
            id: PLACEHOLDER_GLYPH,
            size: font.size(),
        })?;

        rasterizer.metrics(regular, font.size())
    }
}

// NOTE: These flags must be in sync with their usage in the text.*.glsl shaders.
bitflags! {
    #[repr(C)]
    struct RenderingGlyphFlags: u8 {
        const WIDE_CHAR = 0b0000_0001;
        const COLORED   = 0b0000_0010;
    }
}

#[derive(Debug)]
#[repr(C)]
struct InstanceData {
    // Coords.
    col: u16,
    row: u16,

    // Glyph offset.
    left: i16,
    top: i16,

    // Glyph size.
    width: i16,
    height: i16,

    // UV offset.
    uv_left: f32,
    uv_bot: f32,

    // uv scale.
    uv_width: f32,
    uv_height: f32,

    // Color.
    r: u8,
    g: u8,
    b: u8,

    // Cell flags like multicolor or fullwidth character.
    cell_flags: RenderingGlyphFlags,

    // Background color.
    bg_r: u8,
    bg_g: u8,
    bg_b: u8,
    bg_a: u8,
}

#[derive(Debug)]
pub struct QuadRenderer {
    program: TextShaderProgram,
    vao: GLuint,
    ebo: GLuint,
    vbo_instance: GLuint,
    atlas: Vec<Atlas>,
    current_atlas: usize,
    active_tex: GLuint,
    batch: Batch,

    rect_renderer: RectRenderer,
}

#[derive(Debug)]
pub struct RenderApi<'a> {
    active_tex: &'a mut GLuint,
    batch: &'a mut Batch,
    atlas: &'a mut Vec<Atlas>,
    current_atlas: &'a mut usize,
    program: &'a mut TextShaderProgram,
    config: &'a UIConfig,
}

#[derive(Debug)]
pub struct LoaderApi<'a> {
    active_tex: &'a mut GLuint,
    atlas: &'a mut Vec<Atlas>,
    current_atlas: &'a mut usize,
}

#[derive(Debug, Default)]
pub struct Batch {
    tex: GLuint,
    instances: Vec<InstanceData>,
}

impl Batch {
    #[inline]
    pub fn new() -> Self {
        Self { tex: 0, instances: Vec::with_capacity(BATCH_MAX) }
    }

    pub fn add_item(&mut self, cell: &RenderableCell, glyph: &Glyph) {
        if self.is_empty() {
            self.tex = glyph.tex_id;
        }

        let mut cell_flags = RenderingGlyphFlags::empty();
        cell_flags.set(RenderingGlyphFlags::COLORED, glyph.multicolor);
        cell_flags.set(RenderingGlyphFlags::WIDE_CHAR, cell.flags.contains(Flags::WIDE_CHAR));

        self.instances.push(InstanceData {
            col: cell.column.0 as u16,
            row: cell.line.0 as u16,

            top: glyph.top,
            left: glyph.left,
            width: glyph.width,
            height: glyph.height,

            uv_bot: glyph.uv_bot,
            uv_left: glyph.uv_left,
            uv_width: glyph.uv_width,
            uv_height: glyph.uv_height,

            r: cell.fg.r,
            g: cell.fg.g,
            b: cell.fg.b,
            cell_flags,

            bg_r: cell.bg.r,
            bg_g: cell.bg.g,
            bg_b: cell.bg.b,
            bg_a: (cell.bg_alpha * 255.0) as u8,
        });
    }

    #[inline]
    pub fn full(&self) -> bool {
        self.capacity() == self.len()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.instances.len()
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        BATCH_MAX
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.instances.is_empty()
    }

    #[inline]
    pub fn size(&self) -> usize {
        self.len() * size_of::<InstanceData>()
    }

    pub fn clear(&mut self) {
        self.tex = 0;
        self.instances.clear();
    }
}

/// Maximum items to be drawn in a batch.
const BATCH_MAX: usize = 0x1_0000;
const ATLAS_SIZE: i32 = 1024;

impl QuadRenderer {
    pub fn new() -> Result<QuadRenderer, Error> {
        let program = TextShaderProgram::new()?;

        let mut vao: GLuint = 0;
        let mut ebo: GLuint = 0;

        let mut vbo_instance: GLuint = 0;

        unsafe {
            gl::Enable(gl::BLEND);
            gl::BlendFunc(gl::SRC1_COLOR, gl::ONE_MINUS_SRC1_COLOR);
            gl::Enable(gl::MULTISAMPLE);

            // Disable depth mask, as the renderer never uses depth tests.
            gl::DepthMask(gl::FALSE);

            gl::GenVertexArrays(1, &mut vao);
            gl::GenBuffers(1, &mut ebo);
            gl::GenBuffers(1, &mut vbo_instance);
            gl::BindVertexArray(vao);

            // ---------------------
            // Set up element buffer
            // ---------------------
            let indices: [u32; 6] = [0, 1, 3, 1, 2, 3];

            gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, ebo);
            gl::BufferData(
                gl::ELEMENT_ARRAY_BUFFER,
                (6 * size_of::<u32>()) as isize,
                indices.as_ptr() as *const _,
                gl::STATIC_DRAW,
            );

            // ----------------------------
            // Setup vertex instance buffer
            // ----------------------------
            gl::BindBuffer(gl::ARRAY_BUFFER, vbo_instance);
            gl::BufferData(
                gl::ARRAY_BUFFER,
                (BATCH_MAX * size_of::<InstanceData>()) as isize,
                ptr::null(),
                gl::STREAM_DRAW,
            );

            let mut index = 0;
            let mut size = 0;

            macro_rules! add_attr {
                ($count:expr, $gl_type:expr, $type:ty) => {
                    gl::VertexAttribPointer(
                        index,
                        $count,
                        $gl_type,
                        gl::FALSE,
                        size_of::<InstanceData>() as i32,
                        size as *const _,
                    );
                    gl::EnableVertexAttribArray(index);
                    gl::VertexAttribDivisor(index, 1);

                    #[allow(unused_assignments)]
                    {
                        size += $count * size_of::<$type>();
                        index += 1;
                    }
                };
            }

            // Coords.
            add_attr!(2, gl::UNSIGNED_SHORT, u16);

            // Glyph offset and size.
            add_attr!(4, gl::SHORT, i16);

            // UV offset.
            add_attr!(4, gl::FLOAT, f32);

            // Color and cell flags.
            //
            // These are packed together because of an OpenGL driver issue on macOS, which caused a
            // `vec3(u8)` text color and a `u8` cell flags to increase the rendering time by a
            // huge margin.
            add_attr!(4, gl::UNSIGNED_BYTE, u8);

            // Background color.
            add_attr!(4, gl::UNSIGNED_BYTE, u8);

            // Cleanup.
            gl::BindVertexArray(0);
            gl::BindBuffer(gl::ARRAY_BUFFER, 0);
            gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, 0);
        }

        let mut renderer = Self {
            program,
            rect_renderer: RectRenderer::new()?,
            vao,
            ebo,
            vbo_instance,
            atlas: Vec::new(),
            current_atlas: 0,
            active_tex: 0,
            batch: Batch::new(),
        };

        let atlas = Atlas::new(ATLAS_SIZE);
        renderer.atlas.push(atlas);

        Ok(renderer)
    }

    /// Draw all rectangles simultaneously to prevent excessive program swaps.
    pub fn draw_rects(&mut self, size_info: &SizeInfo, rects: Vec<RenderRect>) {
        if rects.is_empty() {
            return;
        }

        // Prepare rect rendering state.
        unsafe {
            // Remove padding from viewport.
            gl::Viewport(0, 0, size_info.width() as i32, size_info.height() as i32);
            gl::BlendFuncSeparate(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA, gl::SRC_ALPHA, gl::ONE);
        }

        self.rect_renderer.draw(size_info, rects);

        // Activate regular state again.
        unsafe {
            // Reset blending strategy.
            gl::BlendFunc(gl::SRC1_COLOR, gl::ONE_MINUS_SRC1_COLOR);

            // Restore viewport with padding.
            let padding_x = size_info.padding_x() as i32;
            let padding_y = size_info.padding_y() as i32;
            let width = size_info.width() as i32;
            let height = size_info.height() as i32;
            gl::Viewport(padding_x, padding_y, width - 2 * padding_x, height - 2 * padding_y);
        }
    }

    pub fn with_api<F, T>(&mut self, config: &UIConfig, props: &SizeInfo, func: F) -> T
    where
        F: FnOnce(RenderApi<'_>) -> T,
    {
        unsafe {
            gl::UseProgram(self.program.id);
            self.program.set_term_uniforms(props);

            gl::BindVertexArray(self.vao);
            gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, self.ebo);
            gl::BindBuffer(gl::ARRAY_BUFFER, self.vbo_instance);
            gl::ActiveTexture(gl::TEXTURE0);
        }

        let res = func(RenderApi {
            active_tex: &mut self.active_tex,
            batch: &mut self.batch,
            atlas: &mut self.atlas,
            current_atlas: &mut self.current_atlas,
            program: &mut self.program,
            config,
        });

        unsafe {
            gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, 0);
            gl::BindBuffer(gl::ARRAY_BUFFER, 0);
            gl::BindVertexArray(0);

            gl::UseProgram(0);
        }

        res
    }

    pub fn with_loader<F, T>(&mut self, func: F) -> T
    where
        F: FnOnce(LoaderApi<'_>) -> T,
    {
        unsafe {
            gl::ActiveTexture(gl::TEXTURE0);
        }

        func(LoaderApi {
            active_tex: &mut self.active_tex,
            atlas: &mut self.atlas,
            current_atlas: &mut self.current_atlas,
        })
    }

    pub fn resize(&mut self, size: &SizeInfo) {
        // Viewport.
        unsafe {
            gl::Viewport(
                size.padding_x() as i32,
                size.padding_y() as i32,
                size.width() as i32 - 2 * size.padding_x() as i32,
                size.height() as i32 - 2 * size.padding_y() as i32,
            );

            // Update projection.
            gl::UseProgram(self.program.id);
            self.program.update_projection(
                size.width(),
                size.height(),
                size.padding_x(),
                size.padding_y(),
            );
            gl::UseProgram(0);
        }
    }
}

impl<'a> RenderApi<'a> {
    pub fn clear(&self, color: Rgb) {
        unsafe {
            let alpha = self.config.background_opacity();
            gl::ClearColor(
                (f32::from(color.r) / 255.0).min(1.0) * alpha,
                (f32::from(color.g) / 255.0).min(1.0) * alpha,
                (f32::from(color.b) / 255.0).min(1.0) * alpha,
                alpha,
            );
            gl::Clear(gl::COLOR_BUFFER_BIT);
        }
    }

    #[cfg(not(any(target_os = "macos", windows)))]
    pub fn finish(&self) {
        unsafe {
            gl::Finish();
        }
    }

    fn render_batch(&mut self) {
        unsafe {
            gl::BufferSubData(
                gl::ARRAY_BUFFER,
                0,
                self.batch.size() as isize,
                self.batch.instances.as_ptr() as *const _,
            );
        }

        // Bind texture if necessary.
        if *self.active_tex != self.batch.tex {
            unsafe {
                gl::BindTexture(gl::TEXTURE_2D, self.batch.tex);
            }
            *self.active_tex = self.batch.tex;
        }

        unsafe {
            self.program.set_background_pass(true);
            gl::DrawElementsInstanced(
                gl::TRIANGLES,
                6,
                gl::UNSIGNED_INT,
                ptr::null(),
                self.batch.len() as GLsizei,
            );
            self.program.set_background_pass(false);
            gl::DrawElementsInstanced(
                gl::TRIANGLES,
                6,
                gl::UNSIGNED_INT,
                ptr::null(),
                self.batch.len() as GLsizei,
            );
        }

        self.batch.clear();
    }

    /// Render a string in a variable location. Used for printing the render timer, warnings and
    /// errors.
    pub fn render_string(
        &mut self,
        glyph_cache: &mut GlyphCache,
        point: Point,
        fg: Rgb,
        bg: Rgb,
        string: &str,
    ) {
        let str_length = string.chars().count();
        let text_run = TextRun {
            line: point.line,
            span: (point.col, point.col + str_length - 1),
            content: (string.to_owned(), vec![None; str_length]),
            fg,
            bg,
            flags: Flags::empty(),
            bg_alpha: 1.0,
            data: None,
        };
        self.render_text_run(&text_run, glyph_cache);
    }

    #[inline]
    fn add_render_item(&mut self, cell: &RenderableCell, glyph: &Glyph) {
        // Flush batch if tex changing.
        if !self.batch.is_empty() && self.batch.tex != glyph.tex_id {
            self.render_batch();
        }

        self.batch.add_item(cell, glyph);

        // Render batch and clear if it's full.
        if self.batch.full() {
            self.render_batch();
        }
    }

    #[inline]
    fn determine_font_key(flags: Flags, glyph_cache: &GlyphCache) -> FontKey {
        // FIXME this is super inefficient.
        match flags & Flags::BOLD_ITALIC {
            Flags::BOLD_ITALIC => glyph_cache.bold_italic_key,
            Flags::ITALIC => glyph_cache.italic_key,
            Flags::BOLD => glyph_cache.bold_key,
            _ => glyph_cache.font_key,
        }
    }

    fn render_zero_widths<'r, I>(
        &mut self,
        zero_width_chars: I,
        cell: &RenderableCell,
        glyph_key: &mut GlyphKey,
        glyph_cache: &mut GlyphCache,
    ) where
        I: Iterator<Item = &'r char>,
    {
        for c in zero_width_chars {
            glyph_key.id = (*c).into();
            let glyph = glyph_cache.get(*glyph_key, self, false);
            self.add_render_item(cell, &glyph);
        }
    }

    fn render_zero_widths_with_data<'r, I>(
        &mut self,
        zero_width_chars: I,
        cell: RenderableCell,
        glyph_key: &mut GlyphKey,
        glyph_cache: &mut GlyphCache,
    ) -> Vec<(RenderableCell, Glyph)>
    where
        I: Iterator<Item = &'r char>,
    {
        let mut result = vec![];
        for c in zero_width_chars {
            glyph_key.id = (*c).into();
            let glyph = glyph_cache.get(*glyph_key, self, false);
            let cell = cell.clone();
            self.add_render_item(&cell, &glyph);
            result.push((cell, glyph));
        }
        result
    }

    pub fn render_text_run(&mut self, text_run: &TextRun, glyph_cache: &mut GlyphCache) {
        let mut cell = text_run.start_cell();
        let (run, zero_widths) = &text_run.content;
        // Get font key for cell
        let font_key = Self::determine_font_key(text_run.flags, glyph_cache);
        let step = if text_run.flags.contains(Flags::WIDE_CHAR) { 2 } else { 1 };
        let mut key = GlyphKey { id: 0.into(), font_key, size: glyph_cache.font_size };
        if text_run.flags.contains(Flags::HIDDEN) {
            for c in run.chars() {
                key.id = c.into();
                // add cell to batch
            }
            let glyph = glyph_cache.get(key, self, true);
            self.add_render_item(&cell, &glyph);
        } else {
            let mut iter = zero_widths.iter();
            for c in run.chars() {
                let zero_width_chars = iter.next().unwrap();
                key.id = c.into();
                // add cell to batch
                let glyph = glyph_cache.get(key, self, true);
                self.add_render_item(&cell, &glyph);
                if let Some(zero_vec) = zero_width_chars {
                    self.render_zero_widths(zero_vec.iter(), &cell, &mut key, glyph_cache);
                }
                cell.column += step;
            }
        }
    }

    pub fn render_text_run_with_data(
        &mut self,
        text_run: &mut TextRun,
        glyph_cache: &mut GlyphCache,
    ) {
        if let Some(data) = &text_run.data {
            for (cell, glyph) in data {
                self.add_render_item(&cell, &glyph);
            }
            return;
        }
        let mut cell = text_run.start_cell();
        let mut result = vec![];
        let (run, zero_widths) = &text_run.content;
        // Get font key for cell
        let font_key = Self::determine_font_key(text_run.flags, glyph_cache);
        let step = if text_run.flags.contains(Flags::WIDE_CHAR) { 2 } else { 1 };
        let mut key = GlyphKey { id: '\0'.into(), font_key, size: glyph_cache.font_size };
        if text_run.flags.contains(Flags::HIDDEN) {
            let mut chars = vec![None; run.len()];
            for (i, c) in run.char_indices() {
                chars[i] = Some(c);
            }
            let infos = glyph_cache.rasterizer.shape(&run, font_key);
            for info in infos {
                let codepoint = info.codepoint;
                let cluster = info.cluster;
                key.id = if codepoint == 0 {
                    chars[cluster as usize]
                        .unwrap_or_else(|| panic!("Couldn't find cluster {}", cluster))
                        .into()
                } else {
                    codepoint.into()
                };
                // add cell to batch
                let glyph = glyph_cache.get(key, self, true);
                self.add_render_item(&cell, &glyph);
                result.push((cell.clone(), glyph));
                cell.column += step;
            }
        } else {
            let mut iter = zero_widths.iter();
            if run.len() > 1 && run.trim().len() > 1 {
                let mut chars = vec![None; run.len()];
                let mut zeros = vec![None; run.len()];
                for (i, c) in run.char_indices() {
                    chars[i] = Some(c);
                    zeros[i] = iter.next().unwrap().as_ref();
                }
                let infos = glyph_cache.rasterizer.shape(&run, font_key);
                for info in infos {
                    let codepoint = info.codepoint;
                    let cluster = info.cluster;
                    let (id, zero_width_chars): (KeyType, _) = if codepoint == 0 {
                        (
                            chars[cluster as usize]
                                .unwrap_or_else(|| panic!("Couldn't find cluster {}", cluster))
                                .into(),
                            zeros[cluster as usize],
                        )
                    } else {
                        (codepoint.into(), zeros[cluster as usize])
                    };
                    key.id = id;
                    // add cell to batch
                    let glyph = glyph_cache.get(key, self, true);
                    self.add_render_item(&cell, &glyph);
                    result.push((cell.clone(), glyph));
                    if let Some(zero_vec) = zero_width_chars {
                        let zw = self.render_zero_widths_with_data(
                            zero_vec.iter(),
                            cell.clone(),
                            &mut key,
                            glyph_cache,
                        );
                        result.extend(zw);
                    }
                    cell.column += step;
                }
            } else {
                for c in run.chars() {
                    let zero_width_chars = iter.next().unwrap();
                    key.id = c.into();
                    let glyph = glyph_cache.get(key, self, true);
                    self.add_render_item(&cell, &glyph);
                    result.push((cell.clone(), glyph));
                    if let Some(zero_vec) = zero_width_chars {
                        let zw = self.render_zero_widths_with_data(
                            zero_vec.iter(),
                            cell.clone(),
                            &mut key,
                            glyph_cache,
                        );
                        result.extend(zw);
                    }
                    cell.column += step;
                }
            }
        }
        text_run.data = Some(result);
    }
}

/// Load a glyph into a texture atlas.
///
/// If the current atlas is full, a new one will be created.
#[inline]
fn load_glyph(
    active_tex: &mut GLuint,
    atlas: &mut Vec<Atlas>,
    current_atlas: &mut usize,
    rasterized: &RasterizedGlyph,
) -> Glyph {
    // At least one atlas is guaranteed to be in the `self.atlas` list; thus
    // the unwrap.
    match atlas[*current_atlas].insert(rasterized, active_tex) {
        Ok(glyph) => glyph,
        Err(AtlasInsertError::Full) => {
            *current_atlas += 1;
            if *current_atlas == atlas.len() {
                let new = Atlas::new(ATLAS_SIZE);
                *active_tex = 0; // Atlas::new binds a texture. Ugh this is sloppy.
                atlas.push(new);
            }
            load_glyph(active_tex, atlas, current_atlas, rasterized)
        },
        Err(AtlasInsertError::GlyphTooLarge) => Glyph {
            tex_id: atlas[*current_atlas].id,
            multicolor: false,
            top: 0,
            left: 0,
            width: 0,
            height: 0,
            uv_bot: 0.,
            uv_left: 0.,
            uv_width: 0.,
            uv_height: 0.,
        },
    }
}

#[inline]
fn clear_atlas(atlas: &mut Vec<Atlas>, current_atlas: &mut usize) {
    for atlas in atlas.iter_mut() {
        atlas.clear();
    }
    *current_atlas = 0;
}

impl<'a> LoadGlyph for LoaderApi<'a> {
    fn load_glyph(&mut self, rasterized: &RasterizedGlyph) -> Glyph {
        load_glyph(self.active_tex, self.atlas, self.current_atlas, rasterized)
    }

    fn clear(&mut self) {
        clear_atlas(self.atlas, self.current_atlas)
    }
}

impl<'a> LoadGlyph for RenderApi<'a> {
    fn load_glyph(&mut self, rasterized: &RasterizedGlyph) -> Glyph {
        load_glyph(self.active_tex, self.atlas, self.current_atlas, rasterized)
    }

    fn clear(&mut self) {
        clear_atlas(self.atlas, self.current_atlas)
    }
}

impl<'a> Drop for RenderApi<'a> {
    fn drop(&mut self) {
        if !self.batch.is_empty() {
            self.render_batch();
        }
    }
}

impl TextShaderProgram {
    pub fn new() -> Result<TextShaderProgram, ShaderCreationError> {
        let vertex_shader = create_shader(gl::VERTEX_SHADER, TEXT_SHADER_V)?;
        let fragment_shader = create_shader(gl::FRAGMENT_SHADER, TEXT_SHADER_F)?;
        let program = create_program(vertex_shader, fragment_shader)?;

        unsafe {
            gl::DeleteShader(fragment_shader);
            gl::DeleteShader(vertex_shader);
            gl::UseProgram(program);
        }

        macro_rules! cptr {
            ($thing:expr) => {
                $thing.as_ptr() as *const _
            };
        }

        macro_rules! assert_uniform_valid {
            ($uniform:expr) => {
                assert!($uniform != gl::INVALID_VALUE as i32);
                assert!($uniform != gl::INVALID_OPERATION as i32);
            };
            ( $( $uniform:expr ),* ) => {
                $( assert_uniform_valid!($uniform); )*
            };
        }

        // get uniform locations
        let (projection, cell_dim, background) = unsafe {
            (
                gl::GetUniformLocation(program, cptr!(b"projection\0")),
                gl::GetUniformLocation(program, cptr!(b"cellDim\0")),
                gl::GetUniformLocation(program, cptr!(b"backgroundPass\0")),
            )
        };

        assert_uniform_valid!(projection, cell_dim, background);

        let shader = Self {
            id: program,
            u_projection: projection,
            u_cell_dim: cell_dim,
            u_background: background,
        };

        unsafe {
            gl::UseProgram(0);
        }

        Ok(shader)
    }

    fn update_projection(&self, width: f32, height: f32, padding_x: f32, padding_y: f32) {
        // Bounds check.
        if (width as u32) < (2 * padding_x as u32) || (height as u32) < (2 * padding_y as u32) {
            return;
        }

        // Compute scale and offset factors, from pixel to ndc space. Y is inverted.
        //   [0, width - 2 * padding_x] to [-1, 1]
        //   [height - 2 * padding_y, 0] to [-1, 1]
        let scale_x = 2. / (width - 2. * padding_x);
        let scale_y = -2. / (height - 2. * padding_y);
        let offset_x = -1.;
        let offset_y = 1.;

        unsafe {
            gl::Uniform4f(self.u_projection, offset_x, offset_y, scale_x, scale_y);
        }
    }

    fn set_term_uniforms(&self, props: &SizeInfo) {
        unsafe {
            gl::Uniform2f(self.u_cell_dim, props.cell_width(), props.cell_height());
        }
    }

    fn set_background_pass(&self, background_pass: bool) {
        let value = if background_pass { 1 } else { 0 };

        unsafe {
            gl::Uniform1i(self.u_background, value);
        }
    }
}

impl Drop for TextShaderProgram {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteProgram(self.id);
        }
    }
}

pub fn create_program(vertex: GLuint, fragment: GLuint) -> Result<GLuint, ShaderCreationError> {
    unsafe {
        let program = gl::CreateProgram();
        gl::AttachShader(program, vertex);
        gl::AttachShader(program, fragment);
        gl::LinkProgram(program);

        let mut success: GLint = 0;
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut success);

        if success == i32::from(gl::TRUE) {
            Ok(program)
        } else {
            Err(ShaderCreationError::Link(get_program_info_log(program)))
        }
    }
}

pub fn create_shader(kind: GLenum, source: &'static str) -> Result<GLuint, ShaderCreationError> {
    let len: [GLint; 1] = [source.len() as GLint];

    let shader = unsafe {
        let shader = gl::CreateShader(kind);
        gl::ShaderSource(shader, 1, &(source.as_ptr() as *const _), len.as_ptr());
        gl::CompileShader(shader);
        shader
    };

    let mut success: GLint = 0;
    unsafe {
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut success);
    }

    if success == GLint::from(gl::TRUE) {
        Ok(shader)
    } else {
        // Read log.
        let log = get_shader_info_log(shader);

        // Cleanup.
        unsafe {
            gl::DeleteShader(shader);
        }

        Err(ShaderCreationError::Compile(log))
    }
}

fn get_program_info_log(program: GLuint) -> String {
    // Get expected log length.
    let mut max_length: GLint = 0;
    unsafe {
        gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut max_length);
    }

    // Read the info log.
    let mut actual_length: GLint = 0;
    let mut buf: Vec<u8> = Vec::with_capacity(max_length as usize);
    unsafe {
        gl::GetProgramInfoLog(program, max_length, &mut actual_length, buf.as_mut_ptr() as *mut _);
    }

    // Build a string.
    unsafe {
        buf.set_len(actual_length as usize);
    }

    // XXX should we expect OpenGL to return garbage?
    String::from_utf8(buf).unwrap()
}

fn get_shader_info_log(shader: GLuint) -> String {
    // Get expected log length.
    let mut max_length: GLint = 0;
    unsafe {
        gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut max_length);
    }

    // Read the info log.
    let mut actual_length: GLint = 0;
    let mut buf: Vec<u8> = Vec::with_capacity(max_length as usize);
    unsafe {
        gl::GetShaderInfoLog(shader, max_length, &mut actual_length, buf.as_mut_ptr() as *mut _);
    }

    // Build a string.
    unsafe {
        buf.set_len(actual_length as usize);
    }

    // XXX should we expect OpenGL to return garbage?
    String::from_utf8(buf).unwrap()
}

#[derive(Debug)]
pub enum ShaderCreationError {
    /// Error reading file.
    Io(io::Error),

    /// Error compiling shader.
    Compile(String),

    /// Problem linking.
    Link(String),
}

impl std::error::Error for ShaderCreationError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ShaderCreationError::Io(err) => err.source(),
            _ => None,
        }
    }
}

impl Display for ShaderCreationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ShaderCreationError::Io(err) => write!(f, "Unable to read shader: {}", err),
            ShaderCreationError::Compile(log) => write!(f, "Failed compiling shader: {}", log),
            ShaderCreationError::Link(log) => write!(f, "Failed linking shader: {}", log),
        }
    }
}

impl From<io::Error> for ShaderCreationError {
    fn from(val: io::Error) -> Self {
        ShaderCreationError::Io(val)
    }
}

/// Manages a single texture atlas.
///
/// The strategy for filling an atlas looks roughly like this:
///
/// ```text
///                           (width, height)
///   ┌─────┬─────┬─────┬─────┬─────┐
///   │ 10  │     │     │     │     │ <- Empty spaces; can be filled while
///   │     │     │     │     │     │    glyph_height < height - row_baseline
///   ├─────┼─────┼─────┼─────┼─────┤
///   │ 5   │ 6   │ 7   │ 8   │ 9   │
///   │     │     │     │     │     │
///   ├─────┼─────┼─────┼─────┴─────┤ <- Row height is tallest glyph in row; this is
///   │ 1   │ 2   │ 3   │ 4         │    used as the baseline for the following row.
///   │     │     │     │           │ <- Row considered full when next glyph doesn't
///   └─────┴─────┴─────┴───────────┘    fit in the row.
/// (0, 0)  x->
/// ```
#[derive(Debug)]
struct Atlas {
    /// Texture id for this atlas.
    id: GLuint,

    /// Width of atlas.
    width: i32,

    /// Height of atlas.
    height: i32,

    /// Left-most free pixel in a row.
    ///
    /// This is called the extent because it is the upper bound of used pixels
    /// in a row.
    row_extent: i32,

    /// Baseline for glyphs in the current row.
    row_baseline: i32,

    /// Tallest glyph in current row.
    ///
    /// This is used as the advance when end of row is reached.
    row_tallest: i32,
}

/// Error that can happen when inserting a texture to the Atlas.
enum AtlasInsertError {
    /// Texture atlas is full.
    Full,

    /// The glyph cannot fit within a single texture.
    GlyphTooLarge,
}

impl Atlas {
    fn new(size: i32) -> Self {
        let mut id: GLuint = 0;
        unsafe {
            gl::PixelStorei(gl::UNPACK_ALIGNMENT, 1);
            gl::GenTextures(1, &mut id);
            gl::BindTexture(gl::TEXTURE_2D, id);
            // Use RGBA texture for both normal and emoji glyphs, since it has no performance
            // impact.
            gl::TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::RGBA as i32,
                size,
                size,
                0,
                gl::RGBA,
                gl::UNSIGNED_BYTE,
                ptr::null(),
            );

            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as i32);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as i32);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);

            gl::BindTexture(gl::TEXTURE_2D, 0);
        }

        Self { id, width: size, height: size, row_extent: 0, row_baseline: 0, row_tallest: 0 }
    }

    pub fn clear(&mut self) {
        self.row_extent = 0;
        self.row_baseline = 0;
        self.row_tallest = 0;
    }

    /// Insert a RasterizedGlyph into the texture atlas.
    pub fn insert(
        &mut self,
        glyph: &RasterizedGlyph,
        active_tex: &mut u32,
    ) -> Result<Glyph, AtlasInsertError> {
        if glyph.width > self.width || glyph.height > self.height {
            return Err(AtlasInsertError::GlyphTooLarge);
        }

        // If there's not enough room in current row, go onto next one.
        if !self.room_in_row(&glyph) {
            self.advance_row()?;
        }

        // If there's still not room, there's nothing that can be done here..
        if !self.room_in_row(&glyph) {
            return Err(AtlasInsertError::Full);
        }

        // There appears to be room; load the glyph.
        Ok(self.insert_inner(glyph, active_tex))
    }

    /// Insert the glyph without checking for room.
    ///
    /// Internal function for use once atlas has been checked for space. GL
    /// errors could still occur at this point if we were checking for them;
    /// hence, the Result.
    fn insert_inner(&mut self, glyph: &RasterizedGlyph, active_tex: &mut u32) -> Glyph {
        let offset_y = self.row_baseline;
        let offset_x = self.row_extent;
        let height = glyph.height as i32;
        let width = glyph.width as i32;
        let multicolor;

        unsafe {
            gl::BindTexture(gl::TEXTURE_2D, self.id);

            // Load data into OpenGL.
            let (format, buffer) = match &glyph.buffer {
                BitmapBuffer::RGB(buffer) => {
                    multicolor = false;
                    (gl::RGB, buffer)
                },
                BitmapBuffer::RGBA(buffer) => {
                    multicolor = true;
                    (gl::RGBA, buffer)
                },
            };

            gl::TexSubImage2D(
                gl::TEXTURE_2D,
                0,
                offset_x,
                offset_y,
                width,
                height,
                format,
                gl::UNSIGNED_BYTE,
                buffer.as_ptr() as *const _,
            );

            gl::BindTexture(gl::TEXTURE_2D, 0);
            *active_tex = 0;
        }

        // Update Atlas state.
        self.row_extent = offset_x + width;
        if height > self.row_tallest {
            self.row_tallest = height;
        }

        // Generate UV coordinates.
        let uv_bot = offset_y as f32 / self.height as f32;
        let uv_left = offset_x as f32 / self.width as f32;
        let uv_height = height as f32 / self.height as f32;
        let uv_width = width as f32 / self.width as f32;

        Glyph {
            tex_id: self.id,
            multicolor,
            top: glyph.top as i16,
            left: glyph.left as i16,
            width: width as i16,
            height: height as i16,
            uv_bot,
            uv_left,
            uv_width,
            uv_height,
        }
    }

    /// Check if there's room in the current row for given glyph.
    fn room_in_row(&self, raw: &RasterizedGlyph) -> bool {
        let next_extent = self.row_extent + raw.width as i32;
        let enough_width = next_extent <= self.width;
        let enough_height = (raw.height as i32) < (self.height - self.row_baseline);

        enough_width && enough_height
    }

    /// Mark current row as finished and prepare to insert into the next row.
    fn advance_row(&mut self) -> Result<(), AtlasInsertError> {
        let advance_to = self.row_baseline + self.row_tallest;
        if self.height - advance_to <= 0 {
            return Err(AtlasInsertError::Full);
        }

        self.row_baseline = advance_to;
        self.row_extent = 0;
        self.row_tallest = 0;

        Ok(())
    }
}
