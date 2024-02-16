//! mdbook backend for AsciiDoc generation

use lazy_static::lazy_static;
use log::{debug, error, info, trace, warn};
use mdbook::{
    book::{BookItem, Chapter},
    renderer::RenderContext,
};
use pulldown_cmark as md;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::ffi::OsStr;
use std::io::Write;
use std::path::{Path, PathBuf};

#[cfg(test)]
mod tests;

lazy_static! {
    /// Regexp to match AsciiDoc images.
    static ref ASCIIDOC_IMAGE_RE: Regex = Regex::new(r"image::?(?P<url>[a-zA-Z0-9_/.]+)").unwrap();
    /// Regexp to match the invented tag used to transmit raw AsciiDoc.
    static ref ASCIIDOC_ESCAPE_RE: Regex =
        Regex::new(r#"<asciidoc content='(?P<content>[^']+)'"#).unwrap();
    /// Regexp to match HTML comments.
    static ref HTML_COMMENT_RE : Regex = Regex::new(r#"<!--\s*(?P<comment>.*?)\s*-->"#).unwrap();
    /// Regexp to match Unicode U+1234 expressions.
    static ref UNICODE_CHAR_RE: Regex = Regex::new(r#"^U\+(?P<code>[0-9a-fA-F]{4})(?P<rest>.*)$"#).unwrap();
    /// Tags attached to code blocks that should be ignored.
    static ref IGNORED_CODE_TAGS: HashSet<&'static str> = HashSet::from([
        "ignore",
    ]);
    /// Tags attached to code blocks that identify languages.
    static ref LANGUAGES: HashSet<&'static str> = HashSet::from([
        // Extras
        "text", "c++", "toml",
        // List from mdbook supported languages
        "apache", "armasm", "bash", "c", "coffeescript", "cpp", "csharp", "css", "d", "diff", "go",
        "handlebars", "haskell", "http", "ini", "java", "javascript", "json", "julia", "kotlin",
        "less", "lua", "makefile", "markdown", "nginx", "objectivec", "perl", "php", "plaintext",
        "properties", "python", "r", "ruby", "rust", "scala", "scss", "shell", "sql", "swift",
        "typescript", "x86asm", "xml", "yaml",
    ]);
}

/// Main entrypoint for backend.
fn main() -> Result<(), Error> {
    env_logger::init();
    let mut stdin = std::io::stdin();
    let ctx = RenderContext::from_json(&mut stdin).unwrap();
    let built_version = semver::Version::parse(mdbook::MDBOOK_VERSION).unwrap_or_else(|e| {
        panic!(
            "failed to parse mdbook version {}: {:?}",
            mdbook::MDBOOK_VERSION,
            e
        )
    });
    let book_version = semver::Version::parse(&ctx.version)
        .unwrap_or_else(|e| panic!("failed to parse book version {}: {:?}", ctx.version, e));
    if built_version != book_version {
        error!(
            "backend built on v{} of mdbook, processing v{} book",
            mdbook::MDBOOK_VERSION,
            ctx.version
        );
    }

    let mut backend = AsciiDocBackend::new(&ctx).expect("failed to initialize");
    info!(
        "created AsciiDoc backend, outputs to '{}'",
        backend.dest_dir.display(),
    );

    backend.process(ctx)
}

/// Local error type.
#[derive(Debug)]
enum Error {
    General(String),
    Io(std::io::Error),
}

impl From<String> for Error {
    fn from(e: String) -> Error {
        Self::General(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Error {
        Self::Io(e)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Strip {
    None,
    TrailingWhitespace,
}

/// What kind of list processing is currently inside.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum List {
    Unnumbered,
    Numbered,
}

/// What kind of rendering is processing currently inside.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Render {
    Italic,
    Bold,
    Strikethrough,
    Table,
    CodeBlock(CodeBlock),
    Heading,
    Link,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CodeBlock {
    Normal,
    Wrapped,
}

#[derive(Clone, Default)]
struct AsciiDocOutput {
    /// Line-by-line content.
    prev_lines: Vec<String>,
    /// Current line being accumulated.
    cur_line: String,
    /// Current stack of lists.
    lists: Vec<List>,
    /// Current stack of render modes.
    modes: Vec<Render>,
}

impl AsciiDocOutput {
    fn write(&mut self, text: &str) {
        self.cur_line += text;
    }

    fn writeln(&mut self, text: &str) {
        self.cur_line += text;
        let cur_line = std::mem::replace(&mut self.cur_line, "".to_string());
        self.prev_lines.push(cur_line);
    }

    fn prev_line(&self) -> Option<&str> {
        if self.prev_lines.is_empty() {
            None
        } else {
            Some(&self.prev_lines[self.prev_lines.len() - 1])
        }
    }

    fn col(&self) -> usize {
        self.cur_line.len()
    }

    /// Is the current output position just after a blank line?
    fn after_blank(&self) -> bool {
        if !self.cur_line.is_empty() {
            false
        } else {
            match self.prev_line() {
                Some(line) => line.is_empty(),
                None => {
                    // Treat the first line of a file as being after a blank line.
                    true
                }
            }
        }
    }

    fn replace_all(&mut self, from: &str, to: &AsciiDocOutput, strip: Strip) {
        let mut to_data = Vec::new();
        to.write_to(&mut to_data)
            .expect("failed to write substitution text");
        let to = String::from_utf8(to_data).expect("non UTF8 text!");
        let to = match strip {
            Strip::None => &to,
            Strip::TrailingWhitespace => to.trim_end(),
        };
        for line in &mut self.prev_lines {
            let newline = line.replace(from, to);
            let _ = std::mem::replace(line, newline);
        }
        self.cur_line = self.cur_line.replace(from, to);
    }

    fn write_to<T: std::io::Write>(&self, out: &mut T) -> Result<(), Error> {
        for line in &self.prev_lines {
            writeln!(out, "{}", line)?;
        }
        writeln!(out, "{}", self.cur_line)?;
        Ok(())
    }

    fn current_list(&self) -> List {
        assert!(!self.lists.is_empty(), "not in a list!");
        self.lists[self.lists.len() - 1]
    }

    fn in_table(&self) -> bool {
        self.modes.contains(&Render::Table)
    }

    fn in_italics(&self) -> bool {
        self.modes.contains(&Render::Italic)
    }

    fn in_header(&self) -> bool {
        self.modes.contains(&Render::Heading)
    }

    fn in_link(&self) -> bool {
        self.modes.contains(&Render::Link)
    }

    fn code_block(&self) -> Option<CodeBlock> {
        self.modes.iter().rev().find_map(|mode| match mode {
            Render::CodeBlock(c) => Some(*c),
            _ => None,
        })
    }
}

// Macros for output with tracing.
macro_rules! out {
    ($f:ident, $($arg:tt)+) => { {
        let output = format!("{}", format_args!($($arg)+));
        trace!("[AD] emit: '{}'", output);
        $f.write(&output);
    } }
}

macro_rules! outln {
    ($f:ident, $($arg:tt)+) => { {
        let output = format!("{}", format_args!($($arg)+));
        trace!("[AD] emit: '{}\\n'", output);
        $f.writeln(&output);
    } }
}

macro_rules! crlf {
    ($f:ident) => {{
        trace!("[AD] emit crlf: '\\n'");
        $f.writeln("");
    }};
}

macro_rules! cr {
    ($f:ident) => {{
        if $f.col() > 0 {
            trace!("[AD] cr needed so emit: '\\n'");
            $f.writeln("");
        } else {
            trace!("[AD] cr but at col 0 already");
        }
    }};
}

macro_rules! maybelf {
    ($f:ident) => {{
        if $f.prev_line().is_some() {
            trace!("[AD] lf needed so emit: '\\n'");
            $f.writeln("");
        } else {
            trace!("[AD] at start so no lf");
        }
    }};
}

type RenderAfterLink = fn(&AsciiDocBackend, &AsciiDocOutput, &str) -> String;
type ShouldRenderAfter = fn(&AsciiDocBackend, &str) -> bool;

/// How to render hyperlinks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum LinkMode {
    /// Render same as in the MarkDown (the text itself is a link).
    /// Invisible in printed output.
    #[default]
    Inline,
    /// Render after the text with the link, transforming the URL
    /// with the given function (which should output AsciiDoc).
    After(RenderAfterLink),
    /// Render links differently depending on the result of the first function.
    /// - `true`: render links after the text using the given function (like `After`).
    /// - `false`: render links inline with the text (like `Inline`)
    Both(ShouldRenderAfter, RenderAfterLink),
}

/// AsciiDoc backend processor.
#[derive(Debug)]
struct AsciiDocBackend {
    /// Where to put output.
    pub dest_dir: PathBuf,
    /// Where to get input.
    src_dir: PathBuf,
    /// Whether to cope with (some) AsciiDoc in the input.
    allow_asciidoc: bool,
    /// Heading extra offset to apply.
    heading_offset: isize,
    /// Chapters to skip (by name).
    skip_chapters: Vec<String>,
    /// Unicode character substitutions.  Keys and values are a single char.
    unicode_subst: HashMap<String, String>,
    /// Map from style=>blockname for additional wrapping of code blocks.
    code_block_wrap: HashMap<String, String>,
    /// Delimiter for additional wrapping of code blocks.
    code_block_wrap_delimiter: String,
    /// How to map links.
    link_mode: LinkMode,
    /// Link shortening table.
    short_links: HashMap<String, String>,
    /// Omit links in headers.
    omit_heading_links: bool,
    /// Suppress newlines in link text.
    suppress_link_newlines: bool,
    /// Filename substitution.
    filename_subst: HashMap<String, String>,
}

impl AsciiDocBackend {
    /// Create a new backend, with options populated from the given context.
    pub fn new(ctx: &RenderContext) -> Result<Self, Error> {
        let allow_asciidoc = if let Some(toml::Value::Boolean(v)) =
            ctx.config.get("output.asciidoc.allow-asciidoc")
        {
            *v
        } else {
            false
        };
        let heading_offset = if let Some(toml::Value::Integer(v)) =
            ctx.config.get("output.asciidoc.heading-offset")
        {
            *v as isize
        } else {
            0
        };
        let link_mode =
            if let Some(toml::Value::String(m)) = ctx.config.get("output.asciidoc.link-mode") {
                match m.as_str() {
                    "default" => LinkMode::Inline,
                    "after" => LinkMode::After(Self::link_after),
                    "shorten" => LinkMode::After(Self::selective_shorten),
                    "shorten-some" => LinkMode::Both(Self::has_short_url, Self::selective_shorten),
                    _ => panic!("Unrecognized link-mode flag {m}"),
                }
            } else {
                LinkMode::default()
            };

        let mut short_links = HashMap::new();
        if let Some(toml::Value::Table(table)) = ctx.config.get("output.asciidoc.url-shorten") {
            for (key, val) in table {
                if let toml::Value::String(val) = val {
                    log::info!("Shorten URL {key} to {val}");
                    short_links.insert(key.to_string(), val.to_string());
                } else {
                    log::error!("Ignoring non-string value ({val:?}) for url-shorten");
                }
            }
        }

        let skip_chapters =
            if let Some(toml::Value::String(v)) = ctx.config.get("output.asciidoc.skip-chapters") {
                v
            } else {
                ""
            }
            .split(',')
            .map(|s| s.to_owned())
            .collect();

        let mut unicode_subst = HashMap::new();
        if let Some(toml::Value::Table(table)) = ctx.config.get("output.asciidoc.unicode-subst") {
            for (key, val) in table {
                if let toml::Value::String(value) = val {
                    let key = transmute_unicode(key);
                    let value = transmute_unicode(value);
                    let mut srcs = key.chars();
                    let mut dests = value.chars();

                    let src = match (srcs.next(), srcs.next()) {
                        (Some(v), None) => v,
                        (Some(c1), Some(c2)) => {
                            log::error!("Expect a single Unicode char as unicode-subst source, got '{c1}' '{c2}'...");
                            continue;
                        }
                        (None, _) => {
                            log::error!("Expect a single Unicode char as unicode-subst source, got empty string");
                            continue;
                        }
                    };
                    let dest = match (dests.next(), dests.next()) {
                        (Some(v), None) => v,
                        (Some(c1), Some(c2)) => {
                            log::error!("Expect a single Unicode char as unicode-subst dest, got '{c1}' '{c2}'...");
                            continue;
                        }
                        (None, _) => {
                            log::error!("Expect a single Unicode char as unicode-subst dest, got empty string");
                            continue;
                        }
                    };
                    log::info!("Map unicode '{src}' to '{dest}'");
                    unicode_subst.insert(format!("{src}"), format!("{dest}"));
                }
            }
        }

        let mut code_block_wrap = HashMap::new();
        if let Some(toml::Value::Table(table)) = ctx.config.get("output.asciidoc.code-block-wrap") {
            for (key, val) in table {
                if let toml::Value::String(val) = val {
                    log::info!("Will wrap code blocks marked with '{key}' in a '{val}' block");
                    code_block_wrap.insert(key.to_string(), val.to_string());
                } else {
                    log::error!("Ignoring non-string value ({val:?}) for code-block-wrap");
                }
            }
        }
        let code_block_wrap_delimiter = if let Some(toml::Value::String(w)) =
            ctx.config.get("output.asciidoc.code-block-wrap-delimiter")
        {
            w.to_string()
        } else {
            "====".to_string()
        };

        let omit_heading_links = if let Some(toml::Value::Boolean(v)) =
            ctx.config.get("output.asciidoc.omit-heading-links")
        {
            *v
        } else {
            false
        };

        let suppress_link_newlines = if let Some(toml::Value::Boolean(v)) =
            ctx.config.get("output.asciidoc.suppress-link-newlines")
        {
            *v
        } else {
            false
        };

        let mut filename_subst = HashMap::new();
        if let Some(toml::Value::Table(table)) = ctx.config.get("output.asciidoc.file-rename") {
            for (key, val) in table {
                if let toml::Value::String(val) = val {
                    log::info!("Rename file '{key}' to '{val}'");
                    filename_subst.insert(key.to_string(), val.to_string());
                } else {
                    log::error!("Ignoring non-string value ({val:?}) for file-rename");
                }
            }
        }

        let dest_dir = ctx.destination.clone();
        std::fs::create_dir_all(&dest_dir).map_err(|e| {
            format!(
                "Failed to create output directory '{}': {:?}",
                dest_dir.display(),
                e
            )
        })?;
        let mut src_dir = ctx.root.clone();
        src_dir.push(ctx.config.book.src.clone());

        let backend = Self {
            dest_dir,
            src_dir,
            allow_asciidoc,
            heading_offset,
            skip_chapters,
            unicode_subst,
            code_block_wrap,
            code_block_wrap_delimiter,
            link_mode,
            short_links,
            omit_heading_links,
            suppress_link_newlines,
            filename_subst,
        };
        info!("configured {backend:?}");
        Ok(backend)
    }

    /// Process the AsciiDoc document.
    pub fn process(&mut self, ctx: RenderContext) -> Result<(), Error> {
        let title = ctx
            .config
            .book
            .title
            .unwrap_or("Unknown Title!".to_string());
        let description = ctx.config.book.description;
        let authors = ctx.config.book.authors;

        debug!("Title '{title:?}' description '{description:?}' authors: {authors:?}");
        let topfilename = self.dest_dir.join("book.asciidoc");
        let mut topfile = std::fs::File::create(&topfilename).map_err(|e| {
            format!(
                "Failed to create output file '{}': {:?}",
                topfilename.display(),
                e
            )
        })?;
        writeln!(topfile, "= {title}")?;
        let mut first = true;
        for author in authors {
            if !first {
                write!(topfile, "; ")?;
            }
            write!(topfile, "{author}")?;
            first = false;
        }
        if !first {
            writeln!(topfile)?;
        }
        writeln!(topfile, ":doctype: book")?;
        writeln!(topfile)?;

        let mut need_numbering_reset = false;
        for item in ctx.book.iter() {
            match &item {
                BookItem::Chapter(ch) => {
                    if let Some(filename) = &ch.path {
                        let filename = filename.to_str().expect("invalid pathname").to_owned();
                        if self.skip_chapters.contains(&filename) {
                            info!("Skipping chapter '{}' due to skip-chapters config", ch.name);
                            continue;
                        }
                    }
                    let (filename, offset) = self.process_chapter(ch)?;
                    if need_numbering_reset {
                        writeln!(topfile, r#"[role="pagenumrestart"]"#)?;
                        need_numbering_reset = false;
                    }
                    writeln!(topfile, "include::{filename}[leveloffset={offset:+}]")?;
                    if ch.name == "Preface" {
                        // Reset page numbering after processing a preface.
                        need_numbering_reset = true;
                    }
                }
                BookItem::Separator => debug!("Visit separator"),
                BookItem::PartTitle(title) => debug!("Visit part '{title}'"),
            }
        }

        Ok(())
    }

    /// Process a single chapter.
    fn process_chapter(&mut self, ch: &Chapter) -> Result<(String, isize), Error> {
        use md::{Event, Tag};
        debug!("Visit chapter '{}' from file '{:?}'", ch.name, ch.path);
        let offset = ch.parent_names.len() as isize + self.heading_offset;

        let mut f = AsciiDocOutput::default();
        let parser = md::Parser::new_ext(
            &ch.content,
            md::Options::ENABLE_TABLES
                | md::Options::ENABLE_FOOTNOTES
                | md::Options::ENABLE_STRIKETHROUGH
                | md::Options::ENABLE_TASKLISTS
                | md::Options::ENABLE_HEADING_ATTRIBUTES,
        );
        let mut indent = Indent::new(1);
        let mut swapped_f = None;
        if let Some(filename) = &ch.path {
            outln!(
                f,
                "[#file_{}]",
                filename.display().to_string().replace('.', "_")
            );
        }
        for event in parser {
            match &event {
                Event::Start(tag) => {
                    trace!("[MD]{indent}Start({tag:?})");
                    match tag {
                        Tag::Paragraph => {}
                        Tag::Heading(level, _frag_id, _classes) => {
                            let level = match level {
                                md::HeadingLevel::H1 => 1,
                                md::HeadingLevel::H2 => 2,
                                md::HeadingLevel::H3 => 3,
                                md::HeadingLevel::H4 => 4,
                                md::HeadingLevel::H5 => 5,
                                md::HeadingLevel::H6 => 6,
                            };
                            f.modes.push(Render::Heading);
                            cr!(f);
                            maybelf!(f);
                            if ch.name == "Preface" && level == 1 {
                                // Assume a chapter called "Preface" should map to an AsciiDoc preface.
                                outln!(f, "[preface]");
                            }
                            if ch.name == "Afterword" && level == 1 {
                                // Assume a chapter called "Afterword" should map to an AsciiDoc afterword.
                                outln!(f, "[appendix]");
                                outln!(f, r#"[role="afterword"]"#);
                            }
                            out!(f, "{} ", "=".repeat(level));
                        }
                        Tag::BlockQuote => {
                            cr!(f);
                            outln!(f, "[quote]");
                            outln!(f, "____");
                        }
                        Tag::CodeBlock(kind) => {
                            cr!(f);
                            let mut code_block = CodeBlock::Normal;
                            let mut attrs = "".to_string();
                            if let md::CodeBlockKind::Fenced(lang) = kind {
                                let tags: Vec<&str> = lang.split(',').collect();
                                // Only insert the tags that look like languages into the AsciiDoc
                                let lang_tags: Vec<&str> = tags
                                    .iter()
                                    .filter_map(|s| {
                                        if LANGUAGES.contains(*s) {
                                            Some(*s)
                                        } else {
                                            None
                                        }
                                    })
                                    .collect();
                                let langs = lang_tags.join(",");
                                let other_tags: Vec<&str> = tags
                                    .iter()
                                    .filter_map(|s| {
                                        if LANGUAGES.contains(*s) || IGNORED_CODE_TAGS.contains(*s)
                                        {
                                            // Ignore language markers and tags explicitly configured to be ignored.
                                            None
                                        } else {
                                            Some(*s)
                                        }
                                    })
                                    .collect();
                                attrs += &format!(",{langs}");
                                if !other_tags.is_empty() {
                                    let extras = other_tags.join(",");
                                    debug!("apply extra code tags '{extras}' as named attribute");
                                    attrs += &format!(",extras=\"{extras}\"");
                                    // Find the first of `other_tags` that has an associated code block wrapper.
                                    let block_wrapper = other_tags
                                        .iter()
                                        .find_map(|tag| self.code_block_wrap.get(*tag));
                                    if let Some(block) = block_wrapper {
                                        log::debug!("code with {extras} triggers {block} wrapper");
                                        let delimiter = &self.code_block_wrap_delimiter;
                                        outln!(f, "[{block}]\n{delimiter}");
                                        code_block = CodeBlock::Wrapped;
                                    }
                                }
                            }
                            f.modes.push(Render::CodeBlock(code_block));
                            out!(f, "[source{attrs}");
                            outln!(f, "]");
                            outln!(f, "----");
                        }
                        Tag::List(first_num) => {
                            let list = if let Some(first) = first_num {
                                if *first != 1 {
                                    debug!("numbered list starting at {first}");
                                    outln!(f, "[start={first}]");
                                }
                                List::Numbered
                            } else {
                                List::Unnumbered
                            };
                            f.lists.push(list);
                        }
                        Tag::Item => {
                            let indent = f.lists.len();
                            let lead = match f.current_list() {
                                List::Numbered => ".",
                                List::Unnumbered => "*",
                            };
                            cr!(f);
                            out!(f, "{} ", lead.repeat(indent));
                        }
                        Tag::FootnoteDefinition(text) => {
                            // Switch to accumulating output for the footnote.
                            debug!("Accumulate text for definition of footnote {text}");
                            assert!(swapped_f.is_none());
                            swapped_f = Some(f);
                            f = AsciiDocOutput::default();
                        }

                        // Table elements
                        Tag::Table(aligns) => {
                            let cols = aligns
                                .iter()
                                .map(|a| match a {
                                    md::Alignment::None => "1",
                                    md::Alignment::Left => "<1",
                                    md::Alignment::Center => "^1",
                                    md::Alignment::Right => ">1",
                                })
                                .collect::<Vec<&str>>()
                                .join(",");
                            outln!(f, "[cols=\"{cols}\"]");
                            outln!(f, "|===");
                        }
                        Tag::TableHead => {
                            // For a head, *don't* prefix with blank line.
                        }
                        Tag::TableRow => {
                            cr!(f);
                            crlf!(f);
                        }
                        Tag::TableCell => {
                            f.modes.push(Render::Table);
                            out!(f, "| ");
                        }

                        // Inline elements
                        Tag::Emphasis => {
                            f.modes.push(Render::Italic);
                            out!(f, "_");
                        }
                        Tag::Strong => {
                            // Use double asterisk so that mid-*wo*rd bold works.
                            f.modes.push(Render::Bold);
                            out!(f, "**");
                        }
                        Tag::Strikethrough => {
                            f.modes.push(Render::Strikethrough);
                            out!(f, "[line-through]#");
                        }
                        Tag::Link(_link_type, dest_url, _title) => {
                            if f.in_header() && self.omit_heading_links {
                                debug!("Skip link '{dest_url}' in header");
                            } else {
                                f.modes.push(Render::Link);
                                self.emit_link_before(&mut f, dest_url);
                            }
                        }
                        Tag::Image(_link_type, dest_url, _title) => {
                            if f.after_blank() {
                                // Block image (::).
                                out!(f, "image::{dest_url}[\"");
                            } else {
                                // Inline image (:).
                                out!(f, "image:{dest_url}[\"");
                            }
                            self.copy_file_to_output(dest_url)?;

                            // May be followed by an `Event::Text` holding the alt text.
                        }
                    }
                    indent.inc();
                }
                Event::End(tag) => {
                    indent.dec();
                    trace!("[MD]{indent}End({tag:?})");
                    match tag {
                        Tag::Paragraph => {
                            cr!(f); // End the current in-progress line.
                            crlf!(f); // Additional blank line.
                        }
                        Tag::Heading(_, _, _) => {
                            assert_eq!(f.modes.pop(), Some(Render::Heading));
                            cr!(f); // End the current in-progress line.
                            crlf!(f); // Additional blank line.
                        }
                        Tag::CodeBlock(_kind) => {
                            let mode = match f.modes.pop() {
                                Some(Render::CodeBlock(m)) => m,
                                m => panic!("Unexpected last mode {m:?}"),
                            };
                            outln!(f, "----");
                            if mode == CodeBlock::Wrapped {
                                let delimiter = &self.code_block_wrap_delimiter;
                                outln!(f, "{delimiter}");
                            }
                            crlf!(f);
                        }
                        Tag::BlockQuote => {
                            cr!(f);
                            outln!(f, "____");
                        }
                        Tag::List(_first_num) => {
                            f.lists.pop().expect("leaving a list when not in one!");
                            crlf!(f);
                        }
                        Tag::Item => {
                            crlf!(f);
                        }
                        Tag::FootnoteDefinition(text) => {
                            // Switch back to accumulating text for the chapter.
                            debug!("Done accumulating text for definition of footnote {text}");
                            let footnote = f;
                            f = swapped_f.take().expect("No stored output!");
                            f.replace_all(
                                &footnote_marker(text),
                                &footnote,
                                Strip::TrailingWhitespace,
                            );
                        }

                        // Table elements
                        Tag::Table(_aligns) => {
                            cr!(f);
                            outln!(f, "|===");
                            crlf!(f);
                        }
                        Tag::TableHead | Tag::TableRow => {}
                        Tag::TableCell => {
                            assert_eq!(f.modes.pop(), Some(Render::Table));
                        }

                        // Inline elements
                        Tag::Emphasis => {
                            assert_eq!(f.modes.pop(), Some(Render::Italic));
                            out!(f, "_");
                        }
                        Tag::Strong => {
                            assert_eq!(f.modes.pop(), Some(Render::Bold));
                            out!(f, "**");
                        }
                        Tag::Strikethrough => {
                            assert_eq!(f.modes.pop(), Some(Render::Strikethrough));
                            out!(f, "#");
                        }
                        Tag::Link(_link_type, dest_url, _title) => {
                            if f.in_header() && self.omit_heading_links {
                                debug!("Skip link '{dest_url}' in header");
                            } else {
                                assert_eq!(f.modes.pop(), Some(Render::Link));
                                self.emit_link_after(&mut f, dest_url);
                            }
                        }
                        Tag::Image(_link_type, _dest_url, title) => {
                            if !title.is_empty() {
                                out!(f, "\",title=\"{title}");
                            }
                            out!(f, "\"]");
                        }
                    }
                }
                Event::Text(text) => {
                    trace!("[MD]{indent}Text({text})");
                    indent.inc();
                    let mut text = self.replace_unicode(text);
                    if f.in_table() {
                        // Escape any vertical bars while in a table.
                        text = text.replace('|', "\\|");
                    }

                    if self.allow_asciidoc {
                        self.process_potential_asciidoc(&text);
                    }

                    if f.code_block().is_none() {
                        // Outside of a code block, escape special characters in general.
                        out!(f, "{}", md2ad(&text));
                    } else {
                        out!(f, "{}", text);
                    }

                    indent.dec();
                }
                Event::Code(text) => {
                    trace!("[MD]{indent}Code({text})");
                    let mut text = self.replace_unicode(text);
                    if f.in_table() {
                        // Escape any vertical bars while in a table.
                        text = text.replace('|', "\\|");
                    }
                    if text.contains('+') {
                        // - Double-backtick allows use in positions without surrounding space.
                        // - Passthrough macro because the text has plus signs in it.
                        out!(f, "``pass:[{text}]``");
                    } else {
                        // - Backtick induces fixed-width font.
                        // - Double-backtick allows use in positions without surrounding space.
                        // - Plus sign prevents interpretation of characters.
                        out!(f, "``+{text}+``");
                    }
                }
                Event::Html(text) => {
                    trace!("[MD]{indent}Html({text})");
                    let html = text.to_string();
                    let mut done = false;
                    // Watch out for escaped AsciiDoc.
                    if self.allow_asciidoc {
                        if let Some(caps) = ASCIIDOC_ESCAPE_RE.captures(&html) {
                            let fragment = caps
                                .name("content")
                                .expect("found AsciiDoc escape with no content");
                            let fragment: &str = fragment.into();
                            debug!("found embedded AsciiDoc '{fragment}'");
                            self.process_potential_asciidoc(fragment);
                            out!(f, "{}", fragment);
                            done = true;
                        }
                    }
                    if !done {
                        match html.trim() {
                            "<br/>" => {
                                outln!(f, "&nbsp;");
                                crlf!(f);
                            }
                            "<hr/>" => {
                                outln!(f, "'''");
                                crlf!(f);
                            }
                            "<aside>" => {
                                outln!(f, "****");
                                crlf!(f);
                            }
                            "</aside>" => {
                                outln!(f, "****");
                                crlf!(f);
                            }
                            "<sup>" | "</sup>" => {
                                out!(f, "^");
                            }
                            "<sub>" | "</sub>" => {
                                out!(f, "~");
                            }
                            html => {
                                if let Some(caps) = HTML_COMMENT_RE.captures(html) {
                                    let comment: &str = caps
                                        .name("comment")
                                        .expect("HTML comment content missing")
                                        .into();
                                    let comment = comment.replace('\n', " ");
                                    outln!(f, "// {comment}");
                                } else {
                                    warn!("Unhandled HTML: {html} in {:?} ({:?})", ch.path, ch.name)
                                }
                            }
                        }
                    }
                }
                Event::FootnoteReference(text) => {
                    trace!("[MD]{indent}FootnoteRef({text})");
                    out!(f, "footnote:[{}]", footnote_marker(text));
                }
                Event::SoftBreak => {
                    trace!("[MD]{indent}SoftBreak");
                    if f.in_link() && self.suppress_link_newlines {
                        out!(f, " ");
                    } else {
                        crlf!(f);
                    }
                }
                Event::HardBreak => {
                    trace!("[MD]{indent}HardBreak");
                }
                Event::Rule => {
                    trace!("[MD]{indent}Rule");
                }
                Event::TaskListMarker(done) => {
                    trace!("[MD]{indent}TaskListMarker({done})");
                    let marker = if *done { "[x]" } else { "[ ]" };
                    out!(f, "{} ", marker);
                }
            }
        }

        // Figure out the unadorned filename for this chapter.
        let filename = ch
            .path
            .as_ref()
            .map(|path| {
                path.to_string_lossy()
                    .strip_suffix(".md")
                    .unwrap()
                    .to_string()
            })
            .unwrap_or_else(|| {
                // No path, so build one from the chapter name
                ch.name.to_ascii_lowercase().replace(' ', "_")
            });
        let filename = self.fsubst(&filename);
        let filename = filename + ".asciidoc";
        debug!("basename = {filename}");

        // Create the corresponding file in the output directory.
        let outfilename = self.dest_dir.join(filename.clone());
        let dest_dir = Path::new(&outfilename).parent().unwrap();
        debug!("mkdir -p {dest_dir:?}");
        std::fs::create_dir_all(dest_dir).map_err(|e| {
            format!(
                "Failed to create output directory '{}': {:?}",
                dest_dir.display(),
                e
            )
        })?;
        debug!("output to {outfilename:?}");
        let mut outfile = std::fs::File::create(&outfilename).map_err(|e| {
            format!(
                "Failed to create output file '{}': {:?}",
                outfilename.display(),
                e
            )
        })?;
        f.write_to(&mut outfile)?;
        Ok((filename, offset))
    }

    // If the url is a local file, copy it into an equivalent location in the output directory.
    fn copy_file_to_output(&self, url: &str) -> Result<(), Error> {
        if let Some(filename) = url_is_local(url) {
            debug!("[AD] copy {filename} to output directory");
            let path = Path::new(&filename);
            if let Some(dir) = path.parent() {
                let mut dest_dir = self.dest_dir.clone();
                dest_dir.push(dir);
                info!("[AD] mkdir {}", dest_dir.display());
                std::fs::create_dir_all(&dest_dir).map_err(|e| {
                    format!(
                        "Failed to create output directory '{}': {:?}",
                        dest_dir.display(),
                        e
                    )
                })?;
            }
            let mut src_file = self.src_dir.clone();
            src_file.push(path);
            let mut dest_file = self.dest_dir.clone();
            dest_file.push(path);
            debug!("[AD] cp {} {}", src_file.display(), dest_file.display());
            std::fs::copy(src_file, dest_file)?;

            // If the file looks like an SVG file, attempt to copy an equivalent PNG file too.
            if path.extension() == Some(OsStr::new("svg")) {
                let png_path = path.with_extension("png");
                let mut src_file = self.src_dir.clone();
                src_file.push(png_path.clone());
                let mut dest_file = self.dest_dir.clone();
                dest_file.push(png_path);
                debug!("[AD] cp {} {}", src_file.display(), dest_file.display());
                let _ = std::fs::copy(src_file, dest_file);
            }
        }
        Ok(())
    }

    /// Process raw text from the input that might already contain some AsciiDoc.
    fn process_potential_asciidoc(&self, text: &str) {
        if let Some(caps) = ASCIIDOC_IMAGE_RE.captures(text) {
            if let Some(url) = caps.name("url") {
                let url: &str = url.into();
                debug!("copy file {url} to output directory");
                let result = self.copy_file_to_output(url);
                if let Err(e) = result {
                    error!("Failed to copy possible AsciiDoc-reference image {url}: {e:?}");
                }
            }
        }
    }

    /// Perform Unicode character substitution on the text.
    fn replace_unicode(&self, input: &str) -> String {
        let mut text = input.to_string();
        for (src, dest) in &self.unicode_subst {
            text = text.replace(src, dest);
        }
        if text != input {
            log::info!("Replaced '{input}' with '{text}' due to Unicode substitution");
        }
        text
    }

    /// Emit AsciiDoc before a link.
    fn emit_link_before(&self, f: &mut AsciiDocOutput, dest_url: &str) {
        if let Some(local_file) = url_is_local(dest_url) {
            trace!("start link {dest_url} is local at {local_file}");
            // Generally want to surround the URL with ++ to prevent interpretation.
            let mut dest_url = dest_url.to_string();
            let (mac, esc) = if local_file.ends_with(".md") {
                // However, putting ++ around xref doesn't appear to work.
                debug!("transform target '{local_file}' of link into local xref");
                dest_url = format!("file_{}", local_file.replace('.', "_"));
                ("xref", "")
            } else {
                ("link", "++")
            };
            out!(f, "{mac}:{esc}{dest_url}{esc}[");
        } else {
            // URL is not a local file.
            let after = match self.link_mode {
                LinkMode::Inline => false,
                LinkMode::After(_) => true,
                LinkMode::Both(has_short_url, _) => has_short_url(self, dest_url),
            };
            if after {
                trace!("link {dest_url} happens after");
                // Link processing happens at end link, so do nothing.
            } else {
                trace!("start inline link {dest_url}");
                out!(f, "link:++{dest_url}++[");
            }
        }
    }

    /// Emit AsciiDoc after a link.
    fn emit_link_after(&self, f: &mut AsciiDocOutput, dest_url: &str) {
        if url_is_local(dest_url).is_some() {
            trace!("end link {dest_url} is local");
            out!(f, "]")
        } else {
            // URL is not a local file.
            let emitter = match self.link_mode {
                LinkMode::Inline => None,
                LinkMode::After(emitter) => Some(emitter),
                LinkMode::Both(has_short_url, emitter) => {
                    if has_short_url(self, dest_url) {
                        Some(emitter)
                    } else {
                        None
                    }
                }
            };
            if let Some(emitter) = emitter {
                trace!("end link {dest_url} via transform");
                out!(f, "{}", emitter(self, f, dest_url));
            } else {
                trace!("end link {dest_url} quietly");
                out!(f, "]");
            }
        }
    }

    /// Transform a URL into AsciiDoc text to be inserted after the link source.
    fn link_after(&self, _f: &AsciiDocOutput, url: &str) -> String {
        format!(" (link:++{url}++[_++{url}++_])")
    }

    /// Transform a URL into AsciiDoc text to be inserted after the link source, but
    /// only if the URL has a short equivalent.
    fn selective_shorten(&self, f: &AsciiDocOutput, url: &str) -> String {
        debug!("Attempt to shorten {url}");
        if let Some(short_url) = self.short_links.get(url) {
            debug!("Shortened {url} to {short_url}");
            if f.in_italics() {
                format!(" (link:++{short_url}++[{short_url}])")
            } else {
                format!(" (link:++{short_url}++[_{short_url}_])")
            }
        } else {
            "".to_string()
        }
    }

    /// Indicate whether there's a short version of the given URL available.
    fn has_short_url(&self, url: &str) -> bool {
        let result = self.short_links.get(url).is_some();
        trace!("Does {url} have a short form? {result}");
        result
    }

    /// Optionally map a filename.
    fn fsubst(&self, filename: &str) -> String {
        if let Some(new_name) = self.filename_subst.get(filename) {
            new_name.to_string()
        } else {
            filename.to_string()
        }
    }
}

/// Determine if a URL refers to a local file, and if so return the local path.
/// Rough and ready, doesn't cope with many edge cases.
fn url_is_local(url: &str) -> Option<String> {
    if let Ok(url) = url::Url::parse(url) {
        if url.scheme() == "file" {
            Some(url.path().to_string())
        } else {
            None
        }
    } else {
        // Did not parse as a URL.
        if url.starts_with('/') || url.ends_with('/') {
            // Leave absolute paths and directories alone.
            None
        } else if let Some(stripped) = url.strip_prefix("./") {
            // Strip leading dot-slash.
            Some(stripped.to_string())
        } else {
            Some(url.to_string())
        }
    }
}

struct Indent(usize);

impl Indent {
    fn new(start: usize) -> Self {
        Self(start)
    }
    fn inc(&mut self) {
        self.0 += 1
    }
    fn dec(&mut self) {
        assert!(self.0 > 0);
        self.0 -= 1
    }
}

impl std::fmt::Display for Indent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", "  ".repeat(self.0))
    }
}

/// Convert MarkDown text with no (MarkDown) special chars into AsciiDoc text with no AsciiDoc special chars.
fn md2ad(v: &str) -> String {
    // A '+' means something in AsciiDoc but not in MarkDown; use the HTML escape code instead.
    v.replace('+', "&#43;")
}

fn footnote_marker(v: &str) -> String {
    format!("TODO-footnote-text-{v}")
}

/// Convert a "U+1234" unicode code point description into a Unicode char.
fn transmute_unicode(text: &str) -> String {
    if let Some(caps) = UNICODE_CHAR_RE.captures(text) {
        let code = caps.name("code").unwrap();
        let code = u32::from_str_radix(code.into(), 16).unwrap();
        let code = char::from_u32(code).unwrap();
        let rest: &str = caps.name("rest").unwrap().into();
        format!("{code}{rest}")
    } else {
        text.to_string()
    }
}
