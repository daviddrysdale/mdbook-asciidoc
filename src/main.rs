//! mdbook backend for AsciiDoc generation

use lazy_static::lazy_static;
use log::{debug, error, info, trace, warn};
use mdbook::{
    book::{BookItem, Chapter},
    renderer::RenderContext,
};
use pulldown_cmark as md;
use regex::Regex;
use std::io::Write;
use std::path::{Path, PathBuf};

lazy_static! {
    static ref ASCIIDOC_IMAGE_RE: Regex = Regex::new(r"image::?(?P<url>[a-zA-Z0-9_/.]+)").unwrap();
    static ref ASCIIDOC_ESCAPE_RE: Regex =
        Regex::new(r#"<asciidoc content='(?P<content>[^']+)'"#).unwrap();
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

#[derive(Clone, Default)]
struct AsciiDocOutput {
    /// Line-by-line content.
    prev_lines: Vec<String>,
    /// Current line being accumulated.
    cur_line: String,
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
            let newline = line.replace(from, &to);
            let _ = std::mem::replace(line, newline);
        }
        self.cur_line = self.cur_line.replace(from, &to);
    }

    fn write_to<T: std::io::Write>(&self, out: &mut T) -> Result<(), Error> {
        for line in &self.prev_lines {
            writeln!(out, "{}", line)?;
        }
        writeln!(out, "{}", self.cur_line)?;
        Ok(())
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

        let skip_chapters =
            if let Some(toml::Value::String(v)) = ctx.config.get("output.asciidoc.skip-chapters") {
                v
            } else {
                ""
            }
            .split(",")
            .map(|s| s.to_owned())
            .collect();

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
            writeln!(topfile, "")?;
        }
        writeln!(topfile, ":doctype: book")?;
        writeln!(topfile, "")?;

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
                    writeln!(topfile, "include::{filename}[leveloffset={offset:+}]")?;
                }
                BookItem::Separator => debug!("Visit separator"),
                BookItem::PartTitle(title) => debug!("Visit part '{title}'"),
            }
        }

        Ok(())
    }

    /// Process a single chapter.
    fn process_chapter<'a>(&mut self, ch: &Chapter) -> Result<(String, isize), Error> {
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
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        enum List {
            Unnumbered,
            Numbered,
        }
        let mut lists = Vec::new();
        let mut swapped_f = None;
        let mut escaping_needed = true;
        if let Some(filename) = &ch.path {
            outln!(
                f,
                "[#file_{}]",
                filename.display().to_string().replace(".", "_")
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
                            cr!(f);
                            maybelf!(f);
                            if ch.name == "Preface" && level == 1 {
                                // Assume a chapter called "Preface" should map to an AsciiDoc preface.
                                outln!(f, "[preface]");
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
                            out!(f, "[source");
                            if let md::CodeBlockKind::Fenced(lang) = kind {
                                out!(f, ",{lang}");
                            }
                            outln!(f, "]");
                            outln!(f, "----");
                            escaping_needed = false;
                        }
                        Tag::List(first_num) => {
                            lists.push(if first_num.is_some() {
                                List::Numbered
                            } else {
                                List::Unnumbered
                            });
                            // TODO: cope with ordered lists not starting at 1
                        }
                        Tag::Item => {
                            let indent = lists.len();
                            assert!(indent > 0);
                            let lead = match lists[indent - 1] {
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
                            out!(f, "| ");
                        }

                        // Inline elements
                        Tag::Emphasis => {
                            out!(f, "_");
                        }
                        Tag::Strong => {
                            out!(f, "*");
                        }
                        Tag::Strikethrough => {
                            out!(f, "[line-through]#");
                        }
                        Tag::Link(_link_type, dest_url, _title) => {
                            let mut dest_url = dest_url.to_string();
                            let mut mac = "link";
                            // Generally want to surround the URL with ++ to prevent interpretation
                            let mut esc = "++";
                            if let Some(local_file) = url_is_local(&dest_url) {
                                if local_file.ends_with(".md") {
                                    debug!(
                                        "transform target '{local_file}' of link into local xref"
                                    );
                                    dest_url = format!(
                                        "file_{}",
                                        local_file.to_string().replace(".", "_")
                                    );
                                    mac = "xref";
                                    // Putting ++ around xref doesn't appear to work.
                                    esc = "";
                                }
                            }
                            out!(f, "{mac}:{esc}{dest_url}{esc}[");
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
                        Tag::Paragraph | Tag::Heading(_, _, _) => {
                            cr!(f); // End the current in-progress line.
                            crlf!(f); // Additional blank line.
                        }
                        Tag::CodeBlock(_kind) => {
                            outln!(f, "----");
                            crlf!(f);
                            escaping_needed = true;
                        }
                        Tag::BlockQuote => {
                            cr!(f);
                            outln!(f, "____");
                        }
                        Tag::List(_first_num) => {
                            lists.pop().expect("leaving a list when not in one!");
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
                        Tag::TableCell => {}

                        // Inline elements
                        Tag::Emphasis => {
                            out!(f, "_");
                        }
                        Tag::Strong => {
                            out!(f, "*");
                        }
                        Tag::Strikethrough => {
                            out!(f, "#");
                        }
                        Tag::Link(_link_type, _dest_url, _title) => {
                            out!(f, "]");
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

                    if self.allow_asciidoc {
                        self.process_potential_asciidoc(text);
                    }

                    if escaping_needed {
                        out!(f, "{}", md2ad(text));
                    } else {
                        out!(f, "{}", text);
                    }

                    indent.dec();
                }
                Event::Code(text) => {
                    trace!("[MD]{indent}Code({text})");
                    // - Backtick induces fixed-width font.
                    // - Double-backtick allows use in positions without surrounding space.
                    // - Plus sign prevents interpretation of characters.
                    out!(f, "``+{text}+``");
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
                            _ => warn!("Unhandled HTML: {html}"),
                        }
                    }
                }
                Event::FootnoteReference(text) => {
                    trace!("[MD]{indent}FootnoteRef({text})");
                    out!(f, "footnote:[{}]", footnote_marker(text));
                }
                Event::SoftBreak => {
                    trace!("[MD]{indent}SoftBreak");
                    crlf!(f);
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
                ch.name.to_ascii_lowercase().replace(" ", "_")
            });
        let filename = filename + ".asciidoc";
        debug!("basename = {filename}");

        // Create the corresponding file in the output directory.
        let outfilename = self.dest_dir.join(filename.clone());
        let dest_dir = Path::new(&outfilename).parent().unwrap();
        debug!("mkdir -p {dest_dir:?}");
        std::fs::create_dir_all(&dest_dir).map_err(|e| {
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
        }
        Ok(())
    }

    /// Process raw text from the input that might already contain some
    /// AsciiDoc.
    fn process_potential_asciidoc(&self, text: &str) {
        if let Some(caps) = ASCIIDOC_IMAGE_RE.captures(&text) {
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
        if url.starts_with("/") {
            // Leave absolute paths alone.
            None
        } else if url.ends_with("/") {
            // Leave directories alone.
            None
        } else if url.starts_with("./") {
            // Strip leading dot-slash.
            Some(url[2..].to_string())
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
    v.replace("+", "&#43;")
}

fn footnote_marker(v: &str) -> String {
    format!("TODO-footnote-text-{v}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_url_is_local() {
        let tests = [
            ("src/file", Some("src/file")),
            ("./src/file", Some("src/file")),
            ("src/subdir/file", Some("src/subdir/file")),
            ("src/subdir/", None),
            ("/root/subdir/file", None),
            ("http://example.com/file", None),
        ];
        for (input, want) in tests {
            let want = want.map(|s| s.to_string());
            let got = url_is_local(input);
            assert_eq!(got, want, "Failed for input '{input}'");
        }
    }

    #[test]
    fn test_image_re() {
        let tests = [
            ("blah", None),
            ("image:: mentioned", None),
            (
                r#"image:images/does_not_compile.svg["Red cross",100,]"#,
                Some("images/does_not_compile.svg"),
            ),
            (
                r#"prefix then image:images/does_not_compile.svg["Red cross",100,]"#,
                Some("images/does_not_compile.svg"),
            ),
            ("image::images/draw.svg", Some("images/draw.svg")),
            (
                r#"prefix then image:images/does_not_compile.svg["Red cross",100,]"#,
                Some("images/does_not_compile.svg"),
            ),
            (
                "prefix then image::images/draw.svg",
                Some("images/draw.svg"),
            ),
        ];
        for (input, want) in tests {
            let got = if let Some(caps) = ASCIIDOC_IMAGE_RE.captures(&input) {
                if let Some(url) = caps.name("url") {
                    let url: &str = url.into();
                    Some(url)
                } else {
                    None
                }
            } else {
                None
            };
            assert_eq!(got, want, "Failed for input '{input}'");
        }
    }
}
