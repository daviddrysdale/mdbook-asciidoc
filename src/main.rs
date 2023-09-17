//! mdbook backend for AsciiDoc generation

use log::{debug, error, info, trace};
use mdbook::{
    book::{BookItem, Chapter},
    renderer::RenderContext,
};
use pulldown_cmark as md;
use std::io::Write;

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
        "created AsciiDoc backend, outputs to '{}', unified:{}",
        backend.dest_dir.display(),
        backend.unified
    );

    backend.process(ctx)
}

/// AsciiDoc backend processor.
struct AsciiDocBackend {
    /// Where to put output.
    pub dest_dir: std::path::PathBuf,
    /// Whether to emit a single AsciiDoc file as output.
    #[allow(dead_code)] // TODO: implement or drop
    pub unified: bool,
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

struct Output {
    f: std::fs::File,
    col: usize,
}

// Macros for output with tracing.
macro_rules! out {
    ($f:ident, $($arg:tt)+) => { {
        let output = format!("{}", format_args!($($arg)+));
        trace!("[AD] emit: '{}'", output);
        write!($f.f, "{}", output).expect("failed to write output!");
        $f.col += output.len();
    } }
}

macro_rules! outln {
    ($f:ident, $($arg:tt)+) => { {
        let output = format!("{}", format_args!($($arg)+));
        trace!("[AD] emit: '{}\\n'", output);
        write!($f.f, "{}\n", output).expect("failed to write output!");
        $f.col = 0;
    } }
}

macro_rules! crlf {
    ($f:ident) => {{
        trace!("[AD] emit crlf: '\\n'");
        writeln!($f.f, "").expect("failed to write output!");
        $f.col = 0;
    }};
}

macro_rules! cr {
    ($f:ident) => {{
        if $f.col > 0 {
            trace!("[AD] cr needed so emit: '\\n'");
            writeln!($f.f, "").expect("failed to write output!");
            $f.col = 0;
        } else {
            trace!("[AD] cr but at col 0 already");
        }
    }};
}

impl AsciiDocBackend {
    /// Create a new backend, with options populated from the given context.
    pub fn new(ctx: &RenderContext) -> Result<Self, Error> {
        let dest_dir = ctx.destination.clone();
        let unified = match ctx.config.get("unified") {
            Some(toml::Value::Boolean(b)) => *b,
            None => false,
            v => {
                return Err(Error::General(format!(
                    "Unexpected value {:?} for boolean",
                    v
                )))
            }
        };

        std::fs::create_dir_all(&dest_dir).map_err(|e| {
            format!(
                "Failed to create output directory '{}': {:?}",
                dest_dir.display(),
                e
            )
        })?;

        Ok(Self { dest_dir, unified })
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
        let topfilename = self.dest_dir.join("book.adoc");
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
                    let (filename, offset) = self.process_chapter(ch)?;
                    writeln!(topfile, "include::{filename}[leveloffset={offset}]")?;
                }
                BookItem::Separator => debug!("Visit separator"),
                BookItem::PartTitle(title) => debug!("Visit part '{title}'"),
            }
        }

        Ok(())
    }

    /// Process a single chapter.
    fn process_chapter<'a>(&mut self, ch: &Chapter) -> Result<(String, usize), Error> {
        use md::{Event, Tag};
        debug!("Visit chapter '{}' from file '{:?}'", ch.name, ch.path);
        let offset = ch.parent_names.len();

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
        let filename = filename + ".adoc";
        debug!("basename = {filename}");

        // Create the corresponding file in the output directory.
        let outfilename = self.dest_dir.join(filename.clone());
        let dest_dir = std::path::Path::new(&outfilename).parent().unwrap();
        debug!("mkdir -p {dest_dir:?}");
        std::fs::create_dir_all(&dest_dir).map_err(|e| {
            format!(
                "Failed to create output directory '{}': {:?}",
                dest_dir.display(),
                e
            )
        })?;
        debug!("output to {outfilename:?}");
        let f = std::fs::File::create(&outfilename).map_err(|e| {
            format!(
                "Failed to create output file '{}': {:?}",
                outfilename.display(),
                e
            )
        })?;
        let mut f = Output { f, col: 0 };

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
        for event in parser {
            match &event {
                Event::Start(tag) => {
                    trace!("[MD]{indent}Start({tag:?})");
                    match tag {
                        Tag::Paragraph => { /* para */ }
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
                            out!(f, "{} ", "=".repeat(level + offset));
                        }
                        Tag::BlockQuote => {
                            cr!(f);
                            outln!(f, "[quote]");
                        }
                        Tag::CodeBlock(kind) => {
                            cr!(f);
                            out!(f, "[source");
                            if let md::CodeBlockKind::Fenced(lang) = kind {
                                out!(f, ",{lang}");
                            }
                            outln!(f, "]");
                            outln!(f, "----");
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
                        Tag::FootnoteDefinition(_text) => { /* footnote */ }

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
                            out!(f, "{dest_url}[");
                        }
                        Tag::Image(_link_type, _dest_url, _title) => { /* image */ }
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
                        }
                        Tag::BlockQuote => {}
                        Tag::List(_first_num) => {
                            lists.pop().expect("leaving a list when not in one!");
                        }
                        Tag::Item => {
                            crlf!(f);
                        }
                        Tag::FootnoteDefinition(_text) => { /* footnote */ }

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
                        Tag::Image(_link_type, _dest_url, _title) => { /* image */ }
                    }
                }
                Event::Text(text) => {
                    trace!("[MD]{indent}Text({text})");
                    indent.inc();

                    out!(f, "{}", text);

                    indent.dec();
                }
                Event::Code(text) => {
                    trace!("[MD]{indent}Code({text})");
                    // Backtick induces fixed-width font, plus sign prevents interpretation of characters.
                    out!(f, "`+{text}+`");
                }
                Event::Html(text) => {
                    trace!("[MD]{indent}Html({text})");
                    let html = text.to_string();
                    match html.trim() {
                        "<br/>" => outln!(f, "&nbsp;"),
                        "<hr/>" => outln!(f, "'''"),
                        _ => error!("Unhandled HTML: {html}"),
                    }
                }
                Event::FootnoteReference(text) => {
                    trace!("[MD]{indent}FootnoteRef({text})");
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
        Ok((filename, offset))
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
