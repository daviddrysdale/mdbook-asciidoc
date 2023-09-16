//! mdbook backend for AsciiDoc generation

use log::{debug, error, info, trace};
use mdbook::{book::BookItem, renderer::RenderContext};
use pulldown_cmark as md;

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
    outfile: std::fs::File,
    /// Whether to emit a single AsciiDoc file as output.
    #[allow(dead_code)] // TODO: implement or drop
    pub unified: bool,
}

/// Local error type.
#[derive(Debug)]
enum Error {
    General(String),
}

impl From<String> for Error {
    fn from(e: String) -> Error {
        Self::General(e)
    }
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
        let outfilename = dest_dir.join("book.adoc");
        let outfile = std::fs::File::create(&outfilename).map_err(|e| {
            format!(
                "Failed to create output file '{}': {:?}",
                outfilename.display(),
                e
            )
        })?;

        Ok(Self {
            dest_dir,
            outfile,
            unified,
        })
    }

    /// Process the AsciiDoc document.
    pub fn process(&mut self, ctx: RenderContext) -> Result<(), Error> {
        self.build(ctx)?;
        self.write()
    }

    /// Write out the generated AsciiDoc.
    fn write(&mut self) -> Result<(), Error> {
        // TODO
        Ok(())
    }

    /// Build the AsciiDoc.
    fn build(&mut self, ctx: RenderContext) -> Result<(), Error> {
        for item in ctx.book.iter() {
            match &item {
                BookItem::Chapter(ch) => {
                    use md::{Event, Tag};
                    let mut indent = "  ".to_string();
                    debug!("Visit chapter '{}'", ch.name);

                    let parser = md::Parser::new_ext(
                        &ch.content,
                        md::Options::ENABLE_TABLES
                            | md::Options::ENABLE_FOOTNOTES
                            | md::Options::ENABLE_STRIKETHROUGH
                            | md::Options::ENABLE_TASKLISTS
                            | md::Options::ENABLE_HEADING_ATTRIBUTES,
                    );
                    for event in parser {
                        match &event {
                            Event::Start(tag) => {
                                trace!("[MD]{indent}Start({tag:?})");
                                match tag {
                                    Tag::Paragraph => { /* para */ }
                                    Tag::Heading(level, _frag_id, _classes) => {
                                        let _extra = match level {
                                            md::HeadingLevel::H1 => None,
                                            md::HeadingLevel::H2 => Some("sect1"),
                                            md::HeadingLevel::H3 => Some("sect2"),
                                            md::HeadingLevel::H4 => Some("sect3"),
                                            md::HeadingLevel::H5 => Some("sect4"),
                                            md::HeadingLevel::H6 => Some("sect5"),
                                        };
                                        /* title */
                                    }
                                    Tag::BlockQuote => { /* blockquote */ }
                                    Tag::CodeBlock(kind) => {
                                        /* programlisting */
                                        if let md::CodeBlockKind::Fenced(_lang) = kind {
                                            /* with lang */
                                        }
                                    }
                                    Tag::List(first_num) => {
                                        if let Some(_first_num) = first_num {
                                            // TODO: cope with ordered lists not starting at 1
                                            /* orderedlist */
                                        } else {
                                            /* itemizedlist */
                                        }
                                    }
                                    Tag::Item => { /* listitem */ }
                                    Tag::FootnoteDefinition(_text) => { /* footnote */ }

                                    // Table elements
                                    Tag::Table(aligns) => {
                                        /* tgroup */
                                        for align in aligns {
                                            let _colspec = match align {
                                                md::Alignment::None => "none",
                                                md::Alignment::Left => "left",
                                                md::Alignment::Center => "center",
                                                md::Alignment::Right => "right",
                                            };
                                        }
                                    }
                                    Tag::TableHead => { /* row */ }
                                    Tag::TableRow => { /* row */ }
                                    Tag::TableCell => { /* entry */ }

                                    // Inline elements
                                    Tag::Emphasis => { /* emphasis */ }
                                    Tag::Strong => { /* emphasis_bold */ }
                                    Tag::Strikethrough => { /* emphasis_strikethrough */ }
                                    Tag::Link(_link_type, _dest_url, _title) => { /* link */ }
                                    Tag::Image(_link_type, _dest_url, _title) => { /* image */ }
                                }
                                indent += "  ";
                            }
                            Event::End(tag) => {
                                indent = indent[..indent.len() - 2].to_string();
                                trace!("[MD]{indent}End({tag:?})");
                            }
                            Event::Text(text) => {
                                trace!("[MD]{indent}Text({text})");
                                indent += "  ";

                                // TODO: insert text

                                indent = indent[..indent.len() - 2].to_string();
                            }
                            Event::Code(text) => {
                                trace!("[MD]{indent}Code({text})");
                                /* code: text */
                            }
                            Event::Html(text) => {
                                trace!("[MD]{indent}Html({text})");
                            }
                            Event::FootnoteReference(text) => {
                                trace!("[MD]{indent}FootnoteRef({text})");
                            }
                            Event::SoftBreak => {
                                // TODO
                                trace!("[MD]{indent}SoftBreak");
                            }
                            Event::HardBreak => {
                                trace!("[MD]{indent}HardBreak");
                            }
                            Event::Rule => {
                                trace!("[MD]{indent}Rule");
                            }
                            Event::TaskListMarker(done) => {
                                trace!("[MD]{indent}TaskListMarker({done})");
                                let _marker = if *done { "☑" } else { "☐" };
                            }
                        }
                    }
                }
                BookItem::Separator => debug!("Visit separator"),
                BookItem::PartTitle(title) => debug!("Visit part '{title}'"),
            }
        }

        Ok(())
    }
}
