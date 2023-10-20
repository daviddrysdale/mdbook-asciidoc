# mdbook-asciidoc

A backend for [mdbook](https://github.com/rust-lang/mdBook) that outputs
[AsciiDoc](https://docs.asciidoctor.org/asciidoc/latest/).

## Installation

To use, install the tool

```sh
cargo install mdbook-asciidoc
```

and add it as a backend in `book.toml`:

```toml
[preprocessor.asciidoc]
```

## Configuration

The following configuration values can be set in `book.toml`.

| Name             | Type    |Default| Description                                                                  |
|------------------|---------|-------|------------------------------------------------------------------------------|
| `allow-asciidoc` | boolean | false | Cope with some AsciiDoc constructs included in the Markdown source when set. |
| `heading-offset` | integer | 0     | Extra offset to apply to heading levels.                                     |
| `skip-chapters`  | string  | ""    | Comma-separated list of filenames to ignore when generating AsciiDoc.        |
