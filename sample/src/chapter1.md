# Title of First Chapter

This is a paragraph of text, including **bold** and *italic* text. Also ~~strikethrough~~.

This includes `code + 1` identifiers, and some "quoted text" (including 'single quotes').

This is an inline URL: <https://lurklurk.org>

This is [link](https://github.com/daviddrysdale/mdbook-docbook).

> This is a block quote, including
> multiple lines.

A paragraph that includes HTML characters &ndash; like an en-dash.

As a result, when writing code that accepts closures, **use the most general `Fn*` trait that works**, to allow ...

Scott Meyers' original _Effective C++_ series.  The C++ language was (and is) full of footguns, so _Effective
C++_ focused on a collection of advice.

```rust
fn main() {
  x += 1;
}
```

Now for some bullets:

- top-level first
    - second-level 1
    - second-level 2
        - third level
- top-level second
  1) sub-numbered bullet 1
  2) sub-numbered bullet 2
- top-level third

  with follow-on para
- top-level fourth.

A MarkDown table.

| Heading 1| Heading 2 | Heading 3| Hdr 4|
|----------|:----------|:--------:|-----:|
| normal | left-align| center| right|
| `code` | [link](http://example.com) | | left *blank*|

List with checkboxes:

- [ ] Do something.
- [x] Do something else.
