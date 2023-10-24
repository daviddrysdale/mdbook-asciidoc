//! Unit tests.

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
