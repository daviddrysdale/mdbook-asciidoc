# Command to open local webpage in browser
PAGEOPEN=open

# Dependencies
sample/expected/book.html: sample/expected/partA.adoc sample/expected/chapter1.adoc sample/expected/chapter2.adoc sample/expected/partB.adoc sample/expected/chapter3.adoc sample/expected/chapter4.adoc
sample/book/asciidoc/book.adoc: sample/expected/partA.md sample/expected/chapter1.md sample/expected/chapter2.md sample/expected/partB.md sample/expected/chapter3.md sample/expected/chapter4.md

all:

install: ${HOME}/.cargo/bin/mdbook-asciidoc

${HOME}/.cargo/bin/mdbook-asciidoc: src/main.rs
	cargo install --offline --path .

sample/book/asciidoc/book.adoc: install
	(cd sample && mdbook build)

%/book.html: %/book.adoc
	asciidoctor -o $@ $<

compare_adoc: sample/book/asciidoc/book.adoc
	echo "Comparing AsciiDoc output with expected"
	diff sample/expected/book.adoc sample/book/asciidoc/book.adoc

update_expected_html: sample/expected/book.html
validate: sample/book/asciidoc/book.html

trace: sample/trace.out
sample/trace.out: install
	(export RUST_LOG=mdbook_asciidoc=trace && cd sample && mdbook build) > sample/trace.out 2>&1

test: compare_adoc validate

compare_html: sample/book/asciidoc/book.html
	${PAGEOPEN} sample/book/html/index.html # As produced by mdBook -> HTML
	${PAGEOPEN} sample/book/asciidoc/book.html # As produced by mdBook -> asciiDoc -> HTML

show_expected_html: sample/expected/book.html
	${PAGEOPEN} $<
