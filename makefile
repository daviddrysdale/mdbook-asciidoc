# Command to open local webpage in browser
PAGEOPEN=open

# Dependencies
SAMPLE_FILES=partA chapter1 chapter2 partB chapter3 subdir/chapter4

all: sample/book/asciidoc/book.adoc

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

compare: sample/book/asciidoc/book.adoc
	meld sample/expected sample/book/asciidoc

compare_html: sample/book/asciidoc/book.html
	${PAGEOPEN} sample/book/html/index.html # As produced by mdBook -> HTML
	${PAGEOPEN} sample/book/asciidoc/book.html # As produced by mdBook -> asciiDoc -> HTML

show_expected_html: sample/expected/book.html
	${PAGEOPEN} $<
show_generated_html: sample/book/asciidoc/book.html
	${PAGEOPEN} $<

sample/expected/book.html: $(addprefix sample/expected/,$(addsuffix .adoc,$(SAMPLE_FILES)))
sample/book/asciidoc/book.adoc: $(addprefix sample/src/,$(addsuffix .md,$(SAMPLE_FILES)))
