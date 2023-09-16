all:

install: ${HOME}/.cargo/bin/mdbook-asciidoc

${HOME}/.cargo/bin/mdbook-asciidoc: src/main.rs
	cargo install --offline --path .

sample/book/asciidoc/book.adoc: install
	(cd sample && mdbook build)

compare: sample/book/asciidoc/book.adoc
	echo "Comparing AsciiDoc output with expected"
	diff sample/expected/book.adoc sample/book/asciidoc/book.adoc

validate: # TODO

trace: sample/trace.out
sample/trace.out: install
	(export RUST_LOG=mdbook_asciidoc=trace && cd sample && mdbook build) > sample/trace.out 2>&1

test: compare validate
