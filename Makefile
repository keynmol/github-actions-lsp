all: lsp 

clean:
	rm -rf bin/*
	scala-cli clean .

bin/lsp:
	mkdir -p bin
	scala-cli package . -M LSP -f -o bin/github-actions-lsp

lsp: bin/lsp 

