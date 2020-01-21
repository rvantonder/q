all:
	dune build ./src/main.bc.js --profile release

minimize:
	./node_modules/uglify-js/bin/uglifyjs _build/default/src/main.bc.js > parser.min.js

install:
	dune install

test:
	dune runtest

clean:
	dune clean

uninstall:
	dune uninstall

promote:
	dune promote

.PHONY: all install test clean uninstall

