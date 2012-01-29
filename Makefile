all: build
	cabal build
configure:
	cabal --enable-tests configure
build:
	cabal build
tests: build
	dist/build/tests/tests
run: build
	dist/build/memento/memento
clean:
	cabal clean
