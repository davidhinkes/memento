all: build
	cabal build
configure:
	cabal configure
build: configure
	cabal build
run: build
	dist/build/memento/memento
clean:
	cabal clean
