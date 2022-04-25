# --------------------------------------------------------------------
.PHONY: all run test

DUNEOPTS := --display=short

# --------------------------------------------------------------------
all: run

test:
	@dune exec test/prover.exe

run: bin/main.exe
	@dune exec $< $(OPTS)

bin/main.exe:
	@dune build $(OPTS)
