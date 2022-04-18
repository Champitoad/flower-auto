# --------------------------------------------------------------------
.PHONY: all run

DUNEOPTS := --display=short

# --------------------------------------------------------------------
all: run

run: bin/main.exe
	@dune exec $< $(OPTS)

bin/main.exe:
	@dune build $(OPTS)
