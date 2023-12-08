# Setup

Standard installation of dependencies with `opam`:

```bash
opam pin add . --deps-only
```

For `nix` (and especially `NixOS`) users, it is recommended to use the provided flake:

```bash
nix develop
```

# Build

```bash
dune build
``` 

# Usage

For testing a set of formulas expressed in the syntax of Actema (see
https://www.actema.xyz/docs/syntax), you can use the `test/check.exe` program.
For instance:

```bash
dune exec test/check.exe < test/edukera.tautos
```

For playing with the ILTP dataset, you can use the `test/iltp/check.exe`
program. For instance:

```bash
dune exec test/iltp/check.exe -- --domain SYJ --problem 105+1.002 --phases --justif                                                   
```

The general command-line syntax is:
    
```bash
check --domain <domain> [--problem <identifier>] [--classical] [--phases] [--justif]
```

where:

- `--domain <domain>` specifies the name of the domain, which can be any folder
  inside `test/iltp/ILTP-v1.1.2-propositional/Problems`, i.e. `SYN`
- `--problem <identifier>` specifies the problem identifier for the specific
  domain, i.e. `041+1` for the problem file
  `test/iltp/ILTP-v1.1.2-propositional/Problems/SYN/SYN-041+1.p`
- `--classical` enables the experimental mode of the algorithm that accounts for
  *classical* justifications, i.e. doubly-negated links between atoms
- `--phases` enables logging of the intermediate goal after each phase
  (**pollination**, **reproduction** and **decomposition**)
- `--justif` enables logging of the sources and targets of each justification
  that is selected during pollination