# rebar32nix

Translates a rebar.lock into a Nix derivation

## Build

``` console
$ rebar3 escriptize
```

or

``` console
$ nix-build
```

## Run

``` console
$ _build/default/bin/rebar32nix
```

or if built with Nix

``` console
$ ./result/bin/rebar32nix https://github.com/my-repository/test -o drv.nix
```

To get the list of supported flags:

``` console
$ ./path/to/rebar32nix --help
Usage: rebar32nix [-h] [-v] [--release-type [<release_type>]]
                  [--builder [<builder>]] [-o <out_file>] [<file>]

  -h, --help      Print this help.
  -v, --version   Show version information.
  --release-type  Generate either a release or an escript. [default: 
                  release]
  --builder       Derivation builder to use [default: rebar3Relx]
  -o, --out       Output file
  <file>          Input file or a valid git URL
```

## Development

**Format code in-place**

``` console
$ rebar3 get-deps
$ rebar3 fmt --write
```
