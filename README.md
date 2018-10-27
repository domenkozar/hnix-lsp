# hnix-lsp

Using [Haskell Nix parser and evaluator](https://github.com/haskell-nix/hnix)
implement [LSP](https://langserver.org/) support for all the editors.

# Features

- [x] Formatting with error reporting
- [ ] Evaluating
- [ ] ???

# Installation

Make sure `hnix-lsp` is available on your `$PATH`:

    $ git clone https://github.com/domenkozar/hnix-lsp.git
    $ cd hnix-lsp
    $ nix-env -if .

## Atom

See https://github.com/domenkozar/ide-nix

## Your editor

Please contribute, usually it's a very small package that just needs to start
`hnix-lsp` executable.
