# Pomodoro Waybar Module

> An interactive pomodoro timer Waybar module in Haskell

[![CI](https://github.com/sgillespie/pomodoro-waybar-module-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/sgillespie/pomodoro-waybar-module-hs/actions/workflows/ci.yml)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Getting started

In order to use this project, you will need:

- [Nix](https://nixos.org/download.html)
- [direnv](https://direnv.net) (optional)

Clone the project:

```
git clone https://github.com/sgillespie/pomodoro-waybar-module-hs.git
cd pomodoro-waybar-module-hs
```

If using direnv, approve and load the environment:

```
direnv allow
```

Otherwise, enter the development shell:

```
nix develop
```

Then build it:

```
cabal build
```

The dev shell provides `cabal`, `haskell-language-server`, `hoogle` and other dev tools.

## Workflow

Common build tasks are collected in the [`justfile`](https://github.com/casey/just).
To view them, run the default recipe:

```
just

Available recipes:
    default     # Show available recipes
    build       # Build the executable
    run *args   # Run the executable (`just run -- --help`)
    dist        # Build release artifacts
    lint        # Run the static analyzers (statix, deadnix, hlint)
    fmt         # Format the source tree in place
    fmt-check   # Check formatting without writing changes
    test        # Run the test suite
```

You can also build and run directly with `cabal` from inside the dev shell.

## Project layout

```
app/                          Executable entry point (Main.hs)
src/                          Library: Pomodoro.Waybar modules
test/                         Test suite
nix/                          Flake modules (build, checks, dist, formatter)
justfile                      Task runner recipes
flake.nix                     Flake entry point
```

## License

[MIT](LICENSE)
