# README

This package provides an interface for the [`haskell-terminal` library](https://github.com/lpeterse/haskell-terminal), enabling you to write `terminal` applications as signal functions.

It consists of a `TerminalEventClock` which provides terminal events, a `flowTerminal` allowing you to run `Rhine`s which can receive terminal events and display to a terminal, as well as a `terminalConcurrently` schedule to coordinate multiple `Rhine`s.

It also probides a simple example program,
which you can run as `cabal run rhine-terminal-simple`
or `nix build .#rhine-terminal && result/bin/rhine-terminal-simple`.
