# messaging-client-terminal

An ANSI terminal UI client.

On Windows it’s only usable with the Windows Subsystem for Linux (WSL) due to
[problems with the GHC runtime system](https://gitlab.haskell.org/ghc/ghc/-/issues/2189)
(expected to be fixed with the new Windows I/O manager coming in GHC 9.2).
See <../ansi-terminal-declarative-simple/README.md> for more details on compatibility.

### Build instructions

```
stack build messaging-client-gtk
stack run messaging-client-terminal-exe -- --help
```
