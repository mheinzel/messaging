# ansi-terminal-declarative-simple

ANSI-terminal-based declarative UI library. Limited features, just based on what we needed for a simple application.

Takes inspiration from `gi-gtk-declarative-app-simple`, `brick` and `patat`.

### Compatibility

The package only relies on support for ANSI terminal escape codes, which is widespread by now
(including on Windows).

However, on Windows the keyboard input does currently does not work nicely,
as line buffering cannot be disabled: https://gitlab.haskell.org/ghc/ghc/-/issues/2189

It seems like this will be fixed with the new Windows I/O manager.
There is [experimental support for it](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1224)
in [GHC 9.0.1](https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/9.0.1-notes.html),
but it does not yet support pipes and sockets and cannot be used together with the existing manager.
The situation should improve with GHC 9.2, find [more information in this talk](https://www.youtube.com/watch?v=kgNh5mdZ1xw).

In addition, this package only detects special key presses by reading the ANSI escape codes.
Terminals that don't send these (e.g. most choices on Windows) will only receive printable characters
(including enter/newline).
If you want your program to be usable on Windows, make sure your interface can be used without special
keys, e.g. by additionally providing text commands.
