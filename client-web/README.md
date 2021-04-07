# messaging-client-web

Uses GHCJS and Miso to generate a JavaScript client. CSS is done using [Bulma](https://bulma.io/).

### Build instructions

The easiest way to use GHCJS is probably using the [Nix package manager](https://nixos.org/).

Optionally you can use a binary cache to avoid building GHCJS from source:

```sh
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use miso-haskell
```

To build the web app directly, you can run the following:

```sh
$ # remove cabal files potentially generated using newer GHC
$ rm ../*/*.cabal
$ nix-build
$ open ./result/bin/messaging-client-web-exe.jsexe/index.html
```

For a more interactive development flow of the web client (assuming the client-core package aleady builds),
it can be nicer to first run `nix-shell` and then inside of it:

```sh
$ find -name "*.hs" | entr -s 'cabal build --ghcjs'
```

For more information, see <https://github.com/dmjio/miso/> or <https://github.com/serras/miso-start-template/>.
