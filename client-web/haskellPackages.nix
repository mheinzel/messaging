let
  # Pin the Miso repository to a particular commit, import the expression, and
  # retrieve the nixpkgs package set it provides.
  #
  # Last updated: 4 September 2020
  pkgs = (
    import (
      builtins.fetchTarball {
        url = "https://github.com/dmjio/miso/archive/bb230192164f0532660aadb4175460740abfa2a2.tar.gz";
        sha256 = "0q44lxzz8pp89ccaiw3iwczha8x2rxjwmgzkxj8cxm97ymsm0diy";
      }
    ) {}
  ).pkgs;
in
# Construct a complete Haskell package set by overlaying the base package set
# from nixpkgs with various packages from external sources.
pkgs.haskell.packages.ghcjs.override (
  oldArgs: {
    # Ensure that we have an up-to-date Hackage snapshot.
    #
    # Last updated: 4 September 2020.
    all-cabal-hashes = pkgs.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/117622c10bf41f70548af023366ad82eab9835e3.tar.gz";
      sha256 = "15zpi7x1iqkjk4dscm0z9xxshl58nmdi3sxgn8w3x86wbz03k1wv";
    };

    # Override/extend the base package set.
    overrides = pkgs.lib.composeExtensions (oldArgs.overrides or (_: _: {})) (
      hself: hsuper: {
        messaging-client-core = pkgs.haskell.lib.dontCheck (
          hself.callCabal2nix "messaging-client-core" ../client-core {}
        );
        messaging-shared = pkgs.haskell.lib.dontCheck (
          hself.callCabal2nix "messaging-shared" ../shared {}
        );
      }
    );
  }
)
