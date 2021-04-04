let
  pkgs = import <nixpkgs> {};
  haskellPackages = import ./haskellPackages.nix;
  dev = haskellPackages.developPackage {
    # cf. https://nix.dev/anti-patterns/language.html#reproducability-referencing-top-level-directory-with
    root = builtins.path {
      path = ./.;
      name = "messaging-client-web";
    };
    name = "messaging-client-web";
  };
in
pkgs.mkShell {
  # this adds all the build inputs of your project package
  inputsFrom = [ dev ];
  # now add your other dependencies
  buildInputs = with pkgs; [ hpack cabal-install entr ];
  # build a fresh cabal file
  shellHook = ''
    rm -f messaging-client-web.cabal
    hpack
  '';
}
