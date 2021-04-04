let
  haskellPackages = import ./haskellPackages.nix;
in
haskellPackages.callCabal2nix "messaging-client-web" ./. {}
