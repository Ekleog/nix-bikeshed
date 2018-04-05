with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "nix-bikeshed";
  buildInputs = [ stack ];
}
