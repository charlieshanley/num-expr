{
  description = "TODO name";

  inputs.nixpkgs.url = "nixpkgs/nixos-23.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      inherit (pkgs) haskellPackages;
    in
      {
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.cabal-install pkgs.cabal2nix haskellPackages.ghc pkgs.ghcid
            haskellPackages.hlint pkgs.stylish-haskell
          ];
        };
      });
}
