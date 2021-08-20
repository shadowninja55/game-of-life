{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, utils }: 
    utils.lib.eachDefaultSystem (system: 
      let
        pkgs = nixpkgs.legacyPackages."${system}";
        hpkgs = pkgs.haskellPackages;
      in rec {
        defaultPackage = hpkgs.callCabal2nix "game-of-life" ./. { };
        defaultApp = utils.lib.mkApp {
          drv = defaultPackage;
        };
        devShell = pkgs.mkShell { 
          buildInputs = with pkgs; [
            ghc
            cabal-install
          ];
        };
      }
    );
}

