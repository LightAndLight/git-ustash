{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    git-ustash.url = "path:../";
  };
  outputs = { self, nixpkgs, flake-utils, git-ustash }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.ghc
            cabal-install
            haskell-language-server

            git-ustash.packages.${system}.default
          ];
        };
      }
    );
}
