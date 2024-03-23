{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    cargo2nix.url = "github:cargo2nix/cargo2nix";
    rust-overlay.follows = "cargo2nix/rust-overlay";
  };
  
  outputs = { self, nixpkgs, flake-utils, rust-overlay, cargo2nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            rust-overlay.overlays.default
            cargo2nix.overlays.default
          ];
        };

        rustVersion = "1.74.0";
      
      in {
        devShell =
          pkgs.mkShell {
            buildInputs = [
              (pkgs.rust-bin.stable.${rustVersion}.default.override {
                extensions = [
                  "cargo"
                  "clippy"
                  "rustc"
                  "rust-src"
                  "rustfmt"
                  "rust-analyzer"
                ];
              })
              cargo2nix.packages.${system}.cargo2nix

              pkgs.pkg-config
              pkgs.openssl.dev
            ];
          };

        packages = rec {
          git-ustash =
            let
              rustPkgs = pkgs.rustBuilder.makePackageSet {
                inherit rustVersion;
                packageFun = import ./Cargo.nix;
              };
            in
            rustPkgs.workspace.git-ustash {};

          default = git-ustash;
        };
      }
    );
}
