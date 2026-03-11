{
  description = "CS491CAP Gradebook - Haskell gradebook management system";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, haskellNix, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            gradebookProject = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc984";

              # Generate .cabal from package.yaml using hpack
              supportHpack = true;

              shell = {
                tools = {
                  cabal = {};
                  haskell-language-server = {};
                };

                buildInputs = with pkgs; [
                  postgresql
                  sqlite
                  fzf
                  git
                ];

                shellHook = ''
                  if [ -t 1 ]; then
                    echo "Haskell dev shell ready (haskell.nix)" >&2
                    echo "GHC: $(ghc --version 2>/dev/null || true)" >&2
                    echo "" >&2
                    echo "Build commands:" >&2
                    echo "  nix build        - Build the project" >&2
                    echo "  cabal build      - Build with cabal (in dev shell)" >&2
                    echo "  cabal run gb     - Run the gb executable" >&2
                    echo "" >&2
                  fi
                '';
              };
            };
          })
        ];

        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        flake = pkgs.gradebookProject.flake {};
      in
      flake // {
        packages.default = flake.packages."gradebook:exe:gb";

        apps.default = {
          type = "app";
          program = "${flake.packages."gradebook:exe:gb"}/bin/gb";
        };
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
