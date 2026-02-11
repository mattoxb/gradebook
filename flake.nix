{
  description = "Haskell course dev shell (GHC + Stack + HLS)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Pick a recent compiler toolchain from nixpkgs.
        # If you want to *pin* a specific version for the semester,
        # set it explicitly here (see notes below).
        hsPkgs = pkgs.haskell.packages.ghc984; # change if you prefer (ghc96, ghc910, etc.)
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            hsPkgs.ghc
            pkgs.stack
            hsPkgs.haskell-language-server
            pkgs.cabal-install    # handy even if you teach Stack
            pkgs.zlib             # common native dep; keeps build logs calmer
            pkgs.pkg-config       # same
            pkgs.git              # Stack often needs it
            pkgs.fzf              # For student search
          ];

          buildInputs = [
            pkgs.sqlite           # For HDBC-sqlite3
            # pkgs.postgresql     # For HDBC-postgresql (when needed)
          ];

          # Make Stack use system GHC so it doesn't download its own toolchain.
          # This plays nicely with Nix.
          STACK_YAML = "stack.yaml";
          STACK_IN_NIX_SHELL = "true";

          # Make native libraries visible to Stack
          LD_LIBRARY_PATH = "${pkgs.sqlite.out}/lib";
          LIBRARY_PATH = "${pkgs.sqlite.out}/lib";
          C_INCLUDE_PATH = "${pkgs.sqlite.dev}/include";

          shellHook = ''
            # Only show welcome message in interactive shells
            if [ -t 1 ]; then
              echo "Haskell dev shell ready." >&2
              echo "GHC: $(ghc --version 2>/dev/null || true)" >&2
              echo "Stack: $(stack --version 2>/dev/null || true)" >&2
              echo "HLS: $(haskell-language-server-wrapper --version 2>/dev/null || true)" >&2
              echo "" >&2
            fi
          '';
        };
      });
}
