{
  description = "Shell with Node.js + globally installed @openai/codex CLI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        node = pkgs.nodejs_20;

        # Create a writable prefix so `npm install -g` works inside Nix shell
        npmPrefix = ".npm-global";
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            node
            pkgs.nodePackages.npm
            pkgs.racket # racket executable available for REPL and raco
          ];

          # Make global npm installs go into a local directory in the project
          shellHook = ''
            export NPM_CONFIG_PREFIX="$PWD/${npmPrefix}"
            export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"

            # Install @openai/codex globally if not installed yet
            if ! command -v codex >/dev/null 2>&1; then
              echo "Installing @openai/codex globally..."
              npm install -g @openai/codex@latest
            fi

            echo "Codex CLI ready. Run: codex --help"
          '';
        };
      });
}
