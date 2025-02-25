{
  description = "CSDS Simple Interpreter Project";  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
    in {
      devShells = {
        default = pkgs.mkShell {
          shellHook = ''
            raco pkg install --auto racket-langserver
            raco pkg install --auto fmt
          '';
          packages = with pkgs; [
            racket
            treefmt
            pre-commit
          ];
        };
      };
    });
}