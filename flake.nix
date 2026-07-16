{
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt.url = "github:numtide/treefmt-nix";
  };

  outputs = {
    flake-parts,
    flake-utils,
    treefmt,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        treefmt.flakeModule

        # Project build via haskell.nix
        ./nix/haskell-project.nix
        # Platform-specific release distribution archives
        ./nix/distribution.nix
        # Tests and static analyzers
        ./nix/checks.nix
        # Source code formatters (treefmt)
        ./nix/formatter.nix
      ];

      systems = flake-utils.lib.defaultSystems;
    };

  nixConfig = {
    extra-trusted-public-keys = [
      "tomato-slicer.cachix.org-1:ozsnl+TFQm8GOJ+JqPbPdr9F6JFRFbEV9FuTCvoObPc="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    extra-substituters = [
      "https://cache.iog.io"
      "https://tomato-slicer.cachix.org"
    ];

    allow-import-from-derivation = "true";
  };
}
