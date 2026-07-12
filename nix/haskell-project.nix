{inputs, ...}: {
  imports = [
    {
      perSystem = {system, ...}: {
        # Inject haskell.nix overlay
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [inputs.haskell-nix.overlay];
        };
      };
    }
  ];

  perSystem = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (pkgs.stdenv.hostPlatform) isx86_64 isLinux;

    crossPlatforms = p:
      lib.optionals isx86_64 [p.mingwW64]
      ++ lib.optionals (isx86_64 && isLinux) [p.musl64];

    cabalProject = pkgs.haskell-nix.cabalProject' {
      src = ./..;
      compiler-nix-name = "ghc910";
      name = "pomodoro-waybar-module-hs";

      shell = {
        tools = {
          cabal = "latest";
          haskell-language-server = "latest";
        };

        buildInputs = with pkgs; [
          statix # nix static analysis
          deadnix # nix dead-code detector
          hlint # Haskell static analysis
        ];
        inputsFrom = [config.treefmt.build.devShell];

        withHoogle = true;

        # We don't need cross platforms in the shell; should speed up evaluation
        crossPlatforms = _: [];
      };
    };

    # Add exes to cabal project
    haskellProject = cabalProject.appendOverlays [
      pkgs.haskell-nix.haskellLib.projectOverlays.projectComponents
    ];

    # Flake with cross builds
    haskellFlake = haskellProject.flake {
      inherit crossPlatforms;
    };

    # Flake without cross builds; We don't need to run tests on them
    nativeFlake = haskellProject.flake {
      crossPlatforms = _: [];
    };
  in {
    # expose haskellProject to other modules
    _module.args.haskellProject = haskellProject;

    inherit (haskellFlake) packages devShells apps;
    inherit (nativeFlake) checks;
  };
}
