{ compiler }:
let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };

  inherit (import sources.gitignore { inherit (pkgs) lib; }) gitignoreSource;

  hPkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      "language-tl" =
        self.callCabal2nix "language-tl" (gitignoreSource ../.) { };
    };
  };

  shell = hPkgs.shellFor {
    packages = p: [ p."language-tl" ];

    nativeBuildInputs = with pkgs.haskellPackages; [
      cabal-install
      ormolu
      hlint
      hpack
    ] ++ [
      hPkgs.ghcide
    ];
  };

  lib = hPkgs."language-tl".overrideAttrs (_: {
    configureFlags = [
      "--enable-tests"
      "--enable-optimization"
      "--enable-static"
      "--enable-shared"
      "--enable-profiling"
    ];
  });
in { inherit shell lib; }
