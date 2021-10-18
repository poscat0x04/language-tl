{
  inputs.nixpkgs.url = github:poscat0x04/nixpkgs/dev;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = language-tl-dev.envFunc { withHoogle = true; };
            defaultPackage = language-tl;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          language-tl = hpkgs.callCabal2nix "language-tl" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit language-tl;
            language-tl-dev = addBuildTools language-tl [
              haskell-language-server
              cabal-install
            ];
          };
    };
}
