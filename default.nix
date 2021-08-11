{ haskell-nix ? (import ./nix/pkgs.nix).haskell-nix
, withProfiling ? false
, lib ? (import ./nix/pkgs.nix).lib
, fdk_aac ? (import ./nix/pkgs.nix).fdk_aac
}:
let
  this =
    haskell-nix.project
      {
        src = haskell-nix.cleanSourceHaskell {
          src = ./.;
          name = "mediabus-fdk-aac";
        };
        projectFileName = "cabal.project";
        compiler-nix-name = "ghc8105";
        pkg-def-extras = [];
        modules =
          [
            {
              packages.mediabus-fdk-aac.components.library.pkgconfig = lib.mkForce [ [ fdk_aac ] ];
              # HACK make 'cabal test' work
              # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-731699727
              packages.mediabus-fdk-aac.components.tests.examples = {
                build-tools = [
                  this.hsPkgs.hspec-discover
                ];
                pkgconfig = lib.mkForce [ [ fdk_aac ] ];
                enableExecutableProfiling = withProfiling;
                ghcOptions = if withProfiling then ["-fprof-auto"] else [];
              };
              packages.mediabus.components.tests.tests.build-tools = [
                this.hsPkgs.hspec-discover
              ];
              # END OF HACK
              packages.mediabus-fdk-aac.allComponent = {
                enableExecutableProfiling = withProfiling;
                enableLibraryProfiling = withProfiling;
              } // (
                if withProfiling then
                  {
                    ghcOptions = "-fprof-auto";
                  }
                else {}
              );
            }
          ];
      };
in
this
