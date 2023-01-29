{
  description = "A Mediabus compatible Conduit for encoding ISO-14496-3 AAC using the Frauenhofer AAC FDK";
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    # Flake for better Haskell builds
    # For updating to a newer fersion comment this line
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # --------------------------------
    # This is required for ./shell.nix
    # And the ./shell.nix is required for the vscode
    # nix-environment-selector plugin
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    # --------------------------------
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    { flake-utils
    , nixpkgs
    , haskellNix
    , ...
    }:
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        inherit (pkgs) lib fdk_aac;
        overlays = [
          haskellNix.overlay
          (final: prev:
            let
              mk-mediabus-fdk-aac = { withProfiling ? false }:
                final.haskell-nix.project {
                  src = final.haskell-nix.cleanSourceHaskell {
                    src = ./.;
                    name = "mediabus-fdk-aac";
                  };
                  projectFileName = "cabal.project";
                  compiler-nix-name = "ghc944"; # "ghc925";
                  pkg-def-extras = [ ];
                  modules = [
                    {
                      packages.mediabus-fdk-aac.components.library = {
                        pkgconfig = lib.mkForce [ [ fdk_aac ] ];
                        enableLibraryProfiling = withProfiling;
                        ghcOptions =
                          if withProfiling
                          then [ "-fprof-auto" ]
                          else [ ];
                      };
                      packages.mediabus-fdk-aac.components.benchmarks.encoder-benchmark = {
                        pkgconfig = lib.mkForce [ [ fdk_aac ] ];
                        enableProfiling = withProfiling;
                        ghcOptions =
                          if withProfiling
                          then [ "-fprof-auto" ]
                          else [ ];
                      };
                      packages.mediabus-fdk-aac.components.exes.mediabus-fdk-aac-example = {
                        pkgconfig = lib.mkForce [ [ fdk_aac ] ];
                        enableProfiling = withProfiling;
                        ghcOptions =
                          if withProfiling
                          then [ "-fprof-auto" ]
                          else [ ];
                      };
                      packages.mediabus-fdk-aac.components.tests.tests = {
                        # HACK make 'cabal test' work
                        # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-731699727
                        build-tools = [
                          final.mediabus-fdk-aac.hsPkgs.hspec-discover
                        ];
                        # END OF HACK
                        pkgconfig = lib.mkForce [ [ fdk_aac ] ];
                        enableProfiling = withProfiling;
                        ghcOptions =
                          if withProfiling
                          then [ "-fprof-auto" ]
                          else [ ];
                      };
                      # HACK make 'cabal test' work
                      # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-731699727
                      packages.mediabus.components.tests.tests.build-tools = [
                        final.mediabus-fdk-aac.hsPkgs.hspec-discover
                      ];
                      # END OF HACK
                    }
                  ];
                  shell.tools = {
                    cabal = { };
                    hlint = { };
                    ormolu = { };
                    haskell-language-server = { };
                  };
                  # Non-Haskell shell tools go here
                  shell.buildInputs = with pkgs; [
                    alejandra
                  ];
                };
            in
            {
              mediabus-fdk-aac = mk-mediabus-fdk-aac { withProfiling = false; };
              mediabus-fdk-aac-prof = mk-mediabus-fdk-aac { withProfiling = true; };
            })
        ];
        profFlake = pkgs.mediabus-fdk-aac-prof.flake {};
        profPkgs = profFlake.packages;
        profApps = profFlake.apps;
      in
      pkgs.lib.recursiveUpdate (pkgs.mediabus-fdk-aac.flake { })
      {packages = {
        "prof-mediabus-fdk-aac:bench:encoder-benchmark" = profPkgs."mediabus-fdk-aac:bench:encoder-benchmark";
        "prof-mediabus-fdk-aac:exe:mediabus-fdk-aac-example" = profPkgs."mediabus-fdk-aac:exe:mediabus-fdk-aac-example";
        "prof-mediabus-fdk-aac:test:tests" = profPkgs."mediabus-fdk-aac:test:tests";
      };
      apps = {
        "prof-mediabus-fdk-aac:bench:encoder-benchmark" = profApps."mediabus-fdk-aac:benchmark:encoder-benchmark";
        "prof-mediabus-fdk-aac:exe:mediabus-fdk-aac-example" = profApps."mediabus-fdk-aac:exe:mediabus-fdk-aac-example";
        "prof-mediabus-fdk-aac:test:tests" = profApps."mediabus-fdk-aac:test:tests";
      };}
    );
}
