{pkgs ? import <nixpkgs> {}}:

with rec {
  inherit (pkgs) lib haskellPackages fetchFromGitHub;
  inherit (haskellPackages) callCabal2nix;
};

let

  mediabus = fetchFromGitHub {
    owner = "sheyll";
    repo = "mediabus";
    rev = "0.5.0.0";
    sha256 = null;
  };

  cleanSrc = lib.cleanSourceWith {
        filter = 
          (path: type:
            let base = baseNameOf (toString path);
            in !(lib.hasPrefix ".ghc.environment." base) &&
               !(lib.hasSuffix ".nix" base) &&
               !(lib.hasSuffix ".gz" base) &&
               !(lib.hasSuffix ".zx" base) 
          );
        src = lib.cleanSource ./.;
      };
in 
  callCabal2nix "mediabus-fdk-aac" cleanSrc { 
    fdk-aac = pkgs.fdk_aac; 
    inherit mediabus;
  }
