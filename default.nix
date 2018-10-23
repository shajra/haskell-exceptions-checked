let config = import ./config.nix; in

{ ghcVersion ? config.ghc.default }:

let

    pkgsMakePath =
	(import <nixpkgs> {}).fetchFromGitHub {
	    owner = "shajra";
	    repo = "example-nix";
	    rev = config.example-nix.rev;
	    sha256 = config.example-nix.sha256;
	};

    pkgsMake = import pkgsMakePath;

    pkgsMakeArgs = {
        haskellArgs = {
            inherit ghcVersion;
            srcTransform = lib:
                lib.nix.filterFilesBySuffices [ ".hs" ".cabal" "LICENSE" ".md" ];
            extraOverrides = nixpkgs: self: super:
                if ghcVersion == "ghc802"
                then { cabal2nix = nixpkgs.haskell.packages.ghc844.cabal2nix; }
                else {};
        };
        nixpkgsRev = config.nixpkgs.rev;
        nixpkgsSha256 = config.nixpkgs.sha256;
    };

in

pkgsMake pkgsMakeArgs ({call, lib, ...}: rec {
    exceptions-checked =
        lib.haskell.enableCabalFlag
            (call.haskell.cabal2nix.lib ./.)
            "strict";

    exceptions-checked-doc = exceptions-checked.doc;

})
