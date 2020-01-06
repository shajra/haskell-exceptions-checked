let config = import ../config.nix; in

{ ghcVersion ? config.ghc.version }:

let

    inherit (config) index-state index-sha256 plan-sha256 isMaterialized;

    checkMaterialization = config.checkMaterialization
        && isMaterialized && ! pkgs.lib.inNixShell;

    srcs = import ./sources.nix;

    hnSrc = srcs."haskell.nix";

    pinnedNixpkgsSrc = "${hnSrc}/nixpkgs";

    nixpkgsArgsOrig = import hnSrc;

    nixpkgsArgs = nixpkgsArgsOrig // {
        inherit (config) nixpkgs-pin;
    };

    pkgs = import pinnedNixpkgsSrc nixpkgsArgs;

    hn = pkgs.haskell-nix;

    modifiedSrc = src: pkgs.runCommand "exceptions-checked-src" {} ''
        cp -r "${hn.cleanSourceHaskell { inherit src; }}" "$out"
        chmod -R +w "$out"
        cp -r "${srcs.cabal-doctest}" "$out/cabal-doctest"
        cat << EOF >> "$out/cabal.project"
        optional-packages: ./cabal-doctest
        EOF
        substituteInPlace "$out/test/doctests.hs" \
            --replace "B.flags"          "B.flags_exe_doctests" \
            --replace "B.pkgs"           "B.pkgs_exe_doctests"  \
            --replace "B.module_sources" "B.module_sources_exe_doctests"
    '';

    pkgSet = hn.cabalProject {
        name = "exceptions-checked";
        src = if pkgs.lib.inNixShell then ../. else modifiedSrc ../.;
        ghc = hn.compiler.${ghcVersion};
	inherit index-state index-sha256 plan-sha256
            checkMaterialization;
        ${if isMaterialized then null else "materialized"} = ./materialized;
        modules = [({...}: {
            doHaddock = true;
            packages.exceptions-checked.components.tests.doctests = {
                extraSrcFiles = [ "test" ];
                preCheck = ''
                    ghc-pkg init dist/package.conf.inplace
                '';
            };
            packages.ghc.flags.ghci = pkgs.lib.mkForce true;
            packages.ghci.flags.ghci = pkgs.lib.mkForce true;
            reinstallableLibGhc = true;
        })];
    };

in { inherit hn pkgs pkgSet; }
