let 

    index-state  = "2020-01-01T00:00:00Z";
    index-sha256 = "00yycmqrncbpqpahvnvj0frgxl2dhsjyyh4bcclyzsalqmgspdhx";
    plan-sha256  = "1lfw3mf6cw1b5d95bxznkg51rbdgqd8r0fq87km3ydl6qy5wslph";
    checkMaterialization = false && ! pkgs.lib.inNixShell;

    srcs = import ./sources.nix;

    hnSrc = srcs."haskell.nix";

    pinnedNixpkgsSrc = "${hnSrc}/nixpkgs";

    hnArgsOrig = import hnSrc;

    hnArgs = hnArgsOrig // { 
        nixpkgs-pin = "release-19.03"; 
    };

    pkgs = import pinnedNixpkgsSrc hnArgs;

    hn = (import pinnedNixpkgsSrc hnArgs).haskell-nix;

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
        inherit index-state index-sha256 plan-sha256 checkMaterialization;
        ${if pkgs.lib.inNixShell then null else "materialized"} = ./materialized;
        modules = [({config, ...}: { 
            reinstallableLibGhc = true; 
            packages.exceptions-checked.components.tests.doctests = {
                extraSrcFiles = [ "test" ];
                preCheck = ''
                    ghc-pkg init dist/package.conf.inplace
                '';
            };
            doHaddock = true;
        })];
    };

in { inherit hn pkgs pkgSet; }
