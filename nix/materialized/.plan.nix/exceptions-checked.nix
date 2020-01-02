let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { strict = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "exceptions-checked"; version = "0.0.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2018 Sukant Hajra";
      maintainer = "Sukant Hajra <rrudbskr5g@snkmail.com>";
      author = "Sukant Hajra, Pepe Iborra";
      homepage = "https://github.com/shajra/exceptions-checked#readme";
      url = "";
      synopsis = "Statically Checked Exceptions";
      description = "This package provides an API to statically check exceptions at the\ntype-level. Think of it like checked exceptions in Java, but with better\nergonomics. People sometimes claim that checked exceptions are a failed\nexperiment. This module is an attempt to prove the contrary.\n\nThough there are enough differences to warrant a separate package, this\nwork is heavily derived from Pepe Iborra's\n<https://hackage.haskell.org/package/control-monad-exception control-monad-exception>\npackage and supporting paper\n<https://dl.acm.org/citation.cfm?id=2127644 Explicitly typed exceptions for Haskell>.\n\nSome features include:\n\n* delegation of exception handling to the\n<https://hackage.haskell.org/package/safe-exceptions safe-exceptions>\nlibrary (instead of 'Control.Exception') for improved handling of\nsynchronous versus asynchronous exceptions.\n\n* an ergonomic API including not just a basic 'throw' and 'catch', but\nhandling functions such as 'catches', 'finally', 'bracket', and\n'onException'.";
      buildType = "Custom";
      isLocal = true;
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (buildToolDepError "base")))
        (hsPkgs.buildPackages.directory or (pkgs.buildPackages.directory or (buildToolDepError "directory")))
        (hsPkgs.buildPackages.filepath or (pkgs.buildPackages.filepath or (buildToolDepError "filepath")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (buildToolDepError "Cabal")))
        (hsPkgs.buildPackages.cabal-doctest or (pkgs.buildPackages.cabal-doctest or (buildToolDepError "cabal-doctest")))
        ];
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" "CHANGELOG.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."mmorph" or (buildDepError "mmorph"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.alex or (pkgs.buildPackages.alex or (buildToolDepError "alex")))
          (hsPkgs.buildPackages.happy or (pkgs.buildPackages.happy or (buildToolDepError "happy")))
          ];
        buildable = true;
        modules = [ "Control/Exception/Checked" ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base-compat" or (buildDepError "base-compat"))
            (hsPkgs."doctest" or (buildDepError "doctest"))
            (hsPkgs."exceptions-checked" or (buildDepError "exceptions-checked"))
            ];
          buildable = true;
          modules = [ "Build_doctests" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "doctests.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }