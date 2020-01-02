{
  pkgs = hackage:
    {
      packages = {
        "ghc".revision = (((hackage."ghc")."8.6.5").revisions).default;
        "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
        "exceptions".flags.transformers-0-4 = true;
        "ghc-paths".revision = (((hackage."ghc-paths")."0.1.0.12").revisions).default;
        "binary".revision = (((hackage."binary")."0.8.6.0").revisions).default;
        "ghc-boot".revision = (((hackage."ghc-boot")."8.6.5").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "ghc-heap".revision = (((hackage."ghc-heap")."8.6.5").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "mmorph".revision = (((hackage."mmorph")."1.1.3").revisions).default;
        "ghci".revision = (((hackage."ghci")."8.6.5").revisions).default;
        "alex".revision = (((hackage."alex")."3.2.5").revisions).default;
        "alex".flags.small_base = true;
        "safe-exceptions".revision = (((hackage."safe-exceptions")."0.1.7.0").revisions).default;
        "syb".revision = (((hackage."syb")."0.7.1").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "doctest".revision = (((hackage."doctest")."0.16.2").revisions).default;
        "parsec".revision = (((hackage."parsec")."3.1.13.0").revisions).default;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.6").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "directory".revision = (((hackage."directory")."1.3.3.0").revisions).default;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.6.5").revisions).default;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.mtl = true;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.three = false;
        "template-haskell".revision = (((hackage."template-haskell")."2.14.0.0").revisions).default;
        "base-compat".revision = (((hackage."base-compat")."0.11.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.0.1").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.8.2").revisions).default;
        "text".revision = (((hackage."text")."1.2.3.1").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."2.4.0.1").revisions).default;
        "base".revision = (((hackage."base")."4.12.0.0").revisions).default;
        "time".revision = (((hackage."time")."1.8.0.2").revisions).default;
        "terminfo".revision = (((hackage."terminfo")."0.4.1.2").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "happy".revision = (((hackage."happy")."1.19.12").revisions).default;
        "happy".flags.small_base = true;
        "hpc".revision = (((hackage."hpc")."0.6.0.3").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "code-page".revision = (((hackage."code-page")."0.2").revisions).default;
        "process".revision = (((hackage."process")."1.6.5.0").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.6.5").revisions).default;
        "array".revision = (((hackage."array")."0.5.3.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.6.5";
        nix-name = "ghc865";
        packages = {
          "ghc" = "8.6.5";
          "binary" = "0.8.6.0";
          "ghc-boot" = "8.6.5";
          "ghc-prim" = "0.5.3";
          "stm" = "2.5.0.0";
          "unix" = "2.7.2.2";
          "ghc-heap" = "8.6.5";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "ghci" = "8.6.5";
          "deepseq" = "1.4.4.0";
          "parsec" = "3.1.13.0";
          "directory" = "1.3.3.0";
          "template-haskell" = "2.14.0.0";
          "containers" = "0.6.0.1";
          "bytestring" = "0.10.8.2";
          "text" = "1.2.3.1";
          "Cabal" = "2.4.0.1";
          "base" = "4.12.0.0";
          "time" = "1.8.0.2";
          "terminfo" = "0.4.1.2";
          "transformers" = "0.5.6.2";
          "hpc" = "0.6.0.3";
          "filepath" = "1.4.2.1";
          "process" = "1.6.5.0";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.6.5";
          "array" = "0.5.3.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        exceptions-checked = ./.plan.nix/exceptions-checked.nix;
        cabal-doctest = ./.plan.nix/cabal-doctest.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "exceptions-checked" = {
            flags = { "strict" = lib.mkOverride 900 false; };
            };
          "cabal-doctest" = { flags = {}; };
          };
        })
    ];
  }