let config = import ./config.nix; in

args@{ ghcVersion ? config.ghc.default }:

(import ./default.nix args).env.haskell.withEnvTools (pkgs:
    [ pkgs.haskellPackages.haskell-ci ]
)
