let config = import ./config.nix; in

args@{ ghcVersion ? config.ghc.default }:

(import ./default.nix args).env.haskell.withEnvTools (pkgs:
    if ghcVersion == "ghc861"
    then []
    else [ pkgs.haskellPackages.haskell-ci ]
)
