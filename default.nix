let config = import ./config; in

args@{ ghcVersion ? config.ghc.version }:

let build = import ./nix/default.nix args;
in {
    all  = build.pkgSet.exceptions-checked.components.all;
    checks = build.pkgSet.exceptions-checked.checks;
}
