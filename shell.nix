let build = import ./nix;
in build.pkgSet.shellFor {
    buildInputs = with build.hn.bootstrap.packages;
     [ alex happy cabal-install build.hn.nix-tools 
     ] ++ build.pkgSet.exceptions-checked.setup.buildInputs;
}
