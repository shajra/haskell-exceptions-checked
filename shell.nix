let build = import ./nix {};
in (build.pkgSet.shellFor {
    exactDeps = false;
    buildInputs = [
        build.hn.bootstrap.packages.cabal-install
        build.hn.nix-tools
     ];
}).overrideAttrs(old: {
    shellHook = ''
        export DOCTEST_PKG_DB="$(
            find "$(readlink -f "$(dirname "$(command -v ghc)")"/..)" \
                -name package.conf.d
        )"
    '';
})
