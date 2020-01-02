let build = import ./nix/default.nix;
in {
    all  = build.pkgSet.exceptions-checked.components.all;
    test = build.hn.haskellLib.check 
        build.pkgSet.exceptions-checked.components.tests.doctests;
}
