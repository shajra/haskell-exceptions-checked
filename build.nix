let

    pkgsMakePath =
	(import <nixpkgs> {}).fetchFromGitHub {
	    owner = "shajra";
	    repo = "example-nix";
	    rev = "a4c3a5be0e7aae28886b3ef6a5b65e146d7538a4";
	    sha256 = "0qhqhpq4ws4b4r3c7m7x1kba7i8m4aygkmvah6pb7s78ikh4yqd5";
	};

    pkgsMake = import pkgsMakePath;

    pkgsMakeArgs = {
      haskellArgs.ghcVersion="ghc843";
      nixpkgsRev = "20c4986c4dd9b014991c619498ae6ef09aa5d0aa";
      nixpkgsSha256 = "1i33m4p0g77xrn3wpybh21lb1ky7f6dgddf4zyyph9zq2mf4316b";
    };

in

pkgsMake pkgsMakeArgs ({call, ...}: {
    app = call.haskell.app ./.;
})
