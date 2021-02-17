{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

let

    clash-125 = nixpkgs.fetchFromGitHub {
		    owner = "clash-lang";
		    repo = "clash-compiler";
		    rev = "ce0723d84eb33f76acacac7a06326a1d0cfbfac2";
		    sha256 = "1mfwnyhvaxw9iy5g0rpmgfc558bpzp5xc4k08vh97cvb35cj3k20";
		    fetchSubmodules = true;
		};
    myclash = import clash-125 {};
in

nixpkgs.mkShell {
  name = "myweeshell";
  shellHook = "";
  buildInputs = [

    (nixpkgs.haskellPackages.ghcWithPackages (p: with p; [
      myclash.clash-ghc
      ghc-typelits-extra
      ghc-typelits-knownnat
      ghc-typelits-natnormalise
      zlib
      plotlyhs
      lucid
      dsp
      pure-fft
      QuickCheck
      hspec
      statistics
    ])
    )
  ];
}
