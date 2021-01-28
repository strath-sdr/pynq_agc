{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

mkShell {
  name = "clash-compiler-shell";
  shellHook = "";
  buildInputs = [

    (pkgs.haskellPackages.ghcWithPackages (p: with p; [
      clash-ghc
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
