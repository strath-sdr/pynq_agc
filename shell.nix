{ nixpkgs ? import <nixpkgs> {} }:

let
     pkgs = import (builtins.fetchGit {
         name = "clash-1.2.4";                                                 
         url = "https://github.com/NixOS/nixpkgs/";                       
         ref = "refs/heads/nixpkgs-unstable";                     
         rev = "2c162d49cd5b979eb66ff1653aecaeaa01690fcc";                                           
     }) {};                                                                           

in

pkgs.mkShell {
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
