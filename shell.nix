{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

mkShell {
  name = "clash-compiler-shell";
  shellHook = "source /tools/Xilinx/Vivado/2019.1/settings64.sh";
  buildInputs = [
    # My own packages...
    #yosys
    #graphviz

    #(python36.buildEnv.override {
    #  extraLibs = with python36Packages; [
    #    # Add pythonPackages without the prefix
	#xdot
    #  ];
    #})

    # For quick clash experimentation
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
    ])
    )
  ];
}
