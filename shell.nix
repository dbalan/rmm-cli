{ pkgs ? import <nixpkgs> {}
}:

pkgs.stdenv.mkDerivation rec {
  name = "rmm";

  nativeBuildInputs = [
    pkgs.pkg-config
  ];

  buildInputs = [
    pkgs.zlib
    pkgs.ghc
    pkgs.haskellPackages.haskell-language-server
    pkgs.cabal-install
    pkgs.haskellPackages.ghcid
    # fixup until hs-libsodium releases their new version
    pkgs.haskellPackages.cabal-plan
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
    export LANG=en_US.UTF-8
  '';
}
