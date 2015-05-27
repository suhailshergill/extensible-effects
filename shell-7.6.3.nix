with (import <nixpkgs> {}).pkgs;
(haskell.packages.ghc763.callPackage ./. {}).env
