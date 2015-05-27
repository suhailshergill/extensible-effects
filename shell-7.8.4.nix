with (import <nixpkgs> {}).pkgs;
(haskell.packages.ghc784.callPackage ./. {}).env
