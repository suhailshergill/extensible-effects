with (import <nixpkgs> {}).pkgs;
(haskell.packages.ghc822.callPackage ./. {}).env
