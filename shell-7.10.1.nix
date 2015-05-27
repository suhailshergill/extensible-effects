with (import <nixpkgs> {}).pkgs;
(haskell.packages.ghc7101.callPackage ./. {}).env
