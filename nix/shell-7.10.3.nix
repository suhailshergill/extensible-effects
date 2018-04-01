with (import <nixpkgs> {}).pkgs;
(haskell.packages.ghc7103.callPackage ./. {}).env
