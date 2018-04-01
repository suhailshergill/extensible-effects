with (import <nixpkgs> {}).pkgs;
(haskell.packages.ghc802.callPackage ./. {}).env
