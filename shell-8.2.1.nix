with (import <nixpkgs> {}).pkgs;
(haskell.packages.ghc821.callPackage ./. {}).env
