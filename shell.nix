with (import <nixpkgs> {}).pkgs;
(haskellPackages.callPackage ./. {}).env
