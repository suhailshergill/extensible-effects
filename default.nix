{ mkDerivation, base, directory, HUnit, QuickCheck, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, test-framework-th, transformers, transformers-base
}:
mkDerivation {
  pname = "extensible-effects";
  version = "2.3.0.1";
  src = ./.;
  libraryHaskellDepends = [ base transformers transformers-base ];
  testHaskellDepends = [
    base directory HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2 test-framework-th
  ];
  homepage = "https://github.com/suhailshergill/extensible-effects";
  description = "An Alternative to Monad Transformers";
  license = stdenv.lib.licenses.mit;
}
