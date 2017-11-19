{ mkDerivation, base, directory, HUnit, QuickCheck, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, test-framework-th, transformers, transformers-base, type-aligned
}:
mkDerivation {
  pname = "extensible-effects";
  version = "2.0.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base transformers transformers-base type-aligned
  ];
  testHaskellDepends = [
    base directory HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2 test-framework-th
  ];
  homepage = "https://github.com/suhailshergill/extensible-effects";
  description = "An Alternative to Monad Transformers";
  license = stdenv.lib.licenses.mit;
}
