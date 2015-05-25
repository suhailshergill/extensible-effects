{ mkDerivation, base, HUnit, QuickCheck, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2
, test-framework-th, transformers, transformers-base, type-aligned
, void
}:
mkDerivation {
  pname = "extensible-effects";
  version = "1.10.0.1";
  src = ./.;
  buildDepends = [
    base transformers transformers-base type-aligned void
  ];
  testDepends = [
    base HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2 test-framework-th void
  ];
  homepage = "https://github.com/suhailshergill/extensible-effects";
  description = "An Alternative to Monad Transformers";
  license = stdenv.lib.licenses.mit;
}
