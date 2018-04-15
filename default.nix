{ mkDerivation, base, criterion, HUnit, monad-control, mtl
, QuickCheck, silently, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2
, test-framework-th, transformers-base
}:
mkDerivation {
  pname = "extensible-effects";
  version = "2.5.3.0";
  src = ./.;
  libraryHaskellDepends = [ base monad-control transformers-base ];
  testHaskellDepends = [
    base HUnit monad-control QuickCheck silently test-framework
    test-framework-hunit test-framework-quickcheck2 test-framework-th
  ];
  benchmarkHaskellDepends = [
    base criterion HUnit mtl test-framework test-framework-hunit
    test-framework-quickcheck2 test-framework-th
  ];
  homepage = "https://github.com/suhailshergill/extensible-effects";
  description = "An Alternative to Monad Transformers";
  license = stdenv.lib.licenses.mit;
}
