extensible-effects is based on the work
[Extensible Effects: An Alternative to Monad Transformers](http://okmij.org/ftp/Haskell/extensible/).
Please read the [paper](http://okmij.org/ftp/Haskell/extensible/exteff.pdf) for details.

[![Build Status](https://travis-ci.org/suhailshergill/extensible-effects.svg?branch=master)](https://travis-ci.org/suhailshergill/extensible-effects)
[![Join the chat at https://gitter.im/suhailshergill/extensible-effects](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/suhailshergill/extensible-effects?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Advantages


  * Effects can be added, removed, and interwoven without changes to code not
    dealing with those effects.

## Disadvantages

### For GHC version 7.8 and upwards
  * Common functions can't be grouped using typeclasses, e.g.
    the `ask` and `getState` functions can't be grouped with some

        class Get t a where
          ask :: Member (t a) r => Eff r a

    `ask` is inherently ambiguous, since the type signature only provides
    a constraint on `t`, and nothing more. To specify fully, a parameter
    involving the type `t` would need to be added, which would defeat the point
    of having the grouping in the first place.
  * Requires a `Typeable` instance on the return type. This is no longer a
    limitation on GHC versions 7.8 and above.
    * fixed by https://github.com/suhailshergill/extensible-effects/issues/38

### For GHC versions prior to 7.8
  * Neither `Eff` nor `(:>)` has a `Typeable` instance, and can thus often not
    be used as a return type (e.g. `State` type) for other `Eff`s. 
    * fixed by https://github.com/suhailshergill/extensible-effects/issues/38
