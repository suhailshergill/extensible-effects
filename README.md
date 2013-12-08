extensible-effects is based on the work
[Extensible Effects: An Alternative to Monad Transformers](http://okmij.org/ftp/Haskell/extensible/).
Please read the [paper](http://okmij.org/ftp/Haskell/extensible/exteff.pdf) for details.

# Advantages

  * Effects can be added, removed, and interwoven without changes to code not
    dealing with those effects.

# Disadvantages

  * Common functions can't be grouped using typeclasses, e.g.
    the `ask` and `getState` functions can't be grouped with some

        class Get t a where
          ask :: Member (t a) r => Eff r a

    `ask` is inherently ambiguous, since the type signature only provides
    a constraint on `t`, and nothing more. To specify fully, a parameter
    involving the type `t` would need to be added, which would defeat the point
    of having the grouping in the first place.

  * Requires a `Typeable` instance on the return type.
  * Neither Eff nor (:>) has a Typeable instance, and thus, cannot be returned.