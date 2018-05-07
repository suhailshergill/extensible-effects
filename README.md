
# Extensible effects

[![Build Status](https://travis-ci.org/suhailshergill/extensible-effects.svg?branch=master)](https://travis-ci.org/suhailshergill/extensible-effects)
[![Join the chat at https://gitter.im/suhailshergill/extensible-effects](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/suhailshergill/extensible-effects?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Stories in Ready](https://badge.waffle.io/suhailshergill/extensible-effects.png?label=ready&title=Ready)](http://waffle.io/suhailshergill/extensible-effects)
[![Stories in progress](https://badge.waffle.io/suhailshergill/extensible-effects.png?label=in%20progress&title=In%20progress)](http://waffle.io/suhailshergill/extensible-effects)

*Implement effectful computations in a modular way!*

The main and only monad is built upon `Eff` from `Control.Eff`.
`Eff r a` is parameterized by the effect-list `r` and the monadic-result type
`a` similar to other monads.
It is the intention that all other monadic computations can be replaced by the
use of `Eff`.

In case you know monad transformers or `mtl`:
This library provides only one monad that includes all your effects instead of
layering different transformers.
It is not necessary to lift the computations through a monad stack.
Also, it is not required to lift every `Monad*` typeclass (like `MonadError`)
though all transformers.

## Quickstart

To experiment with this library, it is suggested to write some lines within
`ghci`.
This section will include some code-examples, which you should try on your own!

Recommended Procedure:

1. add `extensible-effects` as a dependency to a existing cabal or stack project
or `git clone https://github.com/suhailshergill/extensible-effects.git`
2. start `stack ghci` or `cabal repl`
3. import some library modules as described in this section

*examples are a work in progress and there will be some Quickstart module to go
along the guide here*

*examples...*

## Tour through Extensible Effects

This section explains the basic concepts of this library.

### The Effect List

```haskell
import Control.Eff
```

The effect list `r` in the type `Eff r a` is a central concept in this library.
It is a type-level list containing effect types.

If `r` is the empty list, then the computation `Eff r` (or `Eff '[]`) does not
contain any effects to be handled and therefore is a pure computation.
In this case, the result value can be retrieved by `run :: Eff '[] a -> a`

For programming within the `Eff r` monad, it is almost never necessary to list
all effects that can appear.
It suffices to state what types of effects are at least required.
This is done via the `Member t r` typeclass. It describes that the type `t`
occurs inside the list `r`.
If you really want, you can still list all Effects and their order in which
they are used (e.g. `Eff '[Reader r, State s] a`).

### Handling Effects

Functions containing something like `Eff (x ': r) a -> Eff r a` handle effects.

The transition from the longer list of effects `(x ': r)` to just `r`
is a type-level indicator that the effect `x` has been handled.
Depending on the effect, some additional input might be required or some
different output than just `a` is produced.

The handler functions typically are called `run*`, `eval*` or `exec*`.

### Most common Effects

The most common effects used are `Writer`, `Reader`, `Exception` and `State`.

For the `Writer`, `Reader` and `State`, there are lazy and a strict variants.
Each has its own module that provide the same interface.
By importing one or the other, it can be controlled if the effect is strict or
lazy in its inputs and outputs.
Note that this changes the strictness of that effect only.

In this section, only the core functions associated with an effect are
presented.
Have a look at the modules for additional details.

#### The Exception Effect

```haskell
import Control.Eff.Exception
```

The exception effect adds the possibility to exit a computation preemptively
with an exception.
Note that the exceptions from this library are handled by the programmer and
have nothing to do with exceptions thrown inside the Haskell run-time.

```haskell
throwError :: Member (Exc e) r => e -> Eff r a
runError :: Eff (Exc e ': r) a -> Eff r (Either e a)
```

An exception can be thrown using the `throwError` function.
Its return type is `Eff r a` with an arbitrary type `a`.
When handling the effect, the result-type changes to `Either e a` instead of
just `a`.
This indicates how the effect is handled: The returned value is either the
thrown exception or the value returned from a successful computation.

#### The State Effect

```haskell
import Control.Eff.State.{Lazy | Strict}
```

The state effect provides readable and writable state during a computation.

```haskell
get :: Member (State s) r => Eff r s
put :: Member (State s) r => s -> Eff r ()
modify :: Member (State s) r => (s -> s) -> Eff r ()
runState :: s -> Eff (State s ': r) a -> Eff r (a, s)
```

The `get` functions accesses the current state and makes it usable within the
further computation.
The `put` function sets the state to the given value.
`modify` updates the state using a mapping function by combining `get` and
`put`.

The state-effect is handled using the `runState` function.
It takes the initial state as an argument and returns the final state and
effect-result.

#### The Reader Effect

```haskell
import Control.Eff.Reader.{Strict | Lazy}
```

The reader effect provides an environment that can be read.
Sometimes it is considered as read-only state.

```haskell
ask :: Member (Reader e) r => e -> Eff r e
runReader :: e -> Eff (Reader e ': r) a -> Eff r a
```

The environment given to the handle the reader effect is the one given during
the computation if asked for.

#### The Writer Effect

```haskell
import Control.Eff.Writer.{Strict | Lazy}
```

The writer effect allows to output messages during a computation.
It is sometimes referred to as write-only state, which gets retrieved at the
end of the computation.

```haskell
tell :: Member (Writer w) r => w -> Eff r ()
runWriter :: (w -> b -> b) -> b -> Eff (Writer w ': r) a -> Eff r (a, b)
runListWriter :: Eff (Writer w ': r) a -> Eff r (a, [w])
```

Running a writer can be done in several ways.
The most general function is `runWriter` that folds over all written values.
However, if you only want to collect the the values written, the `runListWriter`
function does that.

Note that compared to mtl, the value written has no Monoid constraint on it and
can be collected in any way.

### Using multiple Effects

The main benefit of this library is that multiple effects can be included
with ease.

If you need state and want to be able exit the computation with an exception,
the type of your effectful computation would be the one of `myComp` below.
Then, both the state and exception effect-functions can be used.
To handle the effects, both the `runState` and `runError` functions have to be
provided.

```haskell
myComp :: (Member (Exc e) r, Member (State s) r) => Eff r a

run1 :: (Either e a, s)
run1 = run . runState initalState . runError $ myComp

run2 :: Either e (a, s)
runs = run . runError . runState initalState $ myComp
```

However, the order of the handlers does matter for the final result.
`run1` and `run2` show different executions of the same effectful computation.
In `run1`, the returned state `s` is the last state seen before an eventual
exception gets thrown (similar to the semantics in typical imperative
languages), while in `run2` the final state is returned only if the whole
computation succeeded - transaction style.

### Tips and tricks

There are several constructs that make it easier to work with the effects.

If only a part of the result is necessary for the further computation, have a
look at the `eval*` and `exec*` functions, which exist for some effects.
The `exec*` functions discard the result of the computation (the `a` type).
The `eval*` functions discard the final result of the effect.

Instead of writing
`(Member (Exc e) r, Member (State s) r) => ...` it is
possible to use the type operator `<::` and write
`[ Exc e, State s ] <:: r => ...`, which has the same meaning.

## Other Effects

*work in progress*

## Integration with IO

`IO` as well as any other monad can be used as a base type for `Lift` effect.
There may be at most one instance of `Lift` effect in the effects list, and it
must be handled the last. `Control.Eff.Lift` exports `runLift` handler and
`lift` function, that provides an ability to run arbitrary monadic actions.
Also, there are convenient type aliases, that allow for shorter type constraints.

```haskell
f :: IO ()
f = runLift $ do printHello
                 printWorld

-- These two functions' types are equivalent.

printHello :: SetMember Lift (Lift IO) r => Eff r ()
printHello = lift (putStr "Hello")

printWorld :: Lifted IO r => Eff r ()
printWorld = lift (putStrLn " world!")
```

Note that, since `Lift` is a terminal effect, you do not need to use `run` to
extract pure value. Instead, `runLift` returns a value wrapped in whatever monad
you chose to use.

In addition, `Lift` effect provides `MonadBase`, `MonadBaseControl`, and `MonadIO`
instances, that may be useful, especially with packages like [lifted-base](http://hackage.haskell.org/package/lifted-base),
[lifted-async](http://hackage.haskell.org/package/lifted-async), and other
code that uses those typeclasses.

## Integration with Monad Transformers

*work in progress*

## Writing your own Effects and Handlers

*work in progress*

## Background

extensible-effects is based on the work
[Extensible Effects: An Alternative to Monad Transformers](http://okmij.org/ftp/Haskell/extensible/).
The [paper](http://okmij.org/ftp/Haskell/extensible/exteff.pdf) and
the followup [freer paper](http://okmij.org/ftp/Haskell/extensible/more.pdf)
contain details. Additional explanation behind the approach can be found on [Oleg's website](http://okmij.org/ftp/Haskell/extensible/).

## Limitations

### Ambiguity-Flexibility tradeoff
The extensibility of `Eff` comes at the cost of some ambiguity. A useful pattern
to mitigate the ambiguity is to specialize the call to the handler of effects
using [type application](https://ghc.haskell.org/trac/ghc/wiki/TypeApplication)
or type annotation. Examples of this pattern can be seen in
[Example/Test.hs](./test/Control/Eff/Example/Test.hs).

Note, however, that the extensibility can also be traded back, but that detracts
from some of the advantages. For details see section 4.1 in the
[paper](http://okmij.org/ftp/Haskell/extensible/exteff.pdf).

Some examples where the cost of extensibility is apparent:

  * Common functions can't be grouped using typeclasses, e.g.
    the `ask` and `getState` functions can't be grouped with some

    ```haskell
    class Get t a where
      ask :: Member (t a) r => Eff r a
    ```

    `ask` is inherently ambiguous, since the type signature only provides
    a constraint on `t`, and nothing more. To specify fully, a parameter
    involving the type `t` would need to be added, which would defeat the
    point of having the grouping in the first place.
  * Code requires greater number of type annotations. For details see
    [#31](https://github.com/suhailshergill/extensible-effects/issues/31).

### Current implementation only supports GHC version 7.8 and above
This is not a fundamental limitation of the design or the approach, but there is
an overhead with making the code compatible across a large number of GHC
versions. If this is needed, patches are welcome :)
