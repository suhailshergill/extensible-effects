
# Extensible effects

[![Build Status](https://travis-ci.org/suhailshergill/extensible-effects.svg?branch=master)](https://travis-ci.org/suhailshergill/extensible-effects)
[![Join the chat at https://gitter.im/suhailshergill/extensible-effects](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/suhailshergill/extensible-effects?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Stories in Ready](https://badge.waffle.io/suhailshergill/extensible-effects.png?label=ready&title=Ready)](http://waffle.io/suhailshergill/extensible-effects)
[![Stories in progress](https://badge.waffle.io/suhailshergill/extensible-effects.png?label=in%20progress&title=In%20progress)](http://waffle.io/suhailshergill/extensible-effects)

*Implement effectful computation in a modular way!*

The main and only monad is built upon `Eff` from `Control.Eff`. `Eff r a` is parameterized by
the effect-list `r` and the monadic-result type `a` similar to other monads.
It is the intention that all other monadic computations can be replaced by the use of `Eff`.

In case you know monad transformers or `mtl`: This library provides only one monad that includes all your effects instead of layering different transformers. It is not necessary to lift the computations through a monad stack. Also, it is not required to lift every `Monad*` typeclass (like `MonadError`) though all transformers.

## Quickstart

To experiment with this library, it is suggested to write some lines within `ghci`. This section will include some code-examples, which you should try on your own!

Recommended Procedure:

1. add `extensible-effects` to a existing cabal or stack project or `git clone https://github.com/suhailshergill/extensible-effects.git`
2. start `stack ghci` or `cabal repl`
3. import some library modules as described in this section

(examples are work in progress, maybe also a Quickstart-module)

### The effect list

```
import Control.Eff
```

The effect list `r` in the type `Eff r a` is a central concept in this library.
It is a type-level list containing effect types.

If `r` is the empty list, then the computation `Eff r` (or `Eff '[]`) does not contain any effects to be handled and therefore is a pure computation. In this case, the result value can be retrieved by `run :: Eff '[] a -> a`

For programming within the `Eff r` monad, it is almost never necessary to list all effects that can appear.
It suffices to state what types of effects are at least required.
This is done via the `Member t r` typeclass. It describes that the type `t` occurs inside the list `r`.
I you really want, you can still list all Effects and their order in which they are used (e.g. `Eff '[Reader r, State s] a`).

### Lifecycle of an effect

Programming with effects as implemented in this library is done with two complementary concepts:

* requesting an effect: when programming an effectful computation, the requests can be made to be executed when handled. This is done via the provided functions
* handling an effect: the handler responds to the request by executing the effect

Effect-handler functions, mostly called `run*`, `exec*` or `eval*`, typically have a signature with something like `Eff (x ': r) a -> Eff r a`. Where `x` is the type of effect to be handled, like `Writer w` or `State s`. The transformation from the longer list of effects `(x ': r)` to just `r` is a type-level indicator that the effect `x` has been handled. Depending on the effect, some additional input might be required or some different output than just `a` is produced.

### Most common effects

The most common effects used are `Writer`, `Reader`, `Exception` and `State`.
For the basic effects, the types of those functions clearly state the intent of the effect.

In this section, only the core functions of an effect are presented. Have a look at the modules to see more ways of programming with the effect.

#### The Exception Effect

```
import Control.Eff.Exception
```

The exception effect adds the possibility to exit a computation preemptively with an exception. Note that the exceptions from this library are handled by the programmer and have nothing to do with exceptions thrown inside the Haskell run-time.

```haskell
throwError :: Member (Exc e) r => e -> Eff r a
runError :: Eff (Exc e ': r) a -> Eff r (Either e a)
```

An exception can be thrown using the `throwError` function. Its return type is `Eff r a` with an arbitrary type `a`. When handling the effect, the result-type changes to `Either e a` instead of
just `a`. This indicates how the effect is handled: The returned value is either the thrown exception or the value returned.

#### The State Effect

```
import Control.Eff.State.{Lazy | Strict}
```

The state effect provides readable and writable state during a computation.

```haskell
get :: Member (State s) r => Eff r s
put :: Member (State s) r => s -> Eff r ()
modify :: Member (State s) r => (s -> s) -> Eff r ()
runState :: s -> Eff (State s ': r) a -> Eff r (a, s)
```

The `get` functions accesses the current state and makes it usable within the further computation.
The `put` function sets the state to the given value.
`modify` updates the state using a mapping function by combining `get` and `put`.

The state-effect is handled using the `runState` function. It takes the initial state as an argument and returns the final state and effect-result.

#### The Reader Effect

The reader effect provides an environment that can be read. Sometimes it is considered as read-only state.

```haskell
ask :: Member (Reader e) r => e -> Eff r e
runReader :: e -> Eff (Reader e ': r) a -> Eff r a
```

The environment given to the handle the reader effect is the one given during the computation if asked for.

There are two variants of readers: strict and lazy. Each has its own module and provide the same interface. By importing one or the other, it can be controlled if the reader is strict or lazy in its environment argument.

#### The Writer Effect

The writer effect allows to output messages during a computation. It is sometimes referred to as write-only state, which gets retrieved.

```haskell
tell :: Member (Writer w) r => w -> Eff r ()
runWriter :: (w -> b -> b) -> b -> Eff (Writer w ': r) a -> Eff r (a, b)
runListWriter :: Eff (Writer w ': r) a -> Eff r (a, [w])
```

running a writer can be done in several ways. The most general function is `runWriter` that folds over all written values. However, if you only want to collect the the values written, the `runListWriter` function does that.

Note that compared to mtl, the value written has no Monoid constraint on it and can be collected in any way.

### Using multiple effects

The main benefit of this library is that multiple effects can be included without much changes necessary.

If you need state and want to be able exit the computation with an exception, the type of your effectful computation would be: `myComp :: (Member (Exc e) r, Member (State s) r) => Eff r a`. Then, both the state and exception effect-functions can be used. To handle the effects, both `run*` functions have to be provided. `run . runState initalState . runError $ myComp` would run the computation and return something of type `(Either e a, s)`. Where `s` is the last state seen before an eventual exception gets thrown.

As shown in the previous example, it is important in which order the effect handlers are listed. Consider the difference between `run . runState initalState . runError $ myComp :: (Either e a, s)` and `run . runError . runState initalState $ myComp :: Either e (a, s)`. The former one returns the last-seen state before an eventual exception (similar to the semantics in typical imperative languages), while the latter one only gives the resulting state if the computation succeeded as a whole - transaction style.

example is work in progress

### Tips and tricks

There are several constructs that make it easier to work with the effects.

If only a part of the result is necessary for the further computation, have a look at the `eval*` and `exec*` functions, which exist for some effects. The `exec*` functions discard the result of the computation (the `a` type). The `eval*` functions discard the final result of the effect

Instead of writing `(Member (Writer w) r, Member (Reader e) r, Member (Exc ex) r) => ...` it is possible to use the type operator `<::` and write `[ Writer w, Reader e, Exc ex ] <:: r => ...`.

## Other Effects

work in progress

## Integration with Monad Transformers

work in progress

## Integration with IO

work in progress

## Writing your own Effects and Handlers

work in Progress

## Background

extensible-effects is based on the work
[Extensible Effects: An Alternative to Monad Transformers](http://okmij.org/ftp/Haskell/extensible/).
The [paper](http://okmij.org/ftp/Haskell/extensible/exteff.pdf) and
the followup [freer paper](http://okmij.org/ftp/Haskell/extensible/more.pdf)
contain details. Additional explanation behind the approach can be found on [Oleg's website](http://okmij.org/ftp/Haskell/extensible/).

## Limitations

### Current implementation only supports GHC version 7.8 and above
This is not a fundamental limitation of the design or the approach, but there is
an overhead with making the code compatible across a large number of GHC
versions. If this is needed, patches are welcome :)

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
