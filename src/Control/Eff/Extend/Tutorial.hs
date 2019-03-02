{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
-- | This is a tutorial on implementing custom effects.
--
-- == What is an effect?
--
-- An effect is made up of a few parts.
--
-- === External
--
-- This is the external interface of the effect that users of the effect use.
--
-- 1. __The DSL syntax__: the primitives that this effect enables.
--
-- 2. __DSL interpreter (aka handler)__: the function which interprets the DSL.
--
-- === Internal
--
-- In order to implement the above, the implementer has to, additionally, think
-- of:
--
-- 1. __Request messages__: the messages that are dispatched to the handler when
-- the DSL is used.
--
-- 2. __Interpreter logic__: how to respond to the requests.
--
-- == How do we implement an effect?
--
-- Each of the four parts above corresponds to something that's needed for
-- implementing the effect:
--
-- 1. __A datatype (/internal/)__: these define the /requests/ that the handler
-- receives.
--
-- 2. __Smart constructors (/external/)__: these define the /DSL syntax/ that
-- users of the effect will use.
--
-- 3. __A 'Control.Eff.Extend.Handle' instance (/internal/)__: this defines the
-- /interpreter logic/, i.e., how the handler responds to the requests.
--
-- 4. __A handler using the 'Control.Eff.Extend.Handle' instance (/external/)__:
-- this is the /handler/ that users of the effect invoke. We define this by:
--
--     * invoking 'Control.Eff.Extend.handle_relay'
--     * passing an argument which specifies how to embed pure values
--     * tying the recursive-knot using 'Data.Function.fix'
--
-- __TODO__: The above might benefit from some template-haskell to reduce
-- boilerplate.
--
-- == The effect design process
--
-- * As with all things, this starts with a name (say @t@). The name of the
-- effect is also the name we give the datatype.
--
-- * Having decided the name of the effect, we need to determine the DSL
-- syntax. The syntax names are shared both by the data constructors as well as
-- the smart constructors.
--
-- * We need to define the types for the DSL syntax. Since this is an effectful
-- DSL, each DSL component will have a type of the form:
--
--     * @Member t r => Eff r b@, or
--     * @Member t r => a0 -> Eff r b@.
--     * @Member t r => a0 -> a1 -> Eff r b@.
--     * ...
--
-- * Having decided the types, we have to think about the implementation.
--
-- Let's implement a DSL dealing with some basic arithmetic operations:
-- @addition, minimum@.
module Control.Eff.Extend.Tutorial where

import Control.Eff
import Control.Eff.Extend
import Data.Function (fix)

-- | 'Arith' DSL syntax for constant literals.
lit :: Member Arith r => Integer -> Eff r Integer
-- | 'Arith' DSL syntax to addition of two numbers.
add :: Member Arith r => Eff r Integer -> Eff r Integer -> Eff r Integer
-- | 'Arith' DSL syntax for minimum of two numbers.
min :: Member Arith r => Eff r Integer -> Eff r Integer -> Eff r Integer

-- | The request messages sent to the /handler/.
data Arith a where
  -- | Introduce integer literals
  Lit :: Integer -> Arith Integer
  -- | Addition of two numbers. Per the signature the specific addition
  -- operation is provided by the handler
  Add :: Arith (Integer -> Integer -> Integer)
  -- | Minimum of two numbers. Per the signature the specific minimum operation
  -- is provided by the handler.
  Min :: Arith (Integer -> Integer -> Integer)

-- | Send the @Lit@ request.
lit x = send (Lit x)
-- | Send the @Add@ request.
add x y = send Add <*> x <*> y
-- | Send the @Min@ request.
min x y = send Min <*> x <*> y

-- | The /handler/ logic.
instance Handle Arith r a (Eff r' Integer) where
  handle h q = \case
    Lit x -> pure x
    Add   -> k (+)
    Min   -> k Prelude.min
    where k = qComp q h

-- | The handler
runArith :: Eff (Arith ': r) Integer -> Eff r Integer
runArith = fix (handle_relay pure)
