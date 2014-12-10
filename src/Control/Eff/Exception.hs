{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-- | Exception-producing and exception-handling effects
module Control.Eff.Exception( Exc (..)
                            , Fail
                            , throwExc
                            , die
                            , runExc
                            , runFail
                            , catchExc
                            , onFail
                            , rethrowExc
                            , liftEither
                            , liftEitherM
                            , liftMaybe
                            , ignoreFail
                            ) where

import Control.Monad (void)
import Data.Typeable

import Control.Eff
import Control.Eff.Lift

#if MIN_VERSION_base(4,7,0)
#define Typeable1 Typeable
#endif

-- | These are exceptions of the type e. This is akin to the error monad.
newtype Exc e v = Exc e
    deriving (Functor, Typeable)

type Fail = Exc ()

-- | Throw an exception in an effectful computation.
throwExc :: (Typeable e, Member (Exc e) r) => e -> Eff r a
throwExc e = send (\_ -> inj $ Exc e)
{-# INLINE throwExc #-}

-- | Makes an effect fail, preventing future effects from happening.
die :: Member Fail r => Eff r a
die = throwExc ()
{-# INLINE die #-}

-- | Run a computation that might produce an exception.
runExc :: Typeable e => Eff (Exc e :> r) a -> Eff r (Either e a)
runExc = loop . admin
 where
  loop (Pure x)  = return (Right x)
  loop (Free u)    = handleRelay u loop (\(Exc e) -> return (Left e))

-- | Runs a failable effect, such that failed computation return 'Nothing', and
--   'Just' the return value on success.
runFail :: Eff (Fail :> r) a -> Eff r (Maybe a)
runFail = fmap (either (\_-> Nothing) Just) . runExc
{-# INLINE runFail #-}

-- | Run a computation that might produce exceptions,
-- and give it a way to deal with the exceptions that come up.
catchExc :: (Typeable e, Member (Exc e) r)
         => Eff r a
         -> (e -> Eff r a)
         -> Eff r a
catchExc m handle = loop (admin m)
 where
  loop (Pure x)  = return x
  loop (Free u)    = interpose u loop (\(Exc e) -> handle e)

-- | Add a default value (i.e. failure handler) to a fallible computation.
-- This hides the fact that a failure happened.
onFail :: Eff (Fail :> r) a -- ^ The fallible computation.
       -> Eff r a           -- ^ The computation to run on failure.
       -> Eff r a
onFail e handle = runFail e >>= maybe handle return
{-# INLINE onFail #-}

-- | Run a computation until it produces an exception,
-- and convert and throw that exception in a new context.
rethrowExc :: (Typeable e, Typeable e', Member (Exc e') r)
           => (e -> e')
           -> Eff (Exc e :> r) a
           -> Eff r a
rethrowExc t eff = runExc eff >>= either (throwExc . t) return

-- | Treat Lefts as exceptions and Rights as return values.
liftEither :: (Typeable e, Member (Exc e) r) => Either e a -> Eff r a
liftEither (Left e) = throwExc e
liftEither (Right a) = return a
{-# INLINE liftEither #-}

-- | `liftEither` in a lifted Monad
liftEitherM :: (Typeable1 m, Typeable e, Member (Exc e) r, SetMember Lift (Lift m) r)
            => m (Either e a)
            -> Eff r a
liftEitherM m = lift m >>= liftEither
{-# INLINE liftEitherM #-}

-- | Lift a maybe into the 'Fail' effect, causing failure if it's 'Nothing'.
liftMaybe :: Member Fail r => Maybe a -> Eff r a
liftMaybe = maybe die return
{-# INLINE liftMaybe #-}

-- | Ignores a failure event. Since the event can fail, you cannot inspect its
--   return type, because it has none on failure. To inspect it, use 'runFail'.
ignoreFail :: Eff (Fail :> r) a
           -> Eff r ()
ignoreFail e = void e `onFail` return ()
{-# INLINE ignoreFail #-}
