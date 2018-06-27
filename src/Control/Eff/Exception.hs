{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe #-}
-- | Exception-producing and exception-handling effects
module Control.Eff.Exception ( Exc (..)
                            , Fail
                            , throwError
                            , throwError_
                            , die
                            , runError
                            , runFail
                            , catchError
                            , onFail
                            , rethrowError
                            , liftEither
                            , liftEitherM
                            , liftMaybe
                            , liftMaybeM
                            , ignoreFail
                            ) where

import Control.Eff
import Control.Eff.Extend
import Control.Eff.Lift

import Control.Monad (void)
import Control.Monad.Base
import Control.Monad.Trans.Control

-- ------------------------------------------------------------------------
-- | Exceptions
--
-- exceptions of the type e; no resumption
newtype Exc e v = Exc e

instance ( MonadBase m m
         , SetMember Lift (Lift m) r
         , MonadBaseControl m (Eff r)
         ) => MonadBaseControl m (Eff (Exc e ': r)) where
    type StM (Eff (Exc e ': r)) a = StM (Eff r) (Either e a)
    liftBaseWith f = raise $ liftBaseWith $ \runInBase ->
                       f (runInBase . runError)
    restoreM x = do r :: Either e a <- raise (restoreM x)
                    liftEither r

type Fail = Exc ()

-- | Throw an exception in an effectful computation. The type is inferred.
throwError :: (Member (Exc e) r) => e -> Eff r a
throwError e = send (Exc e)
{-# INLINE throwError #-}

-- | Throw an exception in an effectful computation. The type is unit,
--   which suppresses the ghc-mod warning "A do-notation statement
--   discarded a result of type"
throwError_ :: (Member (Exc e) r) => e -> Eff r ()
throwError_ = throwError
{-# INLINE throwError_ #-}

-- | Makes an effect fail, preventing future effects from happening.
die :: Member Fail r => Eff r a
die = throwError ()
{-# INLINE die #-}

-- | Run a computation that might produce an exception.
runError :: Eff (Exc e ': r) a -> Eff r (Either e a)
runError = handle_relay
  (return . Right)
  (\(Exc e) _k -> return (Left e))

-- | Runs a failable effect, such that failed computation return 'Nothing', and
--   'Just' the return value on success.
runFail :: Eff (Fail ': r) a -> Eff r (Maybe a)
runFail = fmap (either (const Nothing) Just) . runError
{-# INLINE runFail #-}

-- | Run a computation that might produce exceptions, and give it a way to deal
-- with the exceptions that come up. The handler is allowed to rethrow the
-- exception
catchError :: Member (Exc e) r =>
        Eff r a -> (e -> Eff r a) -> Eff r a
catchError m handle = interpose return (\(Exc e) _k -> handle e) m

-- | Add a default value (i.e. failure handler) to a fallible computation.
-- This hides the fact that a failure happened.
onFail :: Eff (Fail ': r) a -- ^ The fallible computation.
       -> Eff r a           -- ^ The computation to run on failure.
       -> Eff r a
onFail e handle = runFail e >>= maybe handle return
{-# INLINE onFail #-}

-- | Run a computation until it produces an exception,
-- and convert and throw that exception in a new context.
rethrowError :: (Member (Exc e') r)
           => (e -> e')
           -> Eff (Exc e ': r) a
           -> Eff r a
rethrowError t eff = runError eff >>= either (throwError . t) return

-- | Treat Lefts as exceptions and Rights as return values.
liftEither :: (Member (Exc e) r) => Either e a -> Eff r a
liftEither = either throwError return
{-# INLINE liftEither #-}

-- | `liftEither` in a lifted Monad
liftEitherM :: (Member (Exc e) r, SetMember Lift (Lift m) r)
            => m (Either e a)
            -> Eff r a
liftEitherM m = lift m >>= liftEither
{-# INLINE liftEitherM #-}

-- | Lift a maybe into the 'Fail' effect, causing failure if it's 'Nothing'.
liftMaybe :: Member Fail r => Maybe a -> Eff r a
liftMaybe = maybe die return
{-# INLINE liftMaybe #-}

-- | `liftMaybe` in a lifted Monad
liftMaybeM :: (Member Fail r, SetMember Lift (Lift m) r)
           => m (Maybe a)
           -> Eff r a
liftMaybeM m = lift m >>= liftMaybe
{-# INLINE liftMaybeM #-}

-- | Ignores a failure event. Since the event can fail, you cannot inspect its
--   return type, because it has none on failure. To inspect it, use 'runFail'.
ignoreFail :: Eff (Fail ': r) a
           -> Eff r ()
ignoreFail e = void e `onFail` return ()
{-# INLINE ignoreFail #-}
