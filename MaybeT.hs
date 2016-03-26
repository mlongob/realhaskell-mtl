{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}
module MaybeT where

import Control.Monad (ap, liftM)
import Control.Monad.State (MonadState, get, put)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (MonadTrans, lift)

newtype MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
}

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do
                unwrapped <- runMaybeT x
                case unwrapped of
                    Nothing -> return Nothing
                    Just y -> runMaybeT (f y)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance (Functor f) => Functor (MaybeT f) where
    fmap = (<$>)

instance (Monad m) => Applicative (MaybeT m) where
    (<*>) = ap
    pure = returnMT

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (>>=) = bindMT
    fail = failMT

instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
    get = lift get
    put k = lift (put k)
