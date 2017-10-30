{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Error.MonadErrorInstance where

import           Control.Category           ((.))
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except


instance Monad m => MonadError (ExceptT e m) where
    type ErrorType (ExceptT e m) = e
    throwError = throwE
    catchError = catchE


instance MonadState m => MonadState (ExceptT e m) where
    type StateType (ExceptT e m) = StateType m
    get = lift get
    put = lift . put
