{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module Control.Error.Util where

import           Control.Monad
import           Control.Monad.Error.Class
import           Data.Maybe

infixl 0 ??
infixl 0 !?

(??) :: MonadError m => Maybe a -> ErrorType m -> m a
aMay ?? e =
    case aMay of
        Nothing -> throwError e
        Just a  -> return a


(!?) :: MonadError m => m (Maybe a) -> ErrorType m -> m a
aM !? e = do
    aMay <- aM
    aMay ?? e

