{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module Control.Error.Util where

import           Control.Monad
import           Control.Monad.Error.Class
import           Data.Either
import           Data.Function             (($))
import           Data.Maybe

infixl 0 ??
infixl 0 !?


class WithError t where
    type ErrorHandlerType t e :: *
    (??) :: MonadError m => t a -> ErrorHandlerType t (ErrorType m) -> m a

    (!?) :: MonadError m => m (t a) -> ErrorHandlerType t (ErrorType m) -> m a
    aM !? e = do
        aErr <- aM
        aErr ?? e


instance WithError Maybe where
    type ErrorHandlerType Maybe e = e
    aMay ?? e =
        case aMay of
            Nothing -> throwError e
            Just a  -> return a


instance WithError (Either e) where
    type ErrorHandlerType (Either e') e = e' -> e
    aEither ?? eF =
        case aEither of
            Left e  -> throwError $ eF e
            Right a -> return a

