{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mstate (MS(unMS), getState, putState, throwError) where

import Control.Monad.State
import Control.Monad.Trans.Except

import Err (CError)


newtype MS s a = MS { unMS :: ExceptT CError (State s) a }
  deriving
    (  Functor
     , Applicative
     , Monad
     )


getState :: MS a a
getState = MS (lift get)


putState :: s -> MS s ()
putState s = MS $ lift $ put s


throwError :: CError -> MS s a
throwError e = MS $ throwE e
