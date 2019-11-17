{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.State
import Control.Monad.Trans.Except


data CError = XError
            | YError
            deriving Show


newtype MS s a = MS { unMS :: ExceptT CError (State s) a }
  deriving
    (  Functor
     , Applicative
     , Monad
     )


dumb :: String -> MS String String
dumb [] = do
     curr <- MS (lift get)
     return curr
dumb (x:xs) = do
     curr <- MS (lift get)
     MS $ lift $ put (x:curr)
     dumb xs


dumber :: String -> State String (Either CError String)
dumber s = runExceptT . unMS $ dumb s


dumbest :: String -> Either CError String
dumbest s = evalState (runExceptT . unMS $ dumb s) []


main :: IO ()
main = do
    let x = dumbest "dog"
    print x
