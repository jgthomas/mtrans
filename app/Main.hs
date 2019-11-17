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


getState = MS (lift get)


putState s = MS $ lift $ put s


throwError e = MS $ throwE e


dumb :: String -> MS String String
dumb [] = do
     curr <- getState
     return curr
dumb (x:xs) = do
     curr <- getState
     case x of
          'x' -> throwError XError
          _   -> do
                  putState (x:curr)
                  dumb xs


dumber :: String -> State String (Either CError String)
dumber s = runExceptT . unMS $ dumb s


dumbest :: String -> Either CError String
dumbest s = evalState (runExceptT . unMS $ dumb s) []


main :: IO ()
main = do
    word <- getLine
    let x = dumbest word
    print x
