
module Main where

import Control.Monad.State (State)
import qualified Control.Monad.State as StateM (evalState)
import qualified Control.Monad.Trans.Except as Except (runExceptT)

import Err    (CError(..))
import Mstate (MS(unMS), getState, putState, throwError)


processData :: String -> MS String String
processData [] = do
     curr <- getState
     return curr
processData (x:xs) = do
     curr <- getState
     case x of
          'x' -> throwError XError
          'y' -> throwError YError
          _   -> do
                  putState (x:curr)
                  processData xs


-- processData :: String -> MS String String
--
-- unMS :: MS s a -> ExceptT CError (State s) a
-- unMS :: MS String String -> ExceptT CError (State s) a
--
-- runExceptT :: ExceptT e m a -> m (Either e a)
-- runExceptT :: ExceptT CError (State s) a -> State String (Either CError String)
runExceptT :: String -> State String (Either CError String)
runExceptT s = Except.runExceptT . unMS $ processData s


evalState :: String -> Either CError String
evalState s = StateM.evalState (runExceptT s) []


main :: IO ()
main = do
    word <- getLine
    let x = evalState word
    print x
