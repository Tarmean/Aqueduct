module Main where

import Aqueduct
import Data.Functor (void)


sumP :: Monad m => Producer Int m () -> Effect m Int
sumP = fold (+) 0 id

each :: Monad m => [a] -> Producer a m ()
each = void . traverse yield 

printP :: Show a => Consumer a IO ()
printP = do
  cur <- await
  lift $ print cur
  printP

rollingSum :: Int -> Consumer Int IO a
rollingSum n = do
  x <- await
  lift $ print (x + n)
  rollingSum (x+n)


main :: IO ()
main =  runEffect $ server `pull` client

server ::Int ->  Pipe x' x Int Int IO r
server i = do
  lift (print i) 
  server =<< yieldDownstream (i*3)
     

client :: Pipe Int Int x' x IO ()
client = do
  lift . print =<< yieldUpstream 3
  lift . print =<< yieldUpstream 4
  lift . print =<< yieldUpstream 5
