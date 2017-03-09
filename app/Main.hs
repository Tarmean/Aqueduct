module Main where

import Aqueduct
import Data.Functor (void)

main :: IO ()
main = runEffect $ do
  each [1..10] >-> rollingSum 0

  lift $ putStrLn "-----"

  summed <- sumP (each [1..10])
  lift $ print summed >> putStrLn "-----"

  each [3..5::Int] `streamInto` (each . enumFromTo 1) >-> printP

sumP :: Monad m => Gen Int m () -> Effect m Int
sumP = fold (+) 0 id

each :: Monad m => [a] -> Gen a m ()
each = void . traverse yield 

printP :: Show a => Iter a IO ()
printP = do
  cur <- await
  lift $ print cur
  printP

rollingSum :: Int -> Iter Int IO a
rollingSum n = do
  x <- await
  lift $ print (x + n)
  rollingSum (x+n)
