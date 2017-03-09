module Main where

import Aqueduct
import Data.Functor (void)

main :: IO ()
main = runEffect $ makeNumbers >-> sumStuff 0

makeNumbers :: Monad m => Gen Int m ()
makeNumbers = void $ traverse yield [1..10]

sumStuff :: Int -> Iter Int IO a
sumStuff n = do
  x <- await
  lift $ print (x + n)
  sumStuff (x+n)
