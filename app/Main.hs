module Main where

import Aqueduct
import Data.Functor (void)
import Control.Monad (replicateM_)
import qualified Data.Foldable as F


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
main =  runEffect $ const pairR `pull` pairL
pairR :: Pipe x x'  Int String IO ()
pairR = replicateM_ 3 $ yield "foo" >>= lift . print

pairL :: Pipe Int String x' x IO ()
pairL = F.fold [ yieldUp 3, yieldUp 4, yieldUp 5 ] >>= lift . print
