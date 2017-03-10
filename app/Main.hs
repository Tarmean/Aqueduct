module Main where

import Aqueduct
import Data.Functor (void)
main :: IO ()
main = runEffect $ each [1..50000] >-> forever' (lift . print =<< await)
forever' a = let a' = a #> a' in a'
f #> g = f >>= \_ -> g

each :: Monad m => [a] -> Producer a m ()
each = foldr (\a p -> yield a >> p) (return ()) 
