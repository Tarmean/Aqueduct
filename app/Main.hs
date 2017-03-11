module Main where

import Aqueduct
import Control.Monad (forM_)


forever' :: Monad m => m a -> m b
forever' a = a >> forever' a

printLoop :: Show a => Pipe () a a' b IO ()
printLoop = do
  input <- await
  lift . print $ input

main :: IO ()
main = runEffect $  each [1..100000::Integer] ~> lift . print

infixl 5 ~>
(~>) :: Monad m => Pipe x' x b' b m r -> (b -> Pipe b' b c' c m b') -> Pipe x' x c' c m r
p ~> f = p >>~ go
  where go x = go =<< yieldUpstream =<< f x

-- yieldUpstream :: b' -> Pipe b' a a' b m a
-- yieldUpstream b = East b Done

lhs :: Pipe x' x String String IO ()
lhs = forM_ [1..10::Int] $ \i -> do
  r <- yieldDownstream $ "from upstream nr. " ++ show i
  lift . putStrLn $ "lhs: " ++ show r

rhs :: String -> Pipe String String  x x' IO String
rhs x = do
  lift . putStrLn $ "rhs 1: " ++ show x
  y <- yieldUpstream "yield manually upstream!"
  lift . putStrLn $ "rhs 2: " ++ show y
  return "return upstream"
  

each :: Monad m => [a] -> Producer a m ()
each = foldr (\a p -> yield a >> p) (return ()) 

(>>~) :: Monad m => Pipe a' a b' b m r -> (b -> Pipe b' b c' c m r) -> Pipe a' a c' c m r
(>>~) = push


--(>~) :: Monad m => (Pipe x' x y' y m r) -> Pipe b' r c' c m r -> Pipe a' a c' c m r
-- s >~ p = go `into` p
--   where 
--     go = do
--       cur <- s
--       yield s
--       go
          
