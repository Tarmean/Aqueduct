{-# LANGUAGE BangPatterns #-}
module Aqueduct (Pipe, Producer, Consumer, Effect, runEffect, yield, await, (>->), lift, fold, into, outof, yieldDownstream, yieldUpstream, push, pull, Void) where
import Control.Monad ((>=>))
import Data.Void (Void)
import Control.Monad.Trans

data Pipe b' a a' b m r = West b  (a' -> Pipe b' a a' b m r) -- yield/await to the left
                        | East b' (a  -> Pipe b' a a' b m r) -- yield/await to the right
                        | Done r                             -- computation finished
                        | Context (m (Pipe b' a a' b m r))   -- interpreting the free monad with runEffect joins the contexts

type Producer b m r  = Pipe Void () () b m r    -- can only send values
type Consumer a m r = Pipe () a Void () m r    -- can only receive values
type Effect m r = Pipe Void () Void () m r -- can't interact with other pipes

runEffect :: Monad m => Effect m r -> m r
runEffect (Context m) = m >>= runEffect
runEffect (Done r)    = return r
runEffect (West _ _)  = undefined
runEffect (East _ _)  = undefined

yield :: b -> Pipe b' a a' b m a'
yield b = West b Done
yieldUpstream :: b' -> Pipe b' a a' b m a
yieldUpstream b = East b Done
await :: Pipe () a a' b m a
await = East () Done
yieldDownstream :: b -> Pipe b' a a' b m a'
yieldDownstream = yield


(>->) :: Monad m => Pipe a' a b' b m r -> Pipe b' b c' c m r -> Pipe a' a c' c m r
p1 >-> p2 = const p1 `pull` p2

fold :: Monad m => (acc -> b -> acc) -> acc -> (acc -> r) -> Producer b m () -> Effect m r
fold step start end = go start
  where
    go acc (Done ()) = Done $ end acc
    go acc (Context m) = Context $ fmap (go acc) m
    go acc (West b f) = let !acc' = step acc b in go acc'(f ())
    go _   (East _ _) = undefined


into :: Monad m => Pipe x' x b' b m r -> (b -> Pipe x' x c' c m b') -> Pipe x' x c' c m r
(Done result) `into` _ = Done result
(Context m) `into` f = Context $ fmap (`into` f) m
(East v cont) `into` f = East v $ (`into`f) . cont
(West v cont) `into` f = f v >>= \x -> cont x `into` f

-- specialized: streamResults :: Consumer a m b -> Consumer b m r -> Consumer a m r
outof :: Monad m => Pipe a' a x' x m b -> Pipe b' b x' x m r ->  Pipe a' a x' x m r
outof = go . const
  where
    go :: Monad m => (b' -> Pipe a' a x' x m b) -> Pipe b' b x' x m r ->  Pipe a' a x' x m r
    go _ (Done result) = Done result
    go f (East v cont) = f v >>= go f . cont
    go f (West v cont) = West v $ go f . cont
    go f (Context m)   =  Context $ fmap (go f) m



pull :: Monad m => (b' -> Pipe a' a b' b m r) -> Pipe b' b c' c m r -> Pipe a' a c' c m r
_ `pull` Done result  = Done result
f `pull` Context m    = Context $ fmap (f `pull`) m
f `pull` West c  cont = West c $ (f `pull`) . cont
f `pull` East b' cont = f b' `push` cont

push :: Monad m => Pipe a' a b' b m r -> (b -> Pipe b' b c' c m r) -> Pipe a' a c' c m r
Done result  `push` _ = Done result
Context m    `push` f = Context $ fmap (`push` f) m
West b  cont `push` f = cont `pull` f b
East a' cont `push` f = East a' $ (`push` f) . cont

instance (Monad m) => Applicative (Pipe b' a a' b m) where
  pure = return
  f <*> a = do { f' <- f; a' <- a; return $ f' a' }
instance (Monad m) => Functor (Pipe b' a a' b m) where
  fmap f a = do { a' <- a; return $ f a' }
instance (Monad m) => Monad (Pipe b' a a' b m) where
  return = Done
  Done r >>= f       = f r
  Context m >>= f    = Context $ fmap (>>=f) m
  West b  cont >>= f = West b (cont >=> f)
  East b' cont >>= f = East b' (cont >=> f)

instance (Monad m, Monoid r) => Monoid (Pipe b' a a' b m r) where
  mempty = return mempty
  l `mappend` r = mappend <$> l <*> r
instance MonadTrans (Pipe b' a a' b) where
  lift v = Context $ v >>= \r -> return (Done r)