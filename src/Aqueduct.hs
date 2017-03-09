module Aqueduct (Gen, Iter, Effect, runEffect, yield, await, (>->), lift, fold, streamInto) where
import Control.Monad ((>=>))
import Data.Void (Void)
import Control.Monad.Trans

data Pipe b' a a' b m r = West b  (a' -> Pipe b' a a' b m r) -- yield/await to the left
                        | East b' (a  -> Pipe b' a a' b m r) -- yield/await to the right
                        | Done r                             -- computation finished
                        | Context (m (Pipe b' a a' b m r))   -- interpreting the free monad with runEffect joins the contexts

type Gen b m r  = Pipe Void () () b m r    -- can only send values
type Iter a m r = Pipe () a Void () m r    -- can only receive values
type Effect m r = Pipe Void () Void () m r -- can't interact with other pipes

runEffect :: Monad m => Effect m r -> m r
runEffect (Context m) = m >>= runEffect
runEffect (Done r)    = return r
runEffect (West _ _)  = undefined
runEffect (East _ _)  = undefined

yield :: b -> Pipe a' a () b m ()
yield b = West b (const (Done ()))
await :: Pipe () a b' b m a
await = East () Done
(>->) :: Monad m => Pipe a' a b' b m r -> Pipe b' b c' c m r -> Pipe a' a c' c m r
p1 >-> p2 = const p1 `pull` p2

fold :: Monad m => (acc -> b -> acc) -> acc -> (acc -> r) -> Gen b m () -> Effect m r
fold step start end = go start
  where
    go acc (Done ()) = Done $ end acc
    go acc (Context m) = Context $ fmap (go acc) m
    go acc (West b f) = go (step acc b) (f ())
    go _   (East _ _) = undefined

streamInto :: Monad m => Gen b m r -> (b -> Gen c m s) -> Gen c m r
(Done result) `streamInto` _ = Done result
(Context m) `streamInto` f = Context $ fmap (`streamInto` f) m
(East _ _) `streamInto` _ = undefined
(West v c) `streamInto` f = f v >> c () `streamInto` f

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

instance MonadTrans (Pipe b' a a' b) where
  lift v = Context $ Done <$> v