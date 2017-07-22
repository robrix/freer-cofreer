{-# LANGUAGE RankNTypes #-}
import Control.Monad.Free.Freer hiding (retract, iterFreer, iterFreerA)
import Criterion.Main
import Data.Functor.Foldable
import Prelude hiding (read)

main :: IO ()
main = defaultMain
  [ bgroup "retract"
    [ bgroup "iteration-catamorphism" (b retract <$> [30])
    , bgroup "iteration-recursion" (b retract' <$> [30])
    , bgroup "direct-recursion" (b retract'' <$> [30])
    ]
  ]
  where b f i = bench (show i) (whnf (f . fibM) i)

-- iteration-catamorphism

retract :: Monad m => Freer m a -> m a
retract = iterFreerA (>>=)
{-# INLINE retract #-}

iterFreer :: (forall x. f x -> (x -> a) -> a) -> Freer f a -> a
iterFreer algebra = cata $ \ r -> case r of
  ReturnF result -> result
  MapF f action -> algebra action f
  ThenF action continue -> algebra action continue
{-# INLINE iterFreer #-}

iterFreerA :: Applicative m => (forall x. f x -> (x -> m a) -> m a) -> Freer f a -> m a
iterFreerA algebra r = iterFreer algebra (fmap pure r)
{-# INLINE iterFreerA #-}


-- iteration-recursion

retract' :: Monad m => Freer m a -> m a
retract' = iterFreerA' (>>=)
{-# INLINE retract' #-}

iterFreer' :: (forall x. f x -> (x -> a) -> a) -> Freer f a -> a
iterFreer' algebra = go
  where go (Return result) = result
        go (Map f action) = algebra action f
        go (Seq f action1 action2) = algebra action1 (go . flip fmap action2 . f)
        go (Then action continue) = algebra action (go . continue)
        {-# INLINE go #-}
{-# INLINE iterFreer' #-}

iterFreerA' :: Applicative m => (forall x. f x -> (x -> m a) -> m a) -> Freer f a -> m a
iterFreerA' algebra r = iterFreer' algebra (fmap pure r)
{-# INLINE iterFreerA' #-}


-- direct-recursion

retract'' :: Monad m => Freer m a -> m a
retract'' (Return a) = return a
retract'' (action `Then` yield) = action >>= retract' . yield
{-# INLINE retract'' #-}


fibM :: Int -> Freer (Either String) Int
fibM n
  | n < 0 = (Left "negative") `Then` return
  | otherwise = go n
  where go 0 = return 0
        go 1 = Right 1 `Then` return
        go n = do
          x <- go (pred n)
          y <- go (pred (pred n))
          Right (x + y) `Then` return
