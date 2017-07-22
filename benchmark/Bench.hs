{-# LANGUAGE GADTs #-}
import Control.Monad.Free.Freer
import Criterion.Main
import Prelude hiding (read)

main :: IO ()
main = defaultMain
  [ bgroup "retract" (b retract <$> [30])
  , bgroup "retract2" (b retract' <$> [30])
  ]
  where b f i = bench (show i) (whnf (f . program) i)

retract' :: Monad m => Freer m a -> m a
retract' (Return a) = return a
retract' (action `Then` yield) = action >>= retract' . yield
{-# INLINE retract' #-}

program :: Int -> Freer (Either String) Int
program n
  | n < 0 = (Left "negative") `Then` return
  | otherwise = go n
  where go 0 = return 0
        go 1 = Right 1 `Then` return
        go n = do
          x <- go (pred n)
          y <- go (pred (pred n))
          Right (x + y) `Then` return
