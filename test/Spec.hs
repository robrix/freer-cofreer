import Control.Comonad.Cofree.Cofreer.Spec
import Control.Monad.Free.Freer.Spec
import Test.Hspec

main :: IO ()
main = hspec . parallel $ do
  describe "Control.Comonad.Cofree.Cofreer.Spec" Control.Comonad.Cofree.Cofreer.Spec.spec
  describe "Control.Monad.Free.Freer.Spec" Control.Monad.Free.Freer.Spec.spec
