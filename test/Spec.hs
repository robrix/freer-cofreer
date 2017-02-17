import Control.Comonad.Cofree.Cofreer.Spec
import Control.Comonad.Trans.Cofree.Cofreer.Spec
import Control.Monad.Free.Freer.Spec
import Control.Monad.Trans.Free.Freer.Spec
import Test.Hspec

main :: IO ()
main = hspec . parallel $ do
  describe "Control.Comonad.Cofree.Cofreer" Control.Comonad.Cofree.Cofreer.Spec.spec
  describe "Control.Comonad.Trans.Cofree.Cofreer" Control.Comonad.Trans.Cofree.Cofreer.Spec.spec
  describe "Control.Monad.Free.Freer" Control.Monad.Free.Freer.Spec.spec
  describe "Control.Monad.Trans.Free.Freer" Control.Monad.Trans.Free.Freer.Spec.spec
