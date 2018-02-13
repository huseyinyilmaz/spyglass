module Main where
import qualified PostContainer as PostContainer
-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

main :: IO ()
main = do
    test <- testSpec "spyglass" specs
    Test.Tasty.defaultMain test

specs :: Spec
specs = parallel $ do PostContainer.specs
