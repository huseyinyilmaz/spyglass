module PostContainer where
import Data.Aeson
import Test.Tasty.Hspec(Spec, it, shouldBe, parallel)
import Test.Hspec
import Test.Hspec.Wai

import Web.Spock (spockAsApp)

import Server(app)
import Types(Item(..))


searchData :: [Item]
searchData = [
  Item {term="apple", content= "apple content"},
  Item {term= "cattle", content= "cattle content"},
  Item {term= "orange", content= "orange content"}
  ]

testRoot :: Spec
testRoot =
  with (spockAsApp app) $
  do
    do describe "GET /" $
         do it "serves the home page" $
              get "/" `shouldRespondWith` "Hello World!" {matchStatus = 200}

testPost :: Spec
testPost =
  with (spockAsApp app) $
  do
    do describe "POST /test" $
         do it "Post a container" $
              post "/test" (encode searchData) `shouldRespondWith` "" {matchStatus = 201}

spec2 :: Spec
spec2 = do
    it "is trivially true" $ do
        True `shouldBe` True

specs :: Spec
specs = parallel $ do
  testRoot
  testPost
  spec2
