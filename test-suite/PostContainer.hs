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
  Item {term= "apple",   content= "apple content"},
  Item {term= "apricot", content= "apricot content"},
  Item {term= "cattle",  content= "cattle content"},
  Item {term= "orange",  content= "orange content"},
  Item {term= "amiddb",  content= "a middle b content"},
  Item {term= "cmiddd",  content= "c middle d content"},
  Item {term= "first second third forth",  content= "first second third forth content"}
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
         do it "Post a container" $ do
              post "/test" (encode searchData) `shouldRespondWith` "" {matchStatus = 201}

testPostGet :: Spec
testPostGet =
  with (spockAsApp app) $
  do
    do describe "POST /test, Get /test" $
         do it "Post a container than test queries." $ do
              post "/test" (encode searchData) `shouldRespondWith` "" {matchStatus = 201}
              get  "/test?query=app" `shouldRespondWith` "[\"apple content\"]" {matchStatus = 200}
              get  "/test?query=ap" `shouldRespondWith` "[\"apple content\",\"apricot content\"]" {matchStatus = 200}
              get  "/test?query=le" `shouldRespondWith` "[\"apple content\",\"cattle content\"]" {matchStatus = 200}
              get  "/test?query=midd" `shouldRespondWith` "[\"a middle b content\",\"c middle d content\"]" {matchStatus = 200}
              get  "/test?query=second third" `shouldRespondWith` "[\"first second third forth content\"]" {matchStatus = 200}


testGetNotFound :: Spec
testGetNotFound =
  with (spockAsApp app) $
  do
    do describe "GET /test 404" $
        do it "Sends a get request and expects 404 as a result." $ do
            get  "/test?query=apple" `shouldRespondWith` "Error: Collection does not exist." {matchStatus = 404}

spec :: Spec
spec = do
    it "is trivially true" $ do
        True `shouldBe` True

specs :: Spec
specs = parallel $ do
  testRoot
  testPost
  testPostGet
  testGetNotFound
