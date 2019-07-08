module PostContainer where
import Data.Aeson
import Test.Tasty.Hspec(Spec, it, shouldBe, parallel)
import Test.Hspec
import Test.Hspec.Wai
import Network.Wai
import qualified Network.Wai.Test as WT
import Server(getApp, getState)
import Env(Config(..),
           AuthUser(..),
           runAppWithStateOnlyT,
           runAppT,
          )
--import Collection(Endpoint(..), Collection(..))
import Request(PostRequest(..), Term(..), IndexType(..))
import Middlewares
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import Data.Monoid((<>))

searchData :: PostRequest
searchData = PostDataRequest {
  values=[
  Term {indexType= Just Infix, term= "apple",   value= "apple content"},
  Term {indexType= Just Infix, term= "apricot", value= "apricot content"},
  Term {indexType= Just Infix, term= "cattle",  value= "cattle for content"},
  Term {indexType= Just Infix, term= "orange",  value= "another orange content"},
  Term {indexType= Just Infix, term= "amiddb",  value= "a middle b content"},
  Term {indexType= Just Infix, term= "cmiddd",  value= "c middle d content"},
  Term {indexType= Just Infix, term= "first second third forth",  value= "first second third forth content"}
  ]}

config :: Config
config = Config {
  _port=8080,
  _monitoringEnabled=False,
  _monitoringIP="0.0.0.0",
  _monitoringPort=8888,
  _loggingEnabled=False,
  _loggingForDevelopment=False,
  _gzipEnabled=False,
  _defaultResultLimit=20,
  _users=[AuthUser {_username="test", _password="test"}],
  _endpoints=[]
  }

app :: IO Application
app = do
  appState <- getState config
  middlewares <- (runAppWithStateOnlyT appState getMiddlewares):: IO Middleware
  let testApp req respond = runAppT appState respond (getApp req respond)
  return $ middlewares testApp

postWithAuth :: B.ByteString -> BL.ByteString -> WaiSession WT.SResponse
postWithAuth path body = request "POST" path
  [("Authorization", ("Basic " <> (B64.encode ("test:test"::B.ByteString))))] body

testRoot :: Spec
testRoot =
  with app $
  do
    do describe "GET /" $
         do it "serves the home page" $
              get "/" `shouldRespondWith` "Hello World!" {matchStatus = 200}

testPostNoAuth :: Spec
testPostNoAuth =
  with app $
  do
    do describe "POST /test" $
         do it "Post without authentication." $ do
              post "/test" (encode searchData) `shouldRespondWith` "Basic authentication is required" {matchStatus = 401}

testPostWrongPass :: Spec
testPostWrongPass =
  with app $
  do
    do describe "POST /test" $
         do it "Post with wrong username and password" $ do
              postWithWrongAuth "/test" (encode searchData) `shouldRespondWith` "Basic authentication is required" {matchStatus = 401}
  where
    postWithWrongAuth :: B.ByteString -> BL.ByteString -> WaiSession WT.SResponse
    postWithWrongAuth path body = request "POST" path
      [("Authorization", ("Basic " <> (B64.encode ("wrong:wrong"::B.ByteString))))] body


testPost :: Spec
testPost =
  with app $
  do
    do describe "POST /test" $
         do it "Post a container" $ do
              postWithAuth "/test" (encode searchData) `shouldRespondWith` "" {matchStatus = 201}

testPostGet :: Spec
testPostGet =
  with app $
  do
    do describe "POST /test, Get /test" $
         do it "Post a container than test queries." $ do
              postWithAuth "/test" (encode searchData) `shouldRespondWith` "" {matchStatus = 201}
              get  "/test?query=app" `shouldRespondWith` "[\"apple content\"]" {matchStatus = 200}
              get  "/test?query=ap" `shouldRespondWith` "[\"apple content\",\"apricot content\"]" {matchStatus = 200}
              get  "/test?query=le" `shouldRespondWith` "[\"cattle for content\",\"apple content\"]" {matchStatus = 200}
              get  "/test?query=midd" `shouldRespondWith` "[\"a middle b content\",\"c middle d content\"]" {matchStatus = 200}
              get  "/test?query=second third" `shouldRespondWith` "[\"first second third forth content\"]" {matchStatus = 200}


testGetNotFound :: Spec
testGetNotFound =
  with app $
  do
    do describe "GET /test 404" $
        do it "Sends a get request and expects 404 as a result." $ do
            get  "/test?query=apple" `shouldRespondWith` "Collection Does Not Exist!" {matchStatus = 404}

spec :: Spec
spec = do
    it "is trivially true" $ do
        True `shouldBe` True

specs :: Spec
specs = parallel $ do
  -- testRoot
  testPost
  testPostNoAuth
  testPostWrongPass
  testPostGet
  testGetNotFound
