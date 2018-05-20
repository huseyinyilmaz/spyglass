-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main
import Network.Wai
import Network.Wai.Test(defaultRequest, runSession, request, srequest, SRequest(..), setPath, SResponse, Session)
import Types(Item(..), Config(..))
import Server(getApp, getState)
import Middlewares
import Data.Aeson
import Data.ByteString.Lazy as LB

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

searchDataText :: LB.ByteString
!searchDataText = encode searchData

config :: Config
config = Config {
  port=8080,
  monitoringEnabled=False,
  monitoringIP="0.0.0.0",
  monitoringPort=8888,
  loggingEnabled=False,
  loggingForDevelopment=False,
  gzipEnabled=False,
  defaultResultLimit=20
  }

app :: IO Application
app = do
  appState <- getState config
  middlewares <- getMiddlewares config
  return (middlewares (getApp appState))


postRequest :: Session SResponse
!postRequest = srequest SRequest
              { simpleRequest = defaultRequest
                { requestMethod = "POST"
                } `setPath` "/test"
              , simpleRequestBody = searchDataText
              }


main :: IO ()
main = defaultMain [
  bgroup "/test"
      [ bench "POST" $ whnfIO $ do
          a <- app
          runSession postRequest a,

        bench "GET" $ whnfIO $ do
          a <- app
          flip runSession a $ do
            _ <- postRequest
            (request (defaultRequest `setPath` "/test?query=le"))
      ]

  ]
