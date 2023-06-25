module Servant.API.Verbs.HeaderSpec where

import Control.Monad.Free
import Data.Foldable
import Data.IORef
import Data.Proxy
import Network.HTTP.Types (HeaderName, renderQuery)
import Network.Wai.Internal
import Servant.API hiding (Verb, Headers, addHeader)
import Servant.API.Verbs.Header
import Servant.Client
import Servant.Client.Core.HasClient
import Servant.Client.Core.RunClient
import Servant.Server
import Test.Hspec
import Test.Hspec.Wai
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Sequence as Sequence
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Servant.Client.Core.Request as Servant
import qualified Servant.Client.Core.Response as Servant

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (pure testApplication) do

  describe "Verb" do

    describe "/" do

      it "should return no headers" do
        get "/" `shouldRespondWith` 200
          { matchHeaders = [emptyHeadersExcept ["Content-Type"]]
          }

      it "makes a request with no headers" $ \((), app) -> do
        res <- runClientWithApplication app getRoot
        res `shouldBe` Right 5

    describe "/test" do

      it "should return an X-Test-Header header" do
        get "/test" `shouldRespondWith` 200
          { matchHeaders = ["X-Test-Header" <:> "5"]
          }

      it "makes a request with headers" $ \((), app) -> do
        runClientWithApplication app getTest >>= \case
          Left err -> expectationFailure (show err)
          Right (WithRequestHeaderList headers val) -> do
            headers `shouldBe` HeaderListCons 5 HeaderListNil
            val `shouldBe` "test"

type TestAPI =
  Verb 'GET 200 '[JSON] NoHeaders Integer :<|>
  "test" :> Verb 'GET 200 '[JSON] (Headers '[Header' [Required, Strict] "X-Test-Header" Integer]) String

testAPI :: Proxy TestAPI
testAPI = Proxy

getRoot :: Free ClientF Integer
getTest :: Free ClientF (WithRequestHeaderList '[Header' '[Required, Strict] "X-Test-Header" Integer] String)
getRoot :<|> getTest = testAPI `clientIn` (Proxy :: Proxy (Free ClientF))

testAPIServer :: Server TestAPI
testAPIServer = do
  let rootHandler = pure 5
      withHeaderHandler = pure (addHeader 5 "test")
  rootHandler :<|> withHeaderHandler

testApplication :: Application
testApplication = serve (Proxy :: Proxy TestAPI) testAPIServer

emptyHeadersExcept :: [HeaderName] -> MatchHeader
emptyHeadersExcept acceptable = MatchHeader \hs _ -> go hs
  where
    go hs =
      case filter ((`notElem` acceptable) . fst) hs of
        [] -> Nothing
        unacceptables ->
          Just $ "Expected headers to be empty, but got " <> show unacceptables

runClientWithApplication :: Application
                         -> Free ClientF a
                         -> IO (Either ClientError a)
runClientWithApplication app act =
  case act of
    Pure x -> pure (Right x)
    Free (Throw err) -> pure (Left err)
    Free (RunRequest req f) -> do
      ref <- newIORef Nothing
      Wai.ResponseReceived <- app (fromServantRequest req) \resp -> do
        writeIORef ref (Just resp)
        pure Wai.ResponseReceived
      readIORef ref >>= \case
        Nothing -> error "no response received from Application"
        Just resp -> do
          servantResponse <- toServantResponse resp
          runClientWithApplication app (f servantResponse)

fromServantRequest :: Servant.Request -> Wai.Request
fromServantRequest sreq =
  let query = toList (Servant.requestQueryString sreq)
      path = ByteString.Lazy.toStrict $ Builder.toLazyByteString $ Servant.requestPath sreq
  in
  Wai.defaultRequest
    { Wai.requestMethod = Servant.requestMethod sreq
    , Wai.httpVersion = Servant.requestHttpVersion sreq
    , Wai.rawPathInfo = path
    , Wai.rawQueryString = renderQuery False query
    , Wai.requestHeaders = toList (Servant.requestHeaders sreq)
    , Wai.pathInfo = HTTP.decodePathSegments path
    , Wai.queryString = query
    }

toServantResponse :: Wai.Response -> IO Servant.Response
toServantResponse = \case
  ResponseFile _ _ _ _ -> error "toServantResponse: ResponseFile"
  ResponseBuilder statusCode headers builder ->
    pure Servant.Response
      { Servant.responseStatusCode = statusCode
      , Servant.responseHeaders = Sequence.fromList headers
      , Servant.responseHttpVersion = HTTP.http11
      , Servant.responseBody = Builder.toLazyByteString builder
      }
  ResponseStream _ _ _ -> error "toServantResponse: ResponseStream"
  ResponseRaw _ _ -> error "toServantResponse: ResponseRaw"
