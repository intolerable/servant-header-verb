module Servant.API.Verbs.Header
  ( module Servant.API.Verbs.Header
  , module Servant.API.Header.HeaderList
  ) where

import Control.Monad
import Data.Kind
import GHC.TypeLits
import Data.Typeable
import GHC.Generics
import Servant.API (ReflectMethod(..))
import Servant.API.Status
import Servant.Server
import Servant.Server.Internal (methodRouter)
import qualified Network.HTTP.Types as HTTP
import Servant.API.ContentTypes
import Servant.Client.Core
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Sequence
import Network.HTTP.Media as Media
import Data.Foldable (toList)

import Servant.API.Modifiers.Generalized
import Servant.API.Header.HeaderList

class HasServerHeaders hs where
  type HeaderServerT hs (a :: Type) :: Type

  splitHeaders :: Proxy hs -> Proxy a -> HeaderServerT hs a -> ([HTTP.Header], a)

class HasClientHeaders hs where
  type HeaderClientT hs (a :: Type) :: Type

  constructWithHeaders :: Proxy hs
                       -> Proxy a
                       -> [HTTP.Header]
                       -> a
                       -> Either [Text] (HeaderClientT hs a)

data NoHeaders

instance HasServerHeaders NoHeaders where
  type HeaderServerT NoHeaders a = a

  splitHeaders _ _ a = pure a

instance HasClientHeaders NoHeaders where
  type HeaderClientT NoHeaders a = a

  constructWithHeaders _ _ _ a = pure a

data Headers (hs :: [Type])

instance GetHeaders (HeaderList 'Outgoing hs) => HasServerHeaders (Headers hs) where
  type HeaderServerT (Headers hs) a = WithRequiredHeaderList hs a

  splitHeaders _ _ (WithRequiredHeaderList hs a) = (getHeaders hs, a)

instance ConstructHeaders (HeaderList 'Incoming hs)
  => HasClientHeaders (Headers hs) where
  type HeaderClientT (Headers hs) a = WithRequestHeaderList hs a

  constructWithHeaders _ _ headers a =
    WithRequestHeaderList <$> constructHeaders Proxy headers
                          <*> pure a

data Verb (method :: m) (statusCode :: Nat) (contentTypes :: [Type]) (headers :: Type) (a :: Type)
  deriving (Typeable, Generic)

instance (ReflectMethod method, KnownNat status, AllCTRender ctys a, HasServerHeaders headers)
  => HasServer (Verb method status ctys headers a) ctx where
    type ServerT (Verb method status ctys headers a) m =
      m (HeaderServerT headers a)

    hoistServerWithContext _ _ nt s = nt s

    route Proxy _ = methodRouter (splitHeaders headers a) method (Proxy :: Proxy ctys) status
      where method = reflectMethod (Proxy :: Proxy method)
            status = statusFromNat (Proxy :: Proxy status)
            headers = Proxy :: Proxy headers
            a = Proxy :: Proxy a

instance (RunClient m, ReflectMethod method, KnownNat status, HasClientHeaders headers, MimeUnrender ct a, ctys ~ (ct ': cts))
  => HasClient m (Verb method status ctys headers a) where
    type Client m (Verb method status ctys headers a) = m (HeaderClientT headers a)

    hoistClientMonad _ _ nt c = nt c

    clientWithRoute _ Proxy req = do
      response <- runRequestAcceptStatus (Just [status]) req
        { requestMethod = method
        , requestAccept = Sequence.fromList (NonEmpty.toList accept)
        }
      val <- response `decodedAs` (Proxy :: Proxy ct)
      case constructWithHeaders headers (Proxy :: Proxy a) (toList $ responseHeaders response) val of
        Left errs -> do
          let msg = "Unable to parse response headers: " <> Text.intercalate ", " errs
          throwClientError $ DecodeFailure msg response
        Right res -> pure res
      where
        method = reflectMethod (Proxy :: Proxy method)
        status = statusFromNat (Proxy :: Proxy status)
        headers = Proxy :: Proxy headers
        accept = contentTypes (Proxy :: Proxy ct)

checkContentTypeHeader :: RunClient m => Response -> m MediaType
checkContentTypeHeader response =
  case lookup "Content-Type" $ toList $ responseHeaders response of
    Nothing -> return $ "application" Media.// "octet-stream"
    Just t -> case parseAccept t of
      Nothing -> throwClientError $ InvalidContentTypeHeader response
      Just t' -> return t'

decodedAs :: forall ct a m. (MimeUnrender ct a, RunClient m)
  => Response -> Proxy ct -> m a
decodedAs response ct = do
  responseContentType <- checkContentTypeHeader response
  unless (any (matches responseContentType) accept) $
    throwClientError $ UnsupportedContentType responseContentType response
  case mimeUnrender ct $ responseBody response of
    Left err -> throwClientError $ DecodeFailure (Text.pack err) response
    Right val -> return val
  where
    accept = toList $ contentTypes ct
