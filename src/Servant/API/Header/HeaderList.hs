module Servant.API.Header.HeaderList
  ( HeaderList(..)
  , WithRequiredHeaderList(..)
  , WithRequestHeaderList(..)
  , GetHeaders(..)
  , ConstructHeaders(..)
  , AddHeader(..)
  , addOptionalHeader
  , addRequiredHeader
  , noHeader
  , ArgumentDirection(..)
  ) where

import Control.DeepSeq
import Data.Kind
import Data.Proxy
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant.API.Header
import Data.Text (Text)
import Servant.API.ResponseHeaders (GetHeaders(..))
import Web.HttpApiData
import qualified Network.HTTP.Types as HTTP
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Singletons.Bool (SBoolI)

import Servant.API.Modifiers.Generalized

data HeaderList (ad :: ArgumentDirection) (hs :: [Type]) where
  HeaderListNil :: HeaderList ad '[]
  HeaderListCons
    :: ArgumentType ad mods a
    -> HeaderList ad as
    -> HeaderList ad (Header' mods sym a ': as)

deriving instance Show (HeaderList ad '[])
deriving instance (Show (ArgumentType ad mods a), Show (HeaderList ad rest))
  => Show (HeaderList ad (Header' mods sym a ': rest))
deriving instance Eq (HeaderList ad '[])
deriving instance (Eq (ArgumentType ad mods a), Eq (HeaderList ad rest))
  => Eq (HeaderList ad (Header' mods sym a ': rest))
deriving instance Ord (HeaderList ad '[])
deriving instance (Ord (ArgumentType ad mods a), Ord (HeaderList ad rest))
  => Ord (HeaderList ad (Header' mods sym a ': rest))

instance NFDataHeaderList ad hs => NFData (HeaderList ad hs) where
  rnf = rnfHeaderList

class NFDataHeaderList ad hs where rnfHeaderList :: HeaderList ad hs -> ()
instance NFDataHeaderList ad '[] where rnfHeaderList HeaderListNil = ()
instance (NFData (ArgumentType ad mods a), NFDataHeaderList ad rest) =>
  NFDataHeaderList ad (Header' mods sym a ': rest) where
    rnfHeaderList (HeaderListCons h hs) = rnf h `seq` rnfHeaderList hs

instance GetHeaderListHeaders ad hs => GetHeaders (HeaderList ad hs) where
  getHeaders = getHeaderListHeaders

class GetHeaderListHeaders ad hs where
  getHeaderListHeaders :: HeaderList ad hs -> [HTTP.Header]

instance GetHeaderListHeaders ad '[] where
  getHeaderListHeaders HeaderListNil = []

instance (IsArgument ad, KnownSymbol sym, ToHttpApiData a, SBoolI (FoldRequired mods), SBoolI (FoldLenient mods), GetHeaderListHeaders ad rest)
  => GetHeaderListHeaders ad (Header' mods sym a ': rest) where
    getHeaderListHeaders (HeaderListCons h hs) = do
      let headerName = CI.mk $ ByteString.pack $ symbolVal (Proxy :: Proxy sym)
          rest = getHeaderListHeaders hs
      foldArgument (Proxy :: Proxy ad) (Proxy :: Proxy mods) (Proxy :: Proxy a)
        (\a -> (headerName, toHeader a) : rest)
        (\ea -> (headerName, either Text.encodeUtf8 toHeader ea) : rest)
        (maybe rest (\a -> (headerName, toHeader a) : rest))
        (maybe rest (\ea -> (headerName, either Text.encodeUtf8 toHeader ea) : rest))
        h

class ConstructHeaders hs where

  -- this type is almost certainly wrong
  constructHeaders :: Proxy hs -> [HTTP.Header] -> Either [Text] hs

instance ConstructHeaders (HeaderList ad '[]) where
  constructHeaders _ _ = Right HeaderListNil

instance (KnownSymbol sym, IsArgument ad, SBoolI (FoldRequired mods), SBoolI (FoldLenient mods), FromHttpApiData a, ConstructHeaders (HeaderList ad hs))
  => ConstructHeaders (HeaderList ad (Header' mods sym a ': hs)) where

  constructHeaders _ headers = do
    let rest :: Either [Text] (HeaderList ad hs)
        rest = constructHeaders (Proxy :: Proxy (HeaderList ad hs)) headers
        sym = symbolVal (Proxy :: Proxy sym)
        maybeParseResult :: Maybe (Either Text a)
        maybeParseResult = parseHeader <$> lookup (CI.mk $ ByteString.pack sym) headers
        missingError :: forall b . Either Text b
        missingError = Left ("Missing required header:" <> Text.pack sym)
        header :: Either Text (ArgumentType ad mods a)
        header =
          unfoldArgument
            (Proxy :: Proxy ad)
            (Proxy :: Proxy mods)
            (Proxy :: Proxy a)
            (maybe missingError id)
            (maybe missingError pure)
            sequenceA
            pure
            maybeParseResult
    case (header, rest) of
      (Left err, Left errs) -> Left (err : errs)
      (Left err, Right _) -> Left [err]
      (Right _, Left errs) -> Left errs
      (Right l, Right r) -> Right (HeaderListCons l r)

data WithRequiredHeaderList hs a =
  WithRequiredHeaderList (HeaderList 'Outgoing hs) a

deriving instance (Show a, Show (HeaderList 'Outgoing hs)) => Show (WithRequiredHeaderList hs a)
deriving instance (Eq a, Eq (HeaderList 'Outgoing hs)) => Eq (WithRequiredHeaderList hs a)
deriving instance (Ord a, Ord (HeaderList 'Outgoing hs)) => Ord (WithRequiredHeaderList hs a)

data WithRequestHeaderList hs a =
  WithRequestHeaderList (HeaderList 'Incoming hs) a

deriving instance (Show a, Show (HeaderList 'Incoming hs)) => Show (WithRequestHeaderList hs a)
deriving instance (Eq a, Eq (HeaderList 'Incoming hs)) => Eq (WithRequestHeaderList hs a)
deriving instance (Ord a, Ord (HeaderList 'Incoming hs)) => Ord (WithRequestHeaderList hs a)

class AddHeader (sym :: Symbol) (a :: Type) (mods :: [Type]) (ad :: ArgumentDirection) orig new
  | sym a mods ad orig new -> new, new -> sym, new -> a, new -> mods, new -> ad, new -> orig where
  addHeader :: ArgumentType ad mods a -> orig -> new

instance {-# OVERLAPPING #-} AddHeader sym a mods 'Outgoing (WithRequiredHeaderList (h ': hs) a) (WithRequiredHeaderList (Header' mods sym a ': h ': hs) a) where
  addHeader h (WithRequiredHeaderList hs a) = WithRequiredHeaderList (HeaderListCons h hs) a

instance {-# OVERLAPPABLE #-} AddHeader sym a mods 'Outgoing orig (WithRequiredHeaderList '[Header' mods sym a] orig) where
  addHeader h a = WithRequiredHeaderList (HeaderListCons h HeaderListNil) a

instance {-# OVERLAPPING #-} AddHeader sym a mods 'Incoming (WithRequestHeaderList (h ': hs) a) (WithRequestHeaderList (Header' mods sym a ': h ': hs) a) where
  addHeader h (WithRequestHeaderList hs a) = WithRequestHeaderList (HeaderListCons h hs) a

instance {-# OVERLAPPABLE #-} AddHeader sym a mods 'Incoming orig (WithRequestHeaderList '[Header' mods sym a] orig) where
  addHeader h a = WithRequestHeaderList (HeaderListCons h HeaderListNil) a

addOptionalHeader :: AddHeader sym a mods ad orig new
                  => ArgumentType ad mods a -> orig -> new
addOptionalHeader = addHeader

addRequiredHeader :: (FoldRequired mods ~ 'True, AddHeader sym a mods ad orig new)
                  => ArgumentType ad mods a -> orig -> new
addRequiredHeader = addHeader

noHeader :: (ArgumentType ad mods a ~ Maybe b, AddHeader sym a mods ad orig new)
         => orig -> new
noHeader = addHeader Nothing
