module Servant.API.Modifiers.Generalized
  ( module Servant.API.Modifiers
  , module Servant.API.Modifiers.Generalized
  ) where

import Servant.API.Modifiers
import Data.Typeable
import GHC.Generics
import Data.Kind
import Data.Singletons.Bool (SBoolI, SBool(..), sbool)
import Data.Text (Text)

data ArgumentDirection = Incoming | Outgoing
  deriving (Typeable, Generic)

class IsArgument (dir :: ArgumentDirection) where
  type ArgumentType (dir :: ArgumentDirection) (mods :: [Type]) (a :: Type) :: Type

  foldArgument :: (SBoolI (FoldRequired mods), SBoolI (FoldLenient mods))
               => Proxy dir
               -> Proxy mods
               -> Proxy a
               -> (a -> r)
               -> (Either Text a -> r)
               -> (Maybe a -> r)
               -> (Maybe (Either Text a) -> r)
               -> ArgumentType dir mods a
               -> r
  unfoldArgument :: ( Applicative f
                    , SBoolI (FoldRequired mods)
                    , SBoolI (FoldLenient mods)
                    )
                 => Proxy dir
                 -> Proxy mods
                 -> Proxy a
                 -> (r -> f a)
                 -> (r -> f (Either Text a))
                 -> (r -> f (Maybe a))
                 -> (r -> f (Maybe (Either Text a)))
                 -> r
                 -> f (ArgumentType dir mods a)

instance IsArgument 'Incoming where
  type ArgumentType 'Incoming mods a =
    RequestArgument mods a

  foldArgument Proxy mods Proxy f ef mf mef a =
    foldRequestArgument mods f ef mf mef a

  unfoldArgument _ (Proxy :: Proxy mods) _ f ef mf mef arg =
    case (sbool :: SBool (FoldRequired mods), sbool :: SBool (FoldLenient mods), arg) of
      (SFalse, SFalse, val) -> mf val
      (SFalse, STrue, val) -> mef val
      (STrue, SFalse, val) -> f val
      (STrue, STrue, val) -> ef val

instance IsArgument 'Outgoing where
  type ArgumentType 'Outgoing mods a =
    RequiredArgument mods a

  foldArgument Proxy mods Proxy f _ef mf _mef a =
    foldRequiredArgument mods f mf a

  unfoldArgument _ (Proxy :: Proxy mods) _ f _ef mf _mef arg =
    case (sbool :: SBool (FoldRequired mods), sbool :: SBool (FoldLenient mods), arg) of
      (SFalse, SFalse, val) -> mf val
      (SFalse, STrue, val) -> mf val
      (STrue, SFalse, val) -> f val
      (STrue, STrue, val) -> f val

foldRequestArgument
  :: forall mods a r .
     (SBoolI (FoldRequired mods), SBoolI (FoldLenient mods))
  => Proxy mods
  -> (a -> r)
  -> (Either Text a -> r)
  -> (Maybe a -> r)
  -> (Maybe (Either Text a) -> r)
  -> RequestArgument mods a
  -> r
foldRequestArgument Proxy f ef mf mef arg =
  case (sbool :: SBool (FoldRequired mods), sbool :: SBool (FoldLenient mods), arg) of
    (SFalse, SFalse, val) -> mf val
    (SFalse, STrue, val) -> mef val
    (STrue, SFalse, val) -> f val
    (STrue, STrue, val) -> ef val
