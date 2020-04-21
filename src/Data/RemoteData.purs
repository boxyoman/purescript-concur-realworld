module Data.RemoteData where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Data.Lens (over)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, case_, inj, on, default)
import Data.Variant.Prism (variant)

_success = SProxy :: SProxy "success"
_error = SProxy :: SProxy "error"
_notAsked = SProxy :: SProxy "notAsked"
_loading = SProxy :: SProxy "loading"

error :: forall err r . err -> Variant ( error :: err | r)
error = inj _error

success :: forall a r . a -> Variant ( success :: a | r)
success = inj _success

notAsked :: forall r . Variant ( notAsked :: Unit | r )
notAsked = inj _notAsked unit

loading :: forall r . Variant ( loading :: Unit | r )
loading = inj _loading unit


type RemoteData' err a = Variant
  ( error :: err
  , success :: a
  )

toEither :: forall err a . RemoteData' err a -> Either err a
toEither =
  case_
    # on _success (\a -> Right a)
    # on _error (\err -> Left err)

except
  :: forall err a m
   . Monad m
  => m (RemoteData' err a)
  -> ExceptT err m a
except = ExceptT <<< map toEither

toMaybe :: forall r a . Variant ( success :: a | r ) -> Maybe a
toMaybe = default Nothing
  # on _success (\a -> Just a)


fromEither
  :: forall a err r
   . Either err a
  -> Variant ( error :: err , success :: a | r)
fromEither (Left err) = error err
fromEither (Right a) = success a


-- | Map over the success case.
overSuccess
  :: forall r a b
   . (a -> b)
  -> Variant ( success :: a | r )
  -> Variant ( success :: b | r )
overSuccess = over (variant _success)

-- | Map over the error case.
overError
  :: forall r a b
   . (a -> b)
  -> Variant ( error :: a | r )
  -> Variant ( error :: b | r )
overError = over (variant _error)


-- | You can use Data.Variant.expand to turn a RemoteData' into a RemoteData.
type RemoteData err a = Variant
  ( error :: err
  , success :: a
  , notAsked :: Unit
  , loading :: Unit
  )
