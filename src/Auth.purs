module Auth where

import Prelude

import Api as Api
import Control.Monad.Reader (class MonadReader, asks)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.RemoteData as RD
import Data.Variant as V
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Types (User)
import Utils.Api (WebRequest_)


login
  :: forall m r
   . MonadAff m
  => MonadReader {user :: Ref (Maybe User) | r} m
  => { email :: String, password :: String }
  -> m (WebRequest_ User)
login loginData = do
  rdUser <- Api.login loginData
  V.match
    { success : \ {user} -> do
        userRef <- asks _.user
        liftEffect $ Ref.write (Just user) userRef
        setItem "token" user.token
        pure $ RD.success user

    , error : \ err -> do
        pure $ RD.error err
    }
    rdUser

updateUserRef
  :: forall m r
   . MonadAff m
  => MonadReader {user :: Ref (Maybe User) | r} m
  => User
  -> m Unit
updateUserRef user = do
  userRef <- asks _.user
  liftEffect $ Ref.write (Just user) userRef

setupUserRef
  :: forall m
   . MonadAff m
  => m (Ref (Maybe User))
setupUserRef = do
  mtoken <- getItem "token"
  case mtoken of
    Nothing ->
      liftEffect $ Ref.new Nothing
    Just token -> do
      rdUser <- Api.getUser token
      V.match
        { success : \{user} ->
            liftEffect $ Ref.new (Just user)
        , error : \err ->
            liftEffect $ Ref.new Nothing
        }
        rdUser


logout
  :: forall m r
   . MonadAff m
  => MonadReader {user :: Ref (Maybe User) | r} m
  => m Unit
logout = do
  userRef <- asks _.user
  liftEffect $ Ref.write Nothing userRef
  removeItem "token"


foreign import getItem_ :: EffectFn1 String (Nullable String)

getItem :: forall m. (MonadAff m) => String -> m (Maybe String)
getItem key =
  toMaybe <$> liftEffect (runEffectFn1 getItem_ key)


foreign import setItem_ :: EffectFn2 String String Unit

setItem :: forall m. (MonadAff m) => String -> String -> m Unit
setItem key value =
  liftEffect (runEffectFn2 setItem_ key value)

foreign import removeItem_ :: EffectFn1 String Unit

removeItem :: forall m. (MonadAff m) => String -> m Unit
removeItem key =
  liftEffect (runEffectFn1 removeItem_ key)
