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
  V.case_
    # V.on RD._success (\{user : user} -> do
        userRef <- asks _.user
        liftEffect $ Ref.write (Just user) userRef
        setItem "token" user.token
        pure $ RD.success user
      )
    # V.on RD._error (\err ->
        pure $ RD.error err
      )
    $ rdUser


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
      V.case_
        # V.on RD._success (\{user : user} -> do
            liftEffect $ Ref.new (Just user)
          )
        # V.on RD._error (\err ->
            liftEffect $ Ref.new Nothing
          )
        $ rdUser


foreign import getItem_ :: EffectFn1 String (Nullable String)

getItem :: forall m. (MonadAff m) => String -> m (Maybe String)
getItem key =
  toMaybe <$> liftEffect (runEffectFn1 getItem_ key)


foreign import setItem_ :: EffectFn2 String String Unit

setItem :: forall m. (MonadAff m) => String -> String -> m Unit
setItem key value =
  liftEffect (runEffectFn2 setItem_ key value)
