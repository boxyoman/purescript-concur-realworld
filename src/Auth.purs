module Auth where

import Prelude

import Api as Api
import Control.Monad.Reader (class MonadReader, asks)
import Data.Maybe (Maybe(..))
import Data.RemoteData as RD
import Data.Variant as V
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
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
        pure $ RD.success user
      )
    # V.on RD._error (\err ->
        pure $ RD.error err
      )
    $ rdUser

