module Utils.Api
  ( HttpError(..)
  , ParseError(..)
  , WebRequest'
  , WebRequest_
  , WebRequest
  , class ReponseBody
  , AsJSON
  , readAsJSON
  , readAsJSON'
  , getBody
  , fetch
  , get
  , post
  , put
  , delete
  ) where

import Prelude

import Control.Monad.Reader (class MonadReader, asks)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.RemoteData as R
import Data.Variant (Variant)
import Effect.Aff (attempt)
import Effect.Aff.Class (liftAff, class MonadAff)
import Effect.Class (liftEffect)
import Effect.Exception as E
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign, MultipleErrors)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Prim.Row (class Union)
import Simple.JSON (class ReadForeign, class WriteForeign, read, writeJSON)
import Types (User)

data ParseError
  = NotJSON String
  | ParseError MultipleErrors

derive instance genericParseError :: Generic ParseError _
instance showParseError :: Show ParseError where
  show = genericShow


data HttpError body
  = NetworkError String
  | FailedRequest { statusCode :: Int, body :: body }
  | ParseFailure ParseError

derive instance genericHttpError :: Generic (HttpError err) _
instance showHttpError :: (Show err) => Show (HttpError err) where
  show = genericShow

type WebRequest' errorBody body = R.RemoteData' (HttpError errorBody) body
type WebRequest_ body = WebRequest' String body
type WebRequest body = R.RemoteData (HttpError String) body


class ReponseBody a where
  getBody :: forall m . MonadAff m => M.Response -> m (Either ParseError a)


newtype AsJSON a = AsJSON a

readAsJSON :: forall a . AsJSON a -> a
readAsJSON (AsJSON a) = a

readAsJSON'
  :: forall a r
   . Variant (success :: AsJSON a | r)
  -> Variant (success :: a | r)
readAsJSON' = R.overSuccess readAsJSON

instance unitResponseBody :: ReponseBody Unit where
  getBody res = pure (Right unit)

instance stringResponseBody :: ReponseBody String where
  getBody res = do
    str <- liftAff $ M.text res
    pure (Right str)

instance foreignResponseBody :: ReponseBody Foreign where
  getBody res = do
    json <- liftAff $ attempt $ M.json res
    pure $ lmap (NotJSON <<< E.message) $ json

instance jsonResponseBody
  :: ReadForeign a
  => ReponseBody (AsJSON a) where
  getBody res = do
    ejson <- liftAff $ attempt $ M.json res
    case ejson of
      Right json ->
        pure $ bimap ParseError AsJSON $ read $ json
      Left error ->
        pure $ Left (NotJSON (E.message error))


fetch
  :: forall options trash body m errorBody
   . Union options trash M.Options
  => ReponseBody body
  => ReponseBody errorBody
  => MonadAff m
  => M.URL
  -> { method :: M.Method | options }
  -> m (WebRequest' errorBody body)
fetch url options = do
  _response <- liftAff $ attempt $ M.fetch windowFetch url options
  case _response of
    Left err ->
      pure
        $ R.error
        $ NetworkError (E.message err)
    Right response -> do
      let statusCode = M.statusCode response
      if statusCode >= 200 && statusCode < 300
        then do
          body <- getBody response
          case body of
            Left perr -> pure $ R.error $ ParseFailure perr
            Right a -> pure $ R.success a
        else do
          body <- getBody response
          case body of
            Left perr ->
              pure $ R.error $ ParseFailure perr
            Right a ->
              pure $ R.error $ FailedRequest { statusCode, body: a}


get
  :: forall body m r
   . ReponseBody body
  => MonadAff m
  => MonadReader {user :: Ref (Maybe User) | r} m
  => M.URL
  -> m (WebRequest_ body)
get url = do
  user <- asks (_.user) >>= (liftEffect <<< Ref.read)
  let mToken = map _.token user

  fetch
    url
    { method : M.getMethod
    , credentials : M.includeCredentials
    , headers : mkHeader mToken
    }

mkHeader :: Maybe String -> M.Headers
mkHeader Nothing = M.makeHeaders {}
mkHeader (Just token) = M.makeHeaders {"Authorization" : "Token " <> token}


post
  :: forall body m requestBody r
   . ReponseBody body
  => MonadAff m
  => WriteForeign requestBody
  => MonadReader {user :: Ref (Maybe User) | r} m
  => M.URL
  -> requestBody
  -> m (WebRequest_ body)
post url body = do
  user <- asks (_.user) >>= (liftEffect <<< Ref.read)
  let mToken = map _.token user

  let bodyStr = writeJSON body :: String
  let options' = { method : M.postMethod
                 , body : bodyStr
                 , headers: (M.makeHeaders { "Content-Type": "application/json" } <> mkHeader mToken)
                 , credentials : M.includeCredentials
                 }
  fetch url options'

put
  :: forall body m requestBody r
   . ReponseBody body
  => MonadAff m
  => MonadReader {user :: Ref (Maybe User) | r} m
  => WriteForeign requestBody
  => M.URL
  -> requestBody
  -> m (WebRequest_ body)
put url body = do
  user <- asks (_.user) >>= (liftEffect <<< Ref.read)
  let mToken = map _.token user
  let bodyStr = writeJSON body :: String
  let options' = { method : M.putMethod
                 , body : bodyStr
                 , headers: (M.makeHeaders { "Content-Type": "application/json" } <> mkHeader mToken)
                 , credentials : M.includeCredentials
                 }
  fetch url options'

delete
  :: forall body m requestBody r
   . ReponseBody body
  => MonadAff m
  => MonadReader {user :: Ref (Maybe User) | r} m
  => WriteForeign requestBody
  => M.URL
  -> requestBody
  -> m (WebRequest_ body)
delete url body = do
  user <- asks (_.user) >>= (liftEffect <<< Ref.read)
  let mToken = map _.token user
  let bodyStr = writeJSON body :: String
  let options' = { method : M.deleteMethod
                 , body : bodyStr
                 , headers: (M.makeHeaders { "Content-Type": "application/json" } <> mkHeader mToken)
                 , credentials : M.includeCredentials
                 }
  fetch url options'
