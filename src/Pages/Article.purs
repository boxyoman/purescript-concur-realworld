module Page.Article where

import Prelude

import Api (getArticle)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.RemoteData as RD
import Data.Variant as V
import Effect.Ref (Ref)
import Types (Article, MyApp, Slug, User)


articlePage :: forall a r . Slug -> MyApp { user :: Ref (Maybe User) | r} a
articlePage slug = do
  rdArticle <- getArticle slug <|> D.text "loading"
  V.case_
    # V.on RD._success (\article ->
        articleView article.article
      )
    # V.on RD._error (\err ->
        forever $ D.text (show err)
      )
    $ rdArticle


articleView :: forall a r . Article -> MyApp { user :: Ref (Maybe User) | r} a
articleView article =
  D.div
    [ P.className "article-page" ]
    [ D.div
      [ P.className "banner" ]
      [ D.div
        [ P.className "container" ]
        [ D.h1' [ D.text (unwrap article.title) ]
        , articleMetaView article
        ]
      ]
    , D.div
      [ P.className "container page" ]
      [ D.div
        [ P.className "row article-content" ]
        [ D.div
          [ P.className "col-md-12" ]
          [ D.text article.body ] -- TODO figure out how to markdown
        ]
      , D.hr'
      , D.div
        [ P.className "article-actions" ]
        [ articleMetaView article ]
      , D.div
        [ P.className "row" ]
        [ D.div
          [ P.className "col-xs-12 col-md-8 offset-md-2" ]
          [] --TODO :: Comments
        ]
      ]
    ]
  where
    authorProfileUrl = "#/profile/" <> unwrap article.author.username


articleMetaView :: forall a r . Article  -> MyApp { user :: Ref (Maybe User) | r} a
articleMetaView article =
  D.div
    [ P.className "article-meta" ]
    [ D.a
      [ P.href $ "#/profile/" <> authorProfileUrl ]
      [ D.img
        [ P.src article.author.image ]
      ]
    , D.div
      [ P.className "info" ]
      [ D.a
        [ P.href $ "#/profile/" <> authorProfileUrl ]
        [ D.text $ unwrap article.author.username ]
      , D.span
        [ P.className "date" ]
        [ D.text (show article.createdAt)]
      ]
    , empty -- TODO: follow and like here
    ]
  where
    authorProfileUrl = "#/profile/" <> unwrap article.author.username
