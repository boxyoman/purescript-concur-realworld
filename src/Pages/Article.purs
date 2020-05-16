module Page.Article where

import Prelude
import Api (getArticle)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Variant (Variant)
import Data.Variant as V
import Effect.Ref (Ref)
import Types (Article, MyApp, Slug, User)
import Routes as R
import Types.DateTimeJSON (viewDate)


articlePage
  :: forall v r
   . Slug
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
articlePage slug = do
  rdArticle <- getArticle slug <|> D.text "loading"
  V.match
    { success : \article -> articleView article.article
    , error : \err -> D.text (show err)
    }
    rdArticle


articleView
  :: forall v r
   . Article
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
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


articleMetaView
  :: forall v r
   . Article
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
articleMetaView article =
  D.div
    [ P.className "article-meta" ]
    [ D.a
      [P.onClick $> R.changeRoute (R.Profile article.author.username)]
      [ D.img
        [ P.src article.author.image ]
      ]
    , D.div
      [ P.className "info" ]
      [ D.a
        [P.onClick $> R.changeRoute (R.Profile article.author.username)]
        [ D.text $ unwrap article.author.username ]
      , viewDate article.createdAt
      ]
    , empty -- TODO: follow and like here
    ]
