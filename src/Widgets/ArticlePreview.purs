module Page.ArticlePreview where

import Prelude

import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Control.MultiAlternative (orr)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.RemoteData as RD
import Data.Variant as V
import Effect.Ref (Ref)
import Types (Article, MyApp, User)
import Utils.Api (WebRequest_)


getAndViewArticles
  :: forall a r
   . MyApp { user :: Ref (Maybe User) | r} (WebRequest_ { articles :: Array Article })
  -> MyApp { user :: Ref (Maybe User) | r} a
getAndViewArticles getArticles =  do
  rdArticles <- getArticles <|> D.text "loading"
  V.case_
    # V.on RD._success (\articles ->
        forever $ orr $ map articleView articles.articles
      )
    # V.on RD._error (\err ->
        forever $ D.text (show err)
      )
    $ rdArticles


articleView :: forall a r . Article -> MyApp { user :: Ref (Maybe User) | r} a
articleView article =
  D.div
    [P.className "article-preview"]
    [ D.div
      [P.className "article-meta" ]
      [ D.a
        [P.href authorProfileUrl]
        [D.img [P.src article.author.image]]
      , D.div
        [ P.className "info" ]
        [ D.a
          [ P.href authorProfileUrl
          , P.className "author"
          ]
          [ D.text $ unwrap article.author.username ]
        , D.span [P.className "date"] [D.text (show article.createdAt)]
        ]
      , D.button
        [ P.className "btn btn-outline-primary btn-sm pull-xs-right" ]
        [ D.i [P.className "ion-heart"] []
        , D.text (" " <> show article.favoritesCount)
        ]
      ]
    , D.a
      [ P.href ("#/article/" <> unwrap article.slug)
      , P.className "preview-link"
      ]
      [ D.h1' [D.text $ unwrap article.title]
      , D.p' [D.text article.description]
      , D.span' [D.text "Read more..."]
      , D.ul
        [ P.className "tag-list" ]
        (mapFlipped article.tagList $ \tag ->
            D.li
              [ P.className "tag-default tag-pill tag-outline" ]
              [ D.text (unwrap tag) ]
        )
      ]
    ]
  where
    authorProfileUrl = "#/profile/" <> unwrap article.author.username

