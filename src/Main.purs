module Main where

import Prelude

import Api (getArticles)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Control.MultiAlternative (orr)
import Effect (Effect)
import Page.Home (globalFeed, homePage)


hello :: forall a. Widget HTML a
hello = forever do
  {-- void $ D.button [P.onClick] [D.text "Get Articles"] --}
  articles <- getArticles <|> D.text "loading"
  ( orr
    [ void $ D.button [P.onClick] [D.text "Restart"]
    , D.text (show articles)
    ]
  )

main :: Effect Unit
main = do
  runWidgetInDom "root" homePage
