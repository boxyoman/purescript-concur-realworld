module Widgets.InputError where

import Prelude

import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alternative (empty)
import Data.Array (null)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe)
import Effect.Ref (Ref)
import Types (MyApp, User)


showErrors
  :: forall a  r
   . Array String
  -> MyApp { user :: Ref (Maybe User) | r} a
showErrors errs =
  if null errs
    then empty
    else
      D.ul
        [ P.className "error-messages" ]
        (mapFlipped errs $ \err ->
          D.li
            [  ]
            [ D.text err ]
        )
