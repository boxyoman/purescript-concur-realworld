module Data.Wrapped (Wrapped(..))
  where

import Data.Newtype (class Newtype)
import Prelude (class Eq, class Ord, class Show)
import Simple.JSON as JSON

-- A single newtype to rule them all
newtype Wrapped p a = Wrapped a

derive newtype instance eqWrapped :: Eq a => Eq (Wrapped p a)
derive newtype instance ordWrapped :: Ord a => Ord (Wrapped p a)
derive newtype instance showWrapped :: Show a => Show (Wrapped p a)
derive newtype instance readjsonWrapped :: JSON.ReadForeign a => JSON.ReadForeign (Wrapped p a)
derive newtype instance writejsonWrapped :: JSON.WriteForeign a => JSON.WriteForeign (Wrapped p a)
derive instance newtypeWrapped :: Newtype (Wrapped p a) _
