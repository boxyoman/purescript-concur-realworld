module Types.DateTimeJSON where

import Prelude

import Concur.Core (class LiftWidget, Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Monad.Except.Trans (ExceptT(..))
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format, unformatParser)
import Data.List (List(..), (:))
import Foreign (ForeignError(..), readString)
import Simple.JSON as JSON
import Text.Parsing.Parser (runParserT)

myDateFormat :: Formatter
myDateFormat
  = YearFull
  : Placeholder "-"
  : MonthTwoDigits
  : Placeholder "-"
  : DayOfMonthTwoDigits
  : Placeholder "T"
  : Hours24
  : Placeholder ":"
  : MinutesTwoDigits
  : Placeholder ":"
  : SecondsTwoDigits
  : Placeholder "."
  : Milliseconds
  : Nil

newtype MyDateTime = MyDateTime DateTime

instance showMyDateTime :: Show MyDateTime where
  show (MyDateTime dt) = show dt

unMyDateTime :: MyDateTime -> DateTime
unMyDateTime (MyDateTime a) = a

instance readForeignMyDateTime :: JSON.ReadForeign MyDateTime where
  readImpl f = do
    str <- readString f
    result <- runParserT str (unformatParser myDateFormat)
    ExceptT $ pure $ bimap (pure <<< ForeignError <<< show) MyDateTime $ result

showDate :: DateTime -> String
showDate = format formatter
  where
    formatter :: Formatter
    formatter
      = DayOfWeekNameShort
      : Placeholder " "
      : DayOfMonth
      : Placeholder " "
      : DayOfMonth
      : Placeholder " "
      : YearFull
      : Nil


viewDate
  :: forall a m
   . MultiAlternative m
  => ShiftMap (Widget HTML) m
  => LiftWidget HTML m
  => MyDateTime
  -> m a
viewDate (MyDateTime datetime) =
  D.span
    [ P.className "date" ]
    [ D.text (showDate datetime)]
