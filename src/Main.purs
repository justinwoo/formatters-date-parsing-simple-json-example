module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foreign (ForeignError(..))
import Data.Formatter.DateTime (Formatter, FormatterCommand(DayOfMonthTwoDigits, Placeholder, MonthTwoDigits, YearFull), format, unformat)
import Data.List (List(Nil), (:))
import Data.List.NonEmpty (NonEmptyList)
import Simple.JSON (readJSON)

type MyThing =
  { dateTime :: DateTime
  }

myDateFormat :: Formatter
myDateFormat
  = YearFull
  : Placeholder "/"
  : MonthTwoDigits
  : Placeholder "/"
  : DayOfMonthTwoDigits
  : Nil

parseMyThing :: String -> Either (NonEmptyList ForeignError) MyThing
parseMyThing s = do
  parsed <- readJSON s
  dateTime <- formatDateTime parsed.dateTime
  pure $ parsed { dateTime = dateTime }
  where
    formatDateTime = lmap (pure <<< ForeignError) <<< unformat myDateFormat

testJSON1 :: String
testJSON1 = """
{
  "dateTime": "2017/04/20"
}
"""

testJSON2 :: String
testJSON2 = """
{
  "dateTime": "2017/4/20"
}
"""

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logDate $ parseMyThing testJSON1
  logDate $ parseMyThing testJSON2
  -- output:
  -- 2017/04/20
  -- (NonEmptyList (NonEmpty (ForeignError "Incorrect 2-digit month(Expected 2 digits but got 1) (line 1, col 7)") Nil))
  where
    logDate (Right {dateTime}) = log $ format myDateFormat dateTime
    logDate (Left e) = log $ show e
