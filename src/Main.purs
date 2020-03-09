module Main where

import Prelude

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, debounce, display, dyn, fireOnce, justWait, loopS, step)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alt((<|>))
import Control.Plus(empty)
import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..), hush)
import Data.Enum (toEnum)
import Data.Formatter.DateTime as FDT
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time (Time(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)

-- Or with a supplied initial value instead of using a Maybe
runEffectInit :: forall a. a -> Effect a -> Signal HTML a
runEffectInit i e = step i do
  a <- liftEffect e
  pure (step a empty)

initDate :: DateTime
initDate = makeDateTime 0 0 0 0 0 0 0

-- TODO: what to do with this?
makeDateTime ∷ Int → Int → Int → Int → Int → Int → Int → DateTime
makeDateTime year month day hour minute second millisecond =
  DateTime
    (canonicalDate
      (fromMaybe bottom $ toEnum year)
      (fromMaybe bottom $ toEnum month)
      (fromMaybe bottom $ toEnum day))
    (Time
        (fromMaybe bottom $ toEnum hour )
        (fromMaybe bottom $ toEnum minute )
        (fromMaybe bottom $ toEnum second )
        (fromMaybe bottom $ toEnum millisecond))

-- TODO: add UTC offset or 'Z': https://www.w3schools.com/xml/schema_dtypes_date.asp
formatXsdDate :: DateTime -> Either String String
formatXsdDate dt = FDT.formatDateTime "YYYY-MM-DD" dt

dateTimeWidg :: Widget HTML DateTime
dateTimeWidg = do
  liftEffect nowDateTime

dateTimeSig :: Signal HTML DateTime
dateTimeSig = justWait initDate (fireOnce dateTimeWidg) pure

hello :: String -> Signal HTML String
hello s = step s do
  greeting <- D.div'
    [ "Hello" <$ D.button [P.onClick] [D.text "Say Hello"]
    , "Namaste" <$ D.button [P.onClick] [D.text "Say Namaste"]
    ]
  _ <- D.text (greeting <> " Sailor!") <|> D.button [P.onClick] [D.text "restart"]
  pure (hello greeting)

outerLoop :: Signal HTML (Maybe DateTime)
outerLoop = loopS Nothing \lastDateMay -> D.div_ [] do
  helloOut <- hello "INIT"
  dateTime <- dateTimeSig
  display $ D.text helloOut
  pure $ pure $ dateTime

main :: Effect Unit
main = runWidgetInDom "root" (dyn outerLoop)
