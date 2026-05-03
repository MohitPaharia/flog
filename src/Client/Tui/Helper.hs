module Client.Tui.Helper where

import           Brick.Types
import           Data.Text       (Text)

import           Client.Tui.Type

popupMode :: Text -> Text -> EventM () St ()
popupMode label content = modify (\s -> s { mode = Popup label content })

errMode :: Text -> EventM () St ()
errMode err = modify (\s -> s { mode = Error err })
