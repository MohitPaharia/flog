{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.Tui where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Control.Monad              (void, when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Text                  (Text, intercalate)
import qualified Graphics.Vty               as V
import qualified Graphics.Vty.CrossPlatform as VC
import           Network.HTTP.Client        (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Servant.Client

import           Client.Tui.Command
import           Client.Tui.FileSystem
import           Client.Tui.Helper
import qualified Client.Tui.Kaomoji         as Moji
import           Client.Tui.Logger          as L
import           Client.Tui.Type
import           Client.Tui.Util
import qualified Data.Text                  as T
import           Data.Time
import           Type.General
import           Type.Post                  (PostPreview (..),
                                             PostResponse (..))
import           Type.User

-- | Draw UI (pure)
drawUI :: St -> [Widget ()]
drawUI st =
  [ case mode st of
      View      -> body
      Popup l c -> popup l c
      Error err -> popupErr err
      Cli cmd   -> popupCli (focus (cmdHistory st)) cmd
  ]
 where
    body = case page st of
      Home -> emptyWidget

      Self_' (SelfResponse uid name (Email email) dob postCount created_at) ->
        center
          . borderWithLabel (txt Moji.smiling)
            $ vBox
              [ hBox [txt $ "Name: "  <>  intercalate " #" [name, T.show uid] ]
              , txt $ "Email: " <> email
              , str $ "Age: " ++ maybe "Unknown" (\date -> show (diffDays (currentDate st) date)) dob
              , str $ "Post Count: " ++ show postCount
              , str $ "With us since: " ++ show created_at
              ]

      Post_' (PostResponse postId title content createdAt updatedAt) ->
        center
          . borderWithLabel (txt $ T.show postId)
            $ vBox $ map (padLeft (Pad 1))
              [ padBottom (Pad 1) $ txt title
              , padRight Max . padBottom Max $ txt content
              ]

      PostPreview_' posts ->
        center
          . borderWithLabel (txt "Posts")
            . vBox $ map (\(PostPreview postId' title' created_at') ->
              hBox $ map txt [T.show postId', title', T.show created_at']
            ) posts

popup :: Text -> Text -> Widget ()
popup label content
  = centerLayer
    $ center
      $ borderWithLabel labelW $ padAll 5 $ padR $ txt content
  where
    l'   = T.length label
    c'   = T.length content
    padR = padRight . Pad $ if c' > l' then 0 else l' - c'
    labelW = attr "popupLabel" . txt $ label

popupErr :: Text -> Widget ()
popupErr err
  = centerLayer
    . center
      . attr "error" $ borderWithLabel (txt errLabel) . padAll 2 . padR $ txt err
  where
    errLabel  = "Error"
    errLabel' = T.length errLabel
    err'      = T.length err
    padR      = padRight . Pad $ if err' > errLabel'  then 0 else errLabel' - err'

popupCli :: Text -> Command -> Widget ()
popupCli input cmd = centerLayer $ center
  $ borderWithLabel (padRight Max $ mascotW)
    $ padTopBottom 1 $ padLeftRight 2 $
       vBox $
        [ padR $ hBox [prefixW, inputW]
        , border usageW
        ]
  where
    len  = T.length input
    padR = padRight $ Pad $ if (len < 30) then (30 - len) else 0
    mascotW      = attr "cliMascot" . txt $ mascot cmd
    prefixW      = attr "cliPrefix" . txt $ ":"
    inputW       = attr "cliInput"  . txt $ input
    usageW       = attr "cliUsage"  . txt $ usage cmd
    discriptionW = attr "cliDiscription" . txt $ description cmd

appEvent :: BrickEvent () e -> EventM () St ()
appEvent = \case

  -- enter command mode
  VtyEvent (V.EvKey (V.KChar ':') []) -> do
    st <- get

    let history  = cmdHistory st
        history' = normalize id history

    modify (\s -> s { mode = Cli noCmd, cmdHistory = history' })

  -- typing (Defaults: No Change.)
  VtyEvent (V.EvKey (V.KChar c) []) -> do
    st <- get
    case mode st of
      Cli _ -> do
        let history   = cmdHistory st
            history'  = normalize (flip T.snoc c) history
            (cmd', _) = parseCmd $ focus history'
        put st { mode = Cli cmd', cmdHistory = history' }
      _ ->
        return ()

  -- Navigate through history
  VtyEvent (V.EvKey V.KUp []) -> do
    st <- get
    case mode st of
      Cli _ -> do
        let history = moveLeft $ cmdHistory st
            input   = focus history
        modify (\s -> s { mode = Cli $ fst (parseCmd input), cmdHistory = history })
      _ -> pure ()

  VtyEvent (V.EvKey V.KDown []) -> do
    st <- get
    case mode st of
      Cli _ -> do
        let history = moveRight $ cmdHistory st
            input   = focus history
        modify (\s -> s { mode = Cli $ fst (parseCmd input), cmdHistory = history })
      _ -> pure ()


  -- backspace (Defaults: No Change.)
  VtyEvent (V.EvKey V.KBS []) -> do
    st <- get
    case mode st of
      Cli _ -> do
        let history   = cmdHistory st
            history'  = normalize (maybe "" fst . T.unsnoc) history
            (cmd', _) = parseCmd $ focus history'
        put st { mode = Cli cmd', cmdHistory = history' }
      _ -> return ()

  -- execute (Defaults: View Mode)
  VtyEvent (V.EvKey V.KEnter []) -> do
    st <- get
    case mode st of
      Cli (Command { argsResolver = resolver, execute = exec }) -> do
        let history   = cmdHistory st
            history'  = normalize id history
            input     = focus history
            prevInput = left "" history'

            history'' = pushLeft "" history'

            (_, args) = parseCmd input

        when (prevInput /= input) $ do
          liftIO $ updateHistory input
          modify (\s -> s { cmdHistory = history'' })

        case resolver args of
          Left err    -> errMode err
          Right args' -> exec args'

      _ -> modify (\s -> s { mode = View })

  -- cancel (Defaults: View Mode)
  VtyEvent (V.EvKey V.KEsc []) -> do
    st <- get

    let history  = cmdHistory st
        history' = normalize id history

    modify (\s -> s { mode = View, cmdHistory = history' })

  _ -> return ()

attr :: String -> Widget n -> Widget n
attr = withAttr . attrName

flipColor :: Widget n -> Widget n
flipColor = attr "flipBgAndFg"

attrMap' :: AttrMap
attrMap' = attrMap V.defAttr
  [ (attrName "error",      fg V.black `V.withStyle` V.bold `V.withBackColor` V.white)
  , (attrName "success",    fg V.green)
  , (attrName "popupLabel", fg V.white       `V.withBackColor` V.blue `V.withStyle    ` V.bold)
  , (attrName "warning",    fg V.yellow      `V.withStyle    ` V.bold)
  , (attrName "cliPrefix",  fg V.brightCyan  `V.withStyle    ` V.bold)
  , (attrName "cliInput",   fg V.brightBlue)
  , (attrName "cliMascot",  style V.strikethrough)
  , (attrName "cliUsage",   fg V.magenta      `V.withStyle    ` V.bold)
  ]

-- | App definition
app :: App St e ()
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = appEvent
  , appStartEvent   = return ()
  , appAttrMap      = const attrMap'
  }

mkEnv :: IO ClientEnv
mkEnv = do
  manager <- newManager tlsManagerSettings
  pure $ mkClientEnv manager (BaseUrl Http "localhost" 3000 "")

-- | Entry point
runApp :: IO ()
runApp = do
  env' <- mkEnv
  token <- getToken
  today <- utctDay <$> getCurrentTime

  resizeHistory 100
  history <- loadHistory 100

  let initialState = St { mode = View
                 , jwt         = token
                 , env         = env'
                 , page        = Home
                 , cmdHistory  = history
                 , currentDate = today }

  vty <- VC.mkVty V.defaultConfig
  void $ customMain vty (VC.mkVty V.defaultConfig) Nothing app initialState
