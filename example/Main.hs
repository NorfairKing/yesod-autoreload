{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Yesod.AutoReload
import Yesod.Core

data App = App

mkYesod
  "App"
  [parseRoutes|
    / HomeR GET
    /ws WebsocketR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ "Hello World" <> autoReloadWidgetFor WebsocketR

getWebsocketR :: Handler ()
getWebsocketR = getAutoReloadR

main :: IO ()
main = warp 8000 App
