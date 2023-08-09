{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | This module contains helper functions to help you implement auto-reloading in your yesod site.
--
-- If this is implemented correctly, you should be able to work on your application and see the browser automatically reload when you save.
--
-- To achieve this, your site will need to have a route that serves a websocket connection.
-- You can use the helper functions in this library to implement that route.
-- You will then add a little piece of javascript to the page you want to have reload.
-- The 'autoReloadWidgetFor' function can help you add that piece of javascript.
--
-- The browser will then make a websocket connection to the websocket route and reload (semi-intelligently) as soon as the connection is closed.
module Yesod.AutoReload
  ( -- * The websocket route
    getAutoReloadR,
    getAutoReloadRWith,

    -- * The bit of javascript
    autoReloadWidgetFor,
  )
where

import Control.Concurrent
import Control.Monad
import Data.Text (Text)
import Text.Julius
import Yesod.Core
import Yesod.WebSockets

-- | A widget that takes care of reloading the page whenever the websocket connection to the given route is closed.
--
-- See 'getAutoReloadRWith' about implementing such a websocket route.
autoReloadWidgetFor :: Route site -> WidgetFor site ()
autoReloadWidgetFor reloadWebsocketRoute =
  toWidget
    [julius|

function connect (reloadAfterConnecting) {
  var uri = new URL("@{reloadWebsocketRoute}",document.baseURI).href.replace(/^http/i, "ws");
  var conn = new WebSocket(uri)
  conn.onopen = function() {
    // console.log("Listening for file changes.");
    if(reloadAfterConnecting) {
      reloadAfterConnecting = false; // Just incase this is run twice
      location.reload();
    }
  }
  conn.onclose = function(e) {
    // console.log("Connection closed using the following event, reloading.");
    // console.log(e);
    if (e) {
      // console.log(e.reason);
      if (e.reason && e.reason === "change") {
        // console.log("Only reloading, not reconnecting.");
        location.reload();
      } else {
        // console.log("Reconnecting before we reload.");
        setTimeout(function() {
          connect(true);
        }, 1000);
      }
    } else {
      // console.log("Received something that didn't look like an event, not reloading.");
    }
  }
}

connect(false);

  |]

-- | A helper function to implement the websocket route that 'autoReloadWidgetFor' will call.
--
-- The argument is a function that will block until the page is supposed to be reloaded.
-- When watching a directory for example, you will want to block until a file has changed.
-- You can use an empty 'MVar ()', a callback that fills it, and 'takeMVar' to implement such a thing.
getAutoReloadRWith :: (MonadHandler m, MonadUnliftIO m) => WebSocketsT m () -> m ()
getAutoReloadRWith waitingFunc = webSockets $ do
  waitingFunc
  sendClose ("change" :: Text)

-- |  A helper function to implement the websocket route that 'autoReloadWidgetFor' will call.
--
-- This function is like 'getAutoReloadRWith' except it takes no argument and just waits forever.
--
-- You can use this function to reload whenever the server restarts.
-- This can work nicely with @stack build --file-watch@.
getAutoReloadR :: (MonadHandler m, MonadUnliftIO m) => m ()
getAutoReloadR =
  webSockets $
    forever $ do
      sendPing ("Ping" :: Text)
      liftIO $ threadDelay 1_000_000 -- 1 second
