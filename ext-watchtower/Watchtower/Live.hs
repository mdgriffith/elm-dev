{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live where

import Control.Applicative ((<|>))
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import Snap.Core hiding (path)
import Snap.Http.Server
import Snap.Util.FileServe


import qualified Develop.Generate.Help
import qualified Json.Encode
import qualified Reporting.Annotation as Ann
import qualified Watchtower.StaticAssets
import qualified Watchtower.Details

import qualified Data.Text as T

import qualified Network.WebSockets      as WS
import qualified Network.WebSockets.Snap as WS



data State = State
        -- (TVar [Client], TVar (Maybe ClientId), BroadcastChan In Text, TVar Text)


init :: IO State
init =
    pure State


websocket :: State -> Snap ()
websocket _ =
    route 
        [ ("/ws", WS.runWebSocketsSnap $ handleWebSocket echo)
        ]
    


handleWebSocket :: (WS.Connection -> T.Text -> IO ()) -> WS.PendingConnection -> IO ()
handleWebSocket onReceive pending = do
    conn <- WS.acceptRequest pending
    msg <- WS.receiveData conn
    putStrLn "Received"
    putStrLn (T.unpack msg)
    onReceive conn msg


echo :: WS.Connection -> T.Text -> IO ()
echo conn msg =
    WS.sendTextData conn msg



error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder $ Develop.Generate.Help.makePageHtml "NotFound" Nothing



{- Messages!

So, we have two different kinds of messages.

If there's a better way to square this, I'd love to hear it.


1. Messages being forwarded from one client to another.  
    I.e. the editor saying a file is visible or focused.
    
2. An idea of a status subscription which you can ask for.


There are also one-off questions that can be asked via normal GET requests handeld directly by `Watchtower`.



-}
data Incoming 
    -- forwarding information from a source to somewhere else
    = EditorsVisible [ Watchtower.Details.Location ]
    | EditorActive Watchtower.Details.Location (Maybe Watchtower.Details.Position)
    | JumpTo Watchtower.Details.Location

    -- status subscription
    -- remember who calls this and send them status events as they change.
    | ElmStatusSubscriptionPlease


data Outgoing
    -- forwarding information
    = FwdEditorsVisible [ Watchtower.Details.Location ]
    | FwdEditorActive Watchtower.Details.Location (Maybe Watchtower.Details.Position)
    | FwdJumpTo Watchtower.Details.Location

    -- new information is available
    | ElmStatus Watchtower.Details.Status



