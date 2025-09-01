module Ext.HotClient where

import qualified Data.ByteString.Builder as B

appendHotClient :: Bool -> B.Builder -> B.Builder
appendHotClient enabled baseJs =
  if enabled then baseJs <> hotClientJS else baseJs

hotClientJS :: B.Builder
hotClientJS =
  B.string8 $ unlines
    [ "\n;(function(){\n"
    , "  try {\n"
    , "    var isSecure = (location.protocol === 'https:');\n"
    , "    var wsUrl = (isSecure ? 'wss://' : 'ws://') + location.host + '/ws';\n"
    , "    var socket;\n"
    , "    function connect(){\n"
    , "      try {\n"
    , "        socket = new WebSocket(wsUrl);\n"
    , "        socket.onopen = function(){ try{ console.info('[elm-dev][hot] connected'); }catch(_){} };\n"
    , "        socket.onmessage = function(ev){\n"
    , "          var msg = null;\n"
    , "          try { msg = JSON.parse(ev.data); } catch (_) { return; }\n"
    , "          try { window.dispatchEvent(new CustomEvent('elm-dev:hot-message', { detail: msg })); } catch(_){}\n"
    , "          // If runtime provides hot apply hook, use it; else reload on success messages.\n"
    , "          try {\n"
    , "            if (window.Elm && window.Elm.Hot && typeof window.Elm.Hot.apply === 'function') {\n"
    , "              window.Elm.Hot.apply(msg);\n"
    , "            } else {\n"
    , "              if (msg && (msg.type === 'hot-apply' || msg.type === 'compile-success')) { location.reload(); }\n"
    , "            }\n"
    , "          } catch (_e) { /* last resort */ try { location.reload(); } catch(_){} }\n"
    , "        };\n"
    , "        socket.onclose = function(){ setTimeout(connect, 1000); };\n"
    , "        socket.onerror = function(){ try { socket.close(); } catch(_){} };\n"
    , "      } catch (_err) { setTimeout(connect, 2000); }\n"
    , "    }\n"
    , "    connect();\n"
    , "  } catch (_outer) { /* ignore */ }\n"
    , "})();\n"
    ]


