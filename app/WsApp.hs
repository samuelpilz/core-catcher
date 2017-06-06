{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
This module implements the logic of the WebSocket App
-}

module WsApp where

import           ClassyPrelude

{-

Api for WS:

use cases
* react to requests with responses & state updates
* autonomously send state / push requests

-}
