{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Proxy
import Data.Text
import Data.ByteString.Lazy
import Database.Persist


import Servant.API


type Api =
   "cmd" :> Capture "slaveId" Int :> ReqBody '[OctetStream] ByteString :> Get '[OctetStream] ByteString
  :<|> "ping" :>  Get '[JSON] String
  :<|> "kill" :>  Get '[JSON] ()

api :: Proxy Api
api = Proxy























