{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Control.Distributed.Blast.Backend.Servant.Api where

import Data.Proxy
import Data.Aeson
import Data.Text
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.ByteString as BS


import Servant.API

import Control.Distributed.Blast.Backend.Servant.Types



type SlaveApi =
   "slave" :> "cmd" :> Capture "slaveId" Int :> ReqBody '[OctetStream] LBS.ByteString :> Get '[OctetStream] LBS.ByteString
  :<|> "slave" :> "init" :> Capture "slaveId" Int :> ReqBody '[OctetStream] BS.ByteString :> Get '[JSON] ()
  :<|> "slave" :> "ping" :>  Get '[JSON] String
  :<|> "slave" :> "kill" :>  Get '[JSON] ()

type MasterApi =
   "master" :> "start" :>  ReqBody '[JSON] MasterInitConfig :> Get '[JSON] Value
  :<|> "master" :> "ping" :>  Get '[JSON] String
  :<|> "master" :> "kill" :>  Get '[JSON] ()

slaveApi :: Proxy SlaveApi
slaveApi = Proxy

masterApi :: Proxy MasterApi
masterApi = Proxy






















