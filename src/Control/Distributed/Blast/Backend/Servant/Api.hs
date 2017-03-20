{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Control.Distributed.Blast.Backend.Servant.Api where

import Data.Proxy
import Data.Aeson
import Data.Text
import Data.ByteString.Lazy
import Database.Persist

import Servant.API

import Control.Distributed.Blast.Backend.Servant.Types



type SlaveApi =
   "slave" :> "cmd" :> Capture "slaveId" Int :> ReqBody '[OctetStream] ByteString :> Get '[OctetStream] ByteString
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






















