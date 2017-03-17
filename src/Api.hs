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
   "cmd" :> ReqBody '[OctetStream] ByteString :> Get '[OctetStream] ByteString
--   "bin" :> ReqBody '[PlainText] String :> Get '[JSON] String
  :<|> "hello" :>  Get '[JSON] String
  :<|> "kill" :>  Get '[JSON] ()
--       "bin" :> Get '[JSON] [Entity Person]
--  :<|> "person" :> Capture "pid" (Key Person) :> Get '[JSON] (Maybe Person)
--  :<|> "create" :> "person" :> Capture "fn" Text :> Capture "fn" Text :> Get '[JSON] (Key Person)
--  :<|> "delete" :> "person" :> Capture "pid" (Key Person) :> Get '[JSON] ()
--  :<|> "update" :> "person" :> Capture "pid" (Key Person) :> Capture "fn" Text :> Capture "fn" Text :> Get '[JSON] ()
--  :<|> "static" :> Raw

api :: Proxy Api
api = Proxy





















