{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RecordWildCards      #-}

module Control.Distributed.Blast.Backend.Servant.Types where

import Control.Applicative
import Data.Foldable
import Data.Proxy
import Data.Aeson
import Data.Aeson.Parser
import Data.Text
import Data.ByteString.Lazy


data SlaveLocation =
  EnvVar String
  | Address String Int


data MasterInitConfig = MkMasterInitConfig {
  slaveLocations :: [SlaveLocation]
}



instance FromJSON SlaveLocation where
  parseJSON = withObject "SlaveLocation" $ \o -> asum [
    EnvVar <$> o .: "envVar" ,
    Address <$> o .: "ip" <*> o .: "port" ]

instance FromJSON MasterInitConfig where
  parseJSON = withObject "MasterInitConfig" $ \o ->
    MkMasterInitConfig <$> o .: "slaveLocations"


instance ToJSON SlaveLocation where
  toJSON (EnvVar n) = object [
    "envVar" .= n]
  toJSON (Address ip port) = object [
    "ip" .= ip
    , "port" .= port
     ]


instance ToJSON MasterInitConfig where
  toJSON (MkMasterInitConfig {..}) =  object [
    "slaveLocations" .= slaveLocations]









