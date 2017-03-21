{-# LANGUAGE DeriveGeneric #-}
module Control.Distributed.Blast.Backend.Servant.CliArgs
where

import Options.Applicative
import Data.Monoid
import GHC.Generics (Generic)




import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import Data.Aeson


import Control.Distributed.Blast.Backend.Servant.Types

data Cred = Cred { username :: String, password :: String } deriving (Show, Generic) -- (1,2)
instance FromJSON Cred -- (3)


readSlaveLocationFiles :: String -> IO [SlaveLocation]
readSlaveLocationFiles fn = do
    content <- BS.readFile fn
    let parsedContent = Y.decode content :: Maybe [SlaveLocation]
    case parsedContent of
        Nothing -> error "Could not slave location file."
        (Just l) -> return l



data MasterOpts = MkMasterOpts {
  masterPort :: String
  , slaveFile :: Maybe String
  }
  deriving (Show)

data SlaveOpts = MkSlaveOpts {
  slavePort :: String
  }
  deriving (Show)

data Opts =
    MasterOpts MasterOpts
    | SlaveOpts SlaveOpts
  deriving (Show)


cliParser :: Parser Opts
cliParser =
  (subparser $ command "master"
        (info masterParser fullDesc))
    <|> (subparser $ command "slave"
            (info slaveParser fullDesc))

  where
  masterParser :: Parser Opts
  masterParser =
    MasterOpts <$>
      (MkMasterOpts
        <$> strOption (
              long "port"
              <> short 'p')
        <*> optional (strOption (
              long "slave-file"
              <> short 'f'))
      )
  slaveParser =
    SlaveOpts <$>
      (MkSlaveOpts
        <$> strOption (
              long "port"
              <> short 'p')
      )

parseCliArgs :: IO Opts
parseCliArgs = execParser (info cliParser fullDesc)

--info (runOptsParser curVersion) (fullDesc

--      acommand :: Parser A
--acommand = subparser $ command "a" (info (A <$> (helper <*> boption))
--                                         (progDesc "you can '-b' if you like with 'a'"))
--
--main = execParser (info (helper <*> optional acommand) fullDesc) >>= print
--