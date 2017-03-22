{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# LANGUAGE OverloadedStrings     #-}

module Main where

import Debug.Trace
import qualified  Data.List as L
import            Data.Map as M
import            Data.Binary as B
import            GHC.Generics (Generic)
import Control.Exception
import qualified Network.HTTP as Http
--import Data.ByteString as BS
--import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as BS

import Data.Aeson as A
import Data.Proxy
import Network.URI    ( parseURI )

import qualified Network.HTTP.Simple as S
import qualified Network.HTTP.Media as M
import Servant.API.ContentTypes as S
import            Control.Distributed.Blast.Distributed.Types


import Control.Distributed.Blast.Backend.Servant.CliArgs
import Control.Distributed.Blast.Backend.Servant.Types

import Options.Applicative
import Data.Monoid
import System.Directory

--data SlaveRequest = SlaveRequest Int
--  LsReqExecute RemoteClosureIndex
--  |LsReqCache Int (Data BS.ByteString)
--  |LsReqUncache Int
--  |LsReqFetch Int
--  |LsReqReset BS.ByteString
--  |LsReqBatch Int [SlaveRequest]
--  deriving (Generic, Show)


--instance Binary SlaveRequest


main :: IO ()
main = do
  cmdOpts <- execParser (info cliParser fullDesc)
  print cmdOpts
  return ()

main2 = do
  print request
  re <- Http.simpleHTTP request
  print re
  r <- Http.getResponseBody re
  print ("body", r)
  let (o::SlaveResponse) = B.decode r
  print ("object", o)
  return ()
  where
  request =
    case parseURI url of
      Nothing -> error ("postRequestWithBody: Not a valid URL - " ++ url)
      Just u  ->
        let typ = show $ contentType (Proxy::Proxy OctetStream)
--        let typ = show $ contentType (Proxy::Proxy PlainText) -- "text/plain" --
            req0 = (Http.mkRequest Http.GET u)  -- :: Request BS.ByteString
            req = setRequestBody2 req0 (typ, bs)
--            req = setRequestBody req0 (typ, body)
        in trace typ $ req0

--  url = "http://127.0.0.1:3000/hello"
  url = "http://127.0.0.1:5001/slave/test"
  bs = B.encode (LsRespVoid) -- BS.pack "bye bye"
  body = "bye bye"

setRequestBody2 :: Http.Request ByteString -> (String, ByteString) -> Http.Request ByteString
setRequestBody2 req (typ, body) = req' { Http.rqBody=body }
  where
    req' = Http.replaceHeader Http.HdrContentType typ .
           Http.replaceHeader Http.HdrContentLength (show $ BS.length body) $
           req



--httpJSON :: (MonadIO m, FromJSON a) => Request -> m (Response a)
main3 = do
  print request
  re <- try $ S.httpJSON request
  case re of
    Right r -> do
      let (body::Value) = S.getResponseBody r
      print ("body", body)
    Left (err::SomeException) -> do
      print ("error")
      print err
  return ()
  where
  masterInitConfig2 = MkMasterInitConfig [
    Address "127.0.0.1" 5001
    , Address "127.0.0.1" 5001
    , Address "127.0.0.1" 5001
    , Address "127.0.0.1" 5001
    , Address "127.0.0.1" 5001
    , Address "127.0.0.1" 5001
    , Address "127.0.0.1" 5001
    ]

  masterInitConfig = MkMasterInitConfig [
    EnvVar "SLAVE_IP" "SLAVE_PORT"
    , EnvVar "SLAVE_IP" "SLAVE_PORT"
    , EnvVar "SLAVE_IP" "SLAVE_PORT"
    , EnvVar "SLAVE_IP" "SLAVE_PORT"
    ]

  request =
    let req0 = S.parseRequest_ url
        req1 = S.setRequestBodyJSON masterInitConfig req0
    in req1
  url = "http://127.0.0.1:5000/master/start"
  bs =  (A.encode masterInitConfig)
--  bs =  (toJSON masterInitConfig)


{-}



data SlaveLocation =
  EnvVar String
  | Address String Int
  deriving Show

data MasterInitConfig = MkMasterInitConfig {
  slaveLocations :: [SlaveLocation]
}



mkRequest :: BufferType ty => RequestMethod -> URI -> Request ty
mkRequest meth uri = req
 where
  req =
    Request { rqURI      = uri
            , rqBody     = empty
            , rqHeaders  = [ Header HdrContentLength "0"
                           , Header HdrUserAgent     defaultUserAgent
                           ]
            , rqMethod   = meth
            }

  empty = buf_empty (toBufOps req)


-- 1. Perform a basic HTTP get request and return the body
get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

-- 2. Get the response code
getCode :: String -> IO ResponseCode
getCode url = simpleHTTP req >>= getResponseCode
    where req = getRequest url

postRequestWithBody
    :: String                      -- ^URL to POST to
    -> String                      -- ^Content-Type of body
    -> String                      -- ^The body of the request
    -> Request_String              -- ^The constructed request
postRequestWithBody urlString typ body =
  case parseURI urlString of
    Nothing -> error ("postRequestWithBody: Not a valid URL - " ++ urlString)
    Just u  -> setRequestBody (mkRequest POST u) (typ, body)

    setRequestBody :: Request_String -> (String, String) -> Request_String

setRequestBody req (typ, body) = req' { rqBody=body }
  where
    req' = replaceHeader HdrContentType typ .
           replaceHeader HdrContentLength (show $ length body) $
           req


           data Request a =
     Request { rqURI       :: URI   -- ^ might need changing in future
                                    --  1) to support '*' uri in OPTIONS request
                                    --  2) transparent support for both relative
                                    --     & absolute uris, although this should
                                    --     already work (leave scheme & host parts empty).
             , rqMethod    :: RequestMethod
             , rqHeaders   :: [Header]
             , rqBody      :: a
             }
-}
