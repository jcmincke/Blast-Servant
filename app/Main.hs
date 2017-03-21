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
import            Data.Aeson
import qualified  Data.List as L
import            Data.Map as M
import            Data.Binary
import            GHC.Generics (Generic)
import            Control.Monad.Logger

import Network.HTTP
--import Data.ByteString as BS
--import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as BS

import Data.Proxy
import Network.URI    ( parseURI )

import qualified Network.HTTP.Media as M
import Servant.API.ContentTypes as S

import            Control.Distributed.Blast
import            Control.Distributed.Blast.Syntax



import Control.Distributed.Blast.Backend.Servant.CliArgs
import Control.Distributed.Blast.Backend.Servant.App

import Data.Monoid
import System.Directory


-- a slow implementation of Fibonnacci
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 3
fib n = fib (n-1) + fib (n-2)

comp1 :: () -> LocalComputation ((), Int)
comp1 () = do
      -- create a remote list [32, 32, 32 ,...]
      r1 <- rconst [ (34::Int)| _ <- [1..3200::Int]]
      -- map fib over the remote list r1
      r2 <- rmap (fun fib) r1
      -- repatriate the results locally.
      l1 <- collect r2
      -- sum them
      l2 <- sum <$$> l1
      -- associate new seed
      r <- (\x -> ((), x)) <$$> l2
      return r

-- create the job, no intermediate reporting, no iteration.

jobDesc1 :: JobDesc () Int
jobDesc1 =
  MkJobDesc () comp1 reporting noIteration
  where
  reporting a b = do
    print a
    print b
    return a
  noIteration :: a -> a -> b -> Bool
  noIteration _ _ _ = True

main :: IO ()
main = do
  let config = MkConfig 1.0 True
  runServant runStdoutLoggingT toValue config jobDesc1
  where
  toValue a = toJSON a






-- runServant :: S.Serialize a =>
--   (forall t. LoggingT IO t -> IO t)
--   -> t -> Config -> JobDesc a b -> IO ()

