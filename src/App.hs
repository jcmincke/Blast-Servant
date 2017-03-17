{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module App where

import            Control.Monad.IO.Class
import            Control.Monad.Logger (runStderrLoggingT)

import            Data.String.Conversions
--import Data.ByteString as BS
import Data.ByteString.Lazy as BS

import qualified  Data.Map as M
import qualified  Data.List as L
--import            Control.DeepSeq
import            Data.Binary
import            GHC.Generics (Generic)
import            Control.Concurrent.MVar

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import           Servant

import           Data.Text

import           Api

import Control.Distributed.Blast.Distributed.Types
import Control.Distributed.Blast.Distributed.Slave
import Control.Distributed.Blast as B
import System.Posix.Process
import System.Exit
import qualified  Data.Serialize as S
import            Control.Monad.Logger



--data SlaveMap = MkSlaveMap {
--
--  }

type SlaveMap a b = M.Map Int (Maybe (SlaveContext (LoggingT IO) a b))

server :: S.Serialize a =>
  (SlaveContext (LoggingT IO) a b)
  -> (forall c . LoggingT IO c -> IO c)
  -> MVar (SlaveMap a b) -> Server Api
server slaveContext0 slaveLogger slaveMapMVar =

  processSlaveCommand slaveContext0 slaveLogger slaveMapMVar
  :<|> getPing
  :<|> getKill



processSlaveCommand slaveContext0 slaveLogger slaveMapMVar slaveId bs = do
  slaveContextM <- liftIO $ getContext slaveMapMVar
  case slaveContextM of
    Just slaveContext -> do
      let (slaveRequest::SlaveRequest) = decode bs
      (resp, slaveContext') <- liftIO $ slaveLogger $ runCommand slaveRequest slaveContext
      let respBs = encode resp
      isOk <- liftIO $ setContext slaveContext' slaveMapMVar
      if isOk
        then return respBs
        else do let errBs = encode $ LsRespError ("Non sequential slave request for slave (slave does not exist):" ++ show slaveId)
                return errBs

    Nothing -> do let errBs = encode $ LsRespError ("Non sequential slave request for slave (slave busy):" ++ show slaveId)
                  return errBs
  where
  getContext mvar = modifyMVar mvar $ \slaveMap ->
    case M.lookup slaveId slaveMap of
      Just (Just slaveContext) -> return (M.insert slaveId Nothing slaveMap, Just slaveContext)
      Just Nothing -> do
        return (M.insert slaveId Nothing slaveMap, Just slaveContext0)
      Nothing -> return (slaveMap, Nothing)

  setContext slaveContext mvar = modifyMVar mvar $ \slaveMap ->
    case M.lookup slaveId slaveMap of
      Just (Just slaveContext) -> return (slaveMap, False)
      Just Nothing -> return (M.insert slaveId (Just slaveContext) slaveMap, True)
      Nothing -> return (slaveMap, False)

--  action slaveContext = do
--    let (slaveRequest::SlaveRequest) = decode bs
--    (resp, slaveContext') <- slaveLogger $ runCommand slaveRequest slaveContext
--    let respBs = encode resp
--    return (slaveContext', respBs)


getPing = do
  return "Pinged"

getKill = do
  liftIO $ exitImmediately (ExitFailure 42)
  return ()




mkApp :: S.Serialize a => (forall c. LoggingT IO c -> IO c) -> B.Config -> B.JobDesc a b -> IO Application
mkApp slaveLogger config jobDesc = do
  let slaveContext0 = makeSlaveContext config 0 jobDesc  -- slave id has no use
  mvar <- newMVar M.empty
  return $ serve api $ server slaveContext0 slaveLogger mvar

runSlave :: (S.Serialize a) => (forall c. LoggingT IO c -> IO c) -> Int -> B.Config -> B.JobDesc a b -> IO ()
runSlave slaveLogger port config jobDesc =
  Warp.run port =<< mkApp slaveLogger config jobDesc










{-
  slaveLogger :: forall m a. (MonadIO m) => LoggingT m a -> m a   -- ^ Logger.

makeSlaveContext :: (MonadLoggerIO m)
  => Config               -- ^ Configuration
  -> Int                  -- ^Index of the slave.
  -> JobDesc a b          -- ^ Job description
  -> SlaveContext m a b   -- ^ Slave Context


-- | Describes the current context of a slave.
data SlaveContext m a b = MkSlaveContext {
  localSlaveId :: Int
  , infos :: InfoMap
  , vault :: V.Vault
  , expGen :: a -> ProgramT (Syntax m) m (SExp 'Local (a, b))
  , config :: Config
  }

-- | Creates a "SlaveContext" for a given slave.
makeSlaveContext :: (MonadLoggerIO m)
  => Config               -- ^ Configuration
  -> Int                  -- ^Index of the slave.
  -> JobDesc a b          -- ^ Job description
  -> SlaveContext m a b   -- ^ Slave Context
makeSlaveContext config slaveId (MkJobDesc {..}) =
  MkSlaveContext slaveId M.empty V.empty computationGen config

-- | Runs the given command against the specified state of a slave.
runCommand :: forall a b m. (S.Serialize a, MonadLoggerIO m)
  => SlaveRequest                          -- ^ Command.
  -> SlaveContext m a b                    -- ^ Slave context
  -> m (SlaveResponse, SlaveContext m a b) -- ^ Returns the response from the slave and the new slave context.



runDB pool query = flip runSqlPersistMPool pool query

getPersons pool = do
  persons <- runDB pool $ selectList [] [Asc PersonPersonName, Asc PersonPersonFirstName]
  return $  persons

getPerson pool pid = do
      mPerson <- runDB pool $ get pid
      return $ mPerson



createPerson pool fn n = runDB pool $ do
    oid <- insert (Person fn n)
    return oid


deletePerson pool pid = do
    oid <- runDB pool $ delete pid
    return ()


updatePerson pool personId fn n = do
    oid <- runDB pool $ repsert personId (Person fn n)
    return ()
-}



  {-}
app :: ConnectionPool -> Application
app pool = serve api $ server pool


mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createPostgresqlPool "host=localhost port=5432 user=V3 dbname=phoenix password=" 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 3000 =<< mkApp sqliteFile
  -}
