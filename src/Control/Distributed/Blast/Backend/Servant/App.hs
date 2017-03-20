{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


module Control.Distributed.Blast.Backend.Servant.App where

import            Control.Concurrent
import            Control.Concurrent.Async
import            Control.Concurrent.MVar
import            Control.Monad.IO.Class
import            Control.Monad.Logger

--import            Data.Aeson (toJson)
import            Data.Aeson.Types (toJSON, Value)
import            Data.Binary as B
import            Data.ByteString.Lazy as BS
import qualified  Data.List as L
import qualified  Data.Map as M
import qualified  Data.Serialize as S
import            Data.Text


import            GHC.Generics (Generic)

import            Network.HTTP
import            Network.HTTP.Base as Http
import qualified  Network.HTTP.Media as M
import            Network.URI    ( parseURI )
import            Network.Wai
import            Network.Wai.Handler.Warp as Warp

import            System.Exit
import            System.Posix.Process


import            Servant
import            Servant.API.ContentTypes as CT



import            Control.Distributed.Blast as B
import            Control.Distributed.Blast.Distributed.Interface
import            Control.Distributed.Blast.Distributed.Slave
import            Control.Distributed.Blast.Distributed.Types

import            Control.Distributed.Blast.Backend.Servant.Api
import            Control.Distributed.Blast.Backend.Servant.Types


type SlaveMap a b = M.Map Int (Maybe (SlaveContext (LoggingT IO) a b))

data MasterRoleContext a = MkMasterRoleContext {
  slaveLocations :: M.Map Int (String, Int)
  , seedM :: Maybe a
  , statefulSlaveMode :: Bool


  }



data SlaveRoleContext a b = MkSlaveRoleContext {
    slaveMap :: SlaveMap a b
  }






slaveServer :: S.Serialize a =>
  (SlaveContext (LoggingT IO) a b)
  -> (forall c . LoggingT IO c -> IO c)
  -> MVar (SlaveRoleContext a b) -> Server SlaveApi
slaveServer slaveContext0 slaveLogger slaveMapMVar =
  processSlaveCommand slaveContext0 slaveLogger slaveMapMVar
  :<|> getPing "Slave"
  :<|> getKill


processSlaveCommand :: (Binary a, S.Serialize a1, MonadIO m) =>
  SlaveContext (LoggingT IO) a1 b
  -> (LoggingT IO (SlaveResponse, SlaveContext (LoggingT IO) a1 b)
      -> IO (a, SlaveContext (LoggingT IO) a1 b))
  -> MVar (SlaveRoleContext a1 b)
  -> Int
  -> ByteString
  -> m ByteString
processSlaveCommand slaveContext0 logger slaveRoleContextMVar slaveId bs = do
  slaveContextM <- liftIO $ getContext slaveRoleContextMVar
  case slaveContextM of
    Just slaveContext -> do
      let (slaveRequest::SlaveRequest) = decode bs
      (resp, slaveContext') <- liftIO $ logger $ runCommand slaveRequest slaveContext
      let respBs = encode resp
      isOk <- liftIO $ setContext slaveContext' slaveRoleContextMVar
      if isOk
        then return respBs
        else do let errBs = encode $ LsRespError ("Non sequential slave request for slave (slave does not exist):" ++ show slaveId)
                return errBs

    Nothing -> do let errBs = encode $ LsRespError ("Non sequential slave request for slave (slave busy):" ++ show slaveId)
                  return errBs
  where
  getContext mvar = modifyMVar mvar $ \(MkSlaveRoleContext {..}) ->
    case M.lookup slaveId slaveMap of
      Just (Just slaveContext) -> return (MkSlaveRoleContext $ M.insert slaveId Nothing slaveMap, Just slaveContext)
      Just Nothing -> do
        return (MkSlaveRoleContext $ M.insert slaveId Nothing slaveMap, Just slaveContext0)
      Nothing -> return (MkSlaveRoleContext $ slaveMap, Nothing)

  setContext slaveContext mvar = modifyMVar mvar $ \(MkSlaveRoleContext {..}) ->
    case M.lookup slaveId slaveMap of
      Just (Just slaveContext) -> return (MkSlaveRoleContext $ slaveMap, False)
      Just Nothing -> return (MkSlaveRoleContext $ M.insert slaveId (Just slaveContext) slaveMap, True)
      Nothing -> return (MkSlaveRoleContext $ slaveMap, False)




instance (S.Serialize a) => CommandClass MasterRoleContext a where
  isStatefullSlave (MkMasterRoleContext{ statefulSlaveMode = mode }) = mode
  getNbSlaves (MkMasterRoleContext {..}) = M.size slaveLocations

  send s@(MkMasterRoleContext {..}) slaveId req = do
--    randomSlaveReset s slaveId
    re <- simpleHTTP request
    case re of
      Right (Response {..}) -> do
        case rspCode of
          (2,_,_) ->
            case B.decode rspBody of
             -- Right (o::SlaveResponse) -> Right o
              Right o -> return $ Right o
              Left err -> return $ Left err
    where
    (ip, port) = slaveLocations M.! slaveId
    url = "http://"++ip++":"++show port ++ "slave/cmd/"++show slaveId
    bs = B.encode req
    request =
      case parseURI url of
        Nothing -> error ("CommandClass.send: Not a valid URL - " ++ url)
        Just u  ->
          let typ = show $ contentType (Proxy::Proxy OctetStream)
              req0 = (mkRequest Http.GET u) :: Http.Request BS.ByteString
              req1 =  replaceHeader HdrContentType typ .
                      replaceHeader HdrContentLength (show $ BS.length bs) $
                      req0
              req = req1 { rqBody=bs }
          in req

  stop _ = return ()
  setSeed s@(MkMasterRoleContext {..}) a = do
    let s' = s {seedM = Just a}
    resetAll s'
    return s'
    where
    resetAll as = do
      let nbSlaves = getNbSlaves as
      let slaveIds = [0 .. nbSlaves - 1]
      let req = resetCommand (S.encode a)
      _ <- mapConcurrently (\slaveId -> send as slaveId req) slaveIds
      return ()


getPing :: Monad m => String -> m String
getPing msg = do
  return ("Hello, I am your "++msg)

getKill :: MonadIO m => m ()
getKill = do
  liftIO $ exitImmediately (ExitFailure 42)
  return ()


masterServer :: (S.Serialize a) =>
  (LoggingT IO (a, b) -> IO (a, b))
  -> (b -> Value)
  -> Config
  -> JobDesc a b
  -> Server MasterApi
masterServer logger toValue blastConfig jobDesc =
  (runMaster logger toValue blastConfig jobDesc)
  :<|> (getPing "Slave")
  :<|> getKill



runMaster :: (S.Serialize a, MonadIO m) =>
  (LoggingT IO (a, b) -> IO (a, b))
  -> (b -> Value)
  -> Config
  -> JobDesc a b
  -> MasterInitConfig
  -> m Value
runMaster logger toValue blastConfig jobDesc (MkMasterInitConfig{..}) = do
  (_, b) <- liftIO $ doRunRec logger blastConfig masterRoleContext jobDesc
  -- todo fix
  return $ toValue b
  where
  -- build slave location map
  slaveLocMap = M.fromList $ L.zipWith (\i loc ->
      case loc of
        Address ip port -> (i, (ip, port))
        _ -> error "env var not yet supported"
        ) [0 ..] slaveLocations
  masterRoleContext = MkMasterRoleContext slaveLocMap Nothing False


doRunRec :: (CommandClass s a) =>
  (LoggingT IO (a, b) -> IO (a, b)) -> Config -> s a -> JobDesc a b -> IO (a, b)
doRunRec logger blastConfig@(MkConfig {..}) masterRoleContext (jobDesc@MkJobDesc {..}) = do
  (a, b) <- logger $ runComputation blastConfig masterRoleContext jobDesc
  a' <- liftIO $ reportingAction a b
  case shouldStop seed a' b of
    True -> return (a', b)
    False -> doRunRec logger blastConfig masterRoleContext (jobDesc {seed = a'})

mkSlaveApp :: S.Serialize a => (forall c. LoggingT IO c -> IO c) -> B.Config -> B.JobDesc a b -> IO Application
mkSlaveApp slaveLogger blastConfig jobDesc = do
  let slaveContext0 = makeSlaveContext blastConfig 0 jobDesc  -- slave id has no use
  mvar <- newMVar $ (MkSlaveRoleContext M.empty)
  return $ serve slaveApi $ slaveServer slaveContext0 slaveLogger mvar

runSlaveServer :: (S.Serialize a) => (forall c. LoggingT IO c -> IO c) -> Int -> B.Config -> B.JobDesc a b -> IO ()
runSlaveServer slaveLogger port blastConfig jobDesc =
  Warp.run port =<< mkSlaveApp slaveLogger blastConfig jobDesc





mkMasterApp :: (S.Serialize a, Monad m) =>
  (LoggingT IO (a, b) -> IO (a, b))
  -> (b -> Value)
  -> Config
  -> JobDesc a b
  -> m Application
mkMasterApp logger toValue blastConfig jobDesc = do
  return $ serve masterApi $ masterServer logger toValue blastConfig jobDesc

runMasterServer :: S.Serialize a =>
  (LoggingT IO (a, b) -> IO (a, b))
  -> Port
  -> (b -> Value)
  -> Config
  -> JobDesc a b
  -> IO ()
runMasterServer logger port toValue blastConfig jobDesc =
  Warp.run port =<< mkMasterApp logger toValue blastConfig jobDesc


-- europe-west1-b


{-

ensureNoRole mvar action = do
  modifyMVar mvar handler
  where
  handler NoRole = do
    (res, newRole) <- action
    return (Right res, newRole)
  handler _ = return (Left "error", NoRole)

withMasterRole mvar action = do
  modifyMVar mvar handler
  where
  handler (MasterRoleContext ctx) = do
    (res, newCtx) <- action
    return (Right res, MasterRoleContext newCtx)
  handler rctx = return (Left "error", rctx)

withSlaveRole mvar action = do
  rctx <- readMVar mvar

  modifyMVar mvar handler
  where
  handler (MasterRoleContext ctx) = do
    (res, newCtx) <- action
    return (Right res, MasterRoleContext newCtx)
  handler rctx = return (Left "error", rctx)

-}



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
