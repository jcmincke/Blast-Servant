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

import Debug.Trace
import            Control.Concurrent
import            Control.Concurrent.Async
import            Control.Concurrent.MVar
import            Control.Exception
import            Control.Monad
import            Control.Monad.IO.Class
import            Control.Monad.Logger

import            Data.Aeson.Types (toJSON, Value)
import            Data.Binary as B
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.ByteString as BS
import qualified  Data.List as L
import qualified  Data.Map as M
import qualified  Data.Serialize as S (Serialize, encode, decode)
import            Data.Text


import            GHC.Generics (Generic)

import            Network.HTTP
import            Network.HTTP.Base as Http
import qualified  Network.HTTP.Media as M
import            Network.URI    ( parseURI )
import            Network.Stream
import            Network.Wai
import            Network.Wai.Handler.Warp as Warp

import            System.Random

import            System.Exit
import            System.Environment
import            System.Posix.Process


import            Servant
import            Servant.API.ContentTypes as CT



import            Control.Distributed.Blast as B
import            Control.Distributed.Blast.Distributed.Interface
import            Control.Distributed.Blast.Distributed.Slave as Slave
import            Control.Distributed.Blast.Distributed.Types

import            Control.Distributed.Blast.Backend.Servant.Api
import            Control.Distributed.Blast.Backend.Servant.CliArgs
import            Control.Distributed.Blast.Backend.Servant.Types

type SlaveMap a b = M.Map Int (Maybe (SlaveContext (LoggingT IO) a b))

data MasterRoleContext a = MkMasterRoleContext {
  slaveLocations :: M.Map Int (String, Int)
  , theSeed :: a
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
  :<|> initSlave slaveContext0 slaveLogger slaveMapMVar
  :<|> getPing "Slave"
  :<|> getKill


initSlave :: (S.Serialize a, MonadIO m) =>
  SlaveContext (LoggingT IO) a b
  -> (LoggingT IO (SlaveResponse, SlaveContext (LoggingT IO) a b)
      -> IO (SlaveResponse, SlaveContext (LoggingT IO) a b))
  -> MVar (SlaveRoleContext a b)
  -> Int
  -> BS.ByteString
  -> m ()
initSlave slaveContext0 logger slaveRoleContextMVar slaveId bs = do
  liftIO $ modifyMVar_ slaveRoleContextMVar createSlave
  where
  createSlave (MkSlaveRoleContext {..}) = do
    (_, slaveContext) <- liftIO $ logger $ Slave.runCommand (LsReqReset bs) slaveContext0
    return (MkSlaveRoleContext $ M.insert slaveId (Just slaveContext) slaveMap)


--data SlaveContext m a b = MkSlaveContext {
--  localSlaveId :: Int  -- todo maybe not useful
--  , infos :: InfoMap
--  , vault :: V.Vault
--  , expGen :: a -> ProgramT (Syntax m) m (SExp 'Local (a, b))
--  , config :: Config
--  }


--randomSlaveReset :: (S.Serialize a) => Controller a -> Int -> IO ()
randomSlaveFailure (MkSlaveContext {..}) = do
  let (MkConfig{..}) = config
  r <- randomRIO (0.0, 1.0)
  when (r > slaveAvailability) $ do
    putStrLn "killing slave"
    error "killing slave"

processSlaveCommand :: (S.Serialize a, MonadIO m) =>
  SlaveContext (LoggingT IO) a b
  -> (LoggingT IO (SlaveResponse, SlaveContext (LoggingT IO) a b)
      -> IO (SlaveResponse, SlaveContext (LoggingT IO) a b))
  -> MVar (SlaveRoleContext a b)
  -> Int
  -> LBS.ByteString
  -> m LBS.ByteString
processSlaveCommand slaveContext0 logger slaveRoleContextMVar slaveId bs = do
  slaveContextM <- liftIO $ getContext slaveRoleContextMVar
  case slaveContextM of
    Right slaveContext -> do
      let (slaveRequest::SlaveRequest) = B.decode bs
      liftIO $ randomSlaveFailure slaveContext
      (resp::SlaveResponse, slaveContext') <- liftIO $ logger $ Slave.runCommand slaveRequest slaveContext
      let respBs = B.encode resp
      setResp <- liftIO $ setContext slaveContext' slaveRoleContextMVar
      case setResp of
        Nothing -> return respBs
        Just err ->  do
          let errBs = B.encode $ LsRespError ("Slave: " ++ show slaveId ++ ": " ++ err)
          return errBs

    Left err -> do
      let errBs = B.encode $ LsRespError ("Slave: " ++ show slaveId ++ ": " ++ err)
      return errBs
  where
  getContext mvar = modifyMVar mvar $ \(MkSlaveRoleContext {..}) ->
    case M.lookup slaveId slaveMap of
      Just (Just slaveContext) -> do
        -- We have a slave, available.
        return (MkSlaveRoleContext $ M.insert slaveId Nothing slaveMap, Right slaveContext)
      Just Nothing -> do
        -- We have a slave but it is busy.
        return (MkSlaveRoleContext $ slaveMap, Left "Sending a command to a busy slave")
      Nothing -> do
        -- We do not have a slave with the given slaveID.
        -- It should have been initialized first
        return (MkSlaveRoleContext $ slaveMap, Left "Sending a command to a non-initialized slave")

  setContext slaveContext mvar = modifyMVar mvar $ \(MkSlaveRoleContext {..}) ->
    case M.lookup slaveId slaveMap of
      Just (Just slaveContext) ->
        -- Trying to release an available slave: should not happen.
        return (MkSlaveRoleContext $ slaveMap, Just "Trying to release an available slave: should not happen")
      Just Nothing ->
        -- Trying to release an busy slave: that's ok.
        return (MkSlaveRoleContext $ M.insert slaveId (Just slaveContext) slaveMap, Nothing)
      Nothing ->
        -- Trying to release an inexistent slave: should not happen.
        return (MkSlaveRoleContext $ slaveMap, Just "Trying to release an inexistent slave: should not happen.")



simpleHTTP' request = do
  re <- try $ simpleHTTP request
  case re of
    Right x -> return $ x
    Left (exc::SomeException) -> return $ Left (ErrorMisc $ show exc)


instance (S.Serialize a) => CommandClass MasterRoleContext a where
  isStatefullSlave (MkMasterRoleContext{ statefulSlaveMode = mode }) = mode
  getNbSlaves (MkMasterRoleContext {..}) = M.size slaveLocations

  send s@(MkMasterRoleContext {..}) slaveId req = do
    go 5 1000 request
    where
    go 0 _ _ = do
      putStrLn ("Slave:" ++ show slaveId ++ " is not responding. The computation is doomed...")
      return $ Left ("Slave does not respond:" ++ show slaveId)
    go count delay request = do
      re <- simpleHTTP' request
      case re of
        Right (httpResp@Response {..}) -> do
          case rspCode of
            (2,_,_) -> do
              body <- getResponseBody re
              return $ Right $  B.decode body
            _ -> do
                r <- goInit count delay
                case r of
                  Nothing -> go count delay request
                  Just err -> return $ Left err
        Left err -> do
          print err
          r <- goInit count delay
          case r of
            Nothing -> go count delay request
            Just err -> return $ Left err
    goInit (0::Int) _ = do
      putStrLn ("Slave:" ++ show slaveId ++ " is not responding. The computation is doomed...")
      return $ Just ("Slave does not respond:" ++ show slaveId)
    goInit count delay = do
      putStrLn ("Failing Slave, trying to reinitialize it (count="++show count++")")
      threadDelay (delay*1000)
      -- in case a slave is restarted, it is the backend's responsibility to initialize it.
      isOk <- masterInitSlave s slaveId
      if isOk
        then do
          putStrLn ("Slave:" ++ show slaveId ++ " is re-initialized")
          return Nothing
        else do goInit (count - 1) (delay+1000)

    (ip, port) = slaveLocations M.! slaveId
    url = "http://"++ip++":"++show port ++ "/slave/cmd/"++show slaveId
    bs = B.encode req
    request =
      case parseURI url of
        Nothing -> error ("CommandClass.send: Not a valid URL - " ++ url)
        Just u  ->
          let typ = show $ contentType (Proxy::Proxy OctetStream)
              req0 = (mkRequest Http.GET u) :: Http.Request LBS.ByteString
              req1 =  replaceHeader HdrContentType typ .
                      replaceHeader HdrContentLength (show $ LBS.length bs) $
                      req0
              req = req1 { rqBody=bs }
          in req

  stop _ = return ()
  setSeed s@(MkMasterRoleContext {..}) a = do
    let s' = s {theSeed = a}
    resetAll s'
    return s'
    where
    resetAll as = do
      let nbSlaves = getNbSlaves as
      let slaveIds = [0 .. nbSlaves - 1]
      let req = resetCommand (S.encode a)
      _ <- mapConcurrently (\slaveId -> send as slaveId req) slaveIds
      return ()

masterInitAllSlaves :: S.Serialize a => MasterRoleContext a -> IO Bool
masterInitAllSlaves s@(MkMasterRoleContext {..}) = do
  rbs <- mapM (masterInitSlave s) $ M.keys slaveLocations
  return $ L.all id rbs


masterInitSlave :: S.Serialize a => MasterRoleContext a -> Int -> IO Bool
masterInitSlave s@(MkMasterRoleContext {..}) slaveId = do
  re <- simpleHTTP' request
  case re of
    Right (httpResp@Response {..}) -> do
      case rspCode of
        (2,_,_) -> return True
        _ -> do
          putStrLn ("Cannot initialize slave: "++show slaveId)
          return False
    Left err -> do
      print err
      putStrLn ("Cannot initialize slave: "++show slaveId)
      return False
  where
  (ip, port) = slaveLocations M.! slaveId
  url = "http://"++ip++":"++show port ++ "/slave/init/"++show slaveId
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
  bs = S.encode theSeed


getPing :: MonadIO m => String -> m String
getPing msg = do
  liftIO $ putStrLn "Pinged"
  return ("Hello, I am your "++msg)

getKill :: MonadIO m => m ()
getKill = do
  liftIO $ exitImmediately (ExitFailure 42)
  return ()


masterServer :: (S.Serialize a) =>
  (forall t. LoggingT IO t -> IO t)
  -> (b -> Value)
  -> Config
  -> JobDesc a b
  -> Server MasterApi
masterServer logger toValue blastConfig jobDesc =
  (runMaster logger toValue blastConfig jobDesc)
  :<|> (getPing "Master")
  :<|> getKill




runMaster :: (S.Serialize a, MonadIO m) =>
  (forall t. LoggingT IO t -> IO t)
  -> (b -> c)
  -> Config
  -> JobDesc a b
  -> MasterInitConfig -> m c
runMaster logger toValue blastConfig jobDesc (MkMasterInitConfig{..}) = do
  liftIO $ putStrLn "Starting Master"
  slaveLocMap <- liftIO $ createSlaveLocMap slaveLocations
  let masterRoleContext = MkMasterRoleContext slaveLocMap (seed jobDesc) False
  slavesInitialized <- liftIO $ masterInitAllSlaves masterRoleContext
  if slavesInitialized
    then do
      (_, b) <- liftIO $ doRunRec logger blastConfig masterRoleContext jobDesc
      return $ toValue b
    else error "Cannot initialize all the slaves"


doRunRec ::  (S.Serialize a) =>
  (forall t. LoggingT IO t -> IO t) -> Config -> MasterRoleContext a -> JobDesc a b -> IO (a, b)
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
runSlaveServer logger port blastConfig jobDesc =
  Warp.run port =<< mkSlaveApp logger blastConfig jobDesc





mkMasterApp :: (S.Serialize a, Monad m) =>
  (forall t. LoggingT IO t -> IO t)
  -> (b -> Value)
  -> Config
  -> JobDesc a b
  -> m Application
mkMasterApp logger toValue blastConfig jobDesc = do
  return $ serve masterApi $ masterServer logger toValue blastConfig jobDesc

runMasterServer :: S.Serialize a =>
  (forall t. LoggingT IO t -> IO t)
  -> Port
  -> (b -> Value)
  -> Config
  -> JobDesc a b
  -> IO ()
runMasterServer logger port toValue blastConfig jobDesc =
  Warp.run port =<< mkMasterApp logger toValue blastConfig jobDesc


runServant :: S.Serialize a =>
  (forall t. LoggingT IO t -> IO t)
  -> (b -> Value) -> Config -> JobDesc a b -> IO ()
runServant logger toValue blastConfig jobDesc = do
  cmdOpts <- parseCliArgs
  case cmdOpts of
    MasterOpts opts -> runServantAsMaster opts
    SlaveOpts opts -> runServantAsSlave opts
  print cmdOpts
  return ()
  where

  runServantAsMaster (MkMasterOpts {..}) =
    case slaveFile of
      Just fn -> do
        slaveLocations <- readSlaveLocationFiles fn
        slaveLocMap <-  createSlaveLocMap slaveLocations
        let masterRoleContext = MkMasterRoleContext slaveLocMap (seed jobDesc) False
        slavesInitialized <- masterInitAllSlaves masterRoleContext
        if slavesInitialized
          then do
            liftIO $ doRunRec logger blastConfig masterRoleContext jobDesc
            return ()
          else error "Cannot initialize all the slaves"
      Nothing -> do
        let (port::Int) = read masterPort
        runMasterServer logger port toValue blastConfig jobDesc

  runServantAsSlave (MkSlaveOpts {..}) = do
    let (port::Int) = read slavePort
    putStrLn "Starting Slave Mode"
    runSlaveServer logger port blastConfig jobDesc
    return ()

createSlaveLocMap slaveLocations = do
  (mapM proc $ L.zip [0 ..] slaveLocations) >>= (return . M.fromList)
  where
  proc (i, (Address ip port)) = return (i, (ip, port))
  proc (i, (EnvVar ipn portn)) = do
    putStrLn $ "Retrieving env vars: "++ ipn ++" and "++portn
    ipM <- lookupEnv ipn
    portM <- lookupEnv portn
    putStrLn $ "Values: "++ show ipM ++" and "++show portM
    case (ipM, portM) of
      (Just ip, Just port) -> return (i, (ip, read port::Int))
      _ -> error "Cannot retrieve env vars"
-- europe-west1-b

