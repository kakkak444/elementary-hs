{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Elementary.Logger
    ( LogLevel(..)
    , LogEvent(..)
    , LoggerT

    , MonadLogger

    , toText
    , hoistLogger
    , runCustomLoggerT
    , runHandleLoggerT
    , runStdoutLoggerT
    , runStderrLoggerT
    , runFileLoggerT
    , logWithLevel
    , logWithLevelS
    , logWithLevelN
    , logWithLevelNS
    , logTrace  , logDebug  , logInfo  , logWarn  , logError
    , logTraceS , logDebugS , logInfoS , logWarnS , logErrorS
    , logTraceN , logDebugN , logInfoN , logWarnN , logErrorN
    , logTraceNS, logDebugNS, logInfoNS, logWarnNS, logErrorNS
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Coerce
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time.Clock
import System.IO

import Elementary.Logger.Internal

hoistLogger :: (forall x. m x -> n x) -> LoggerT m a -> LoggerT n a
hoistLogger f (LoggerT m) = coerce $ \e -> f $ runReaderT m e

runCustomLoggerT :: (MonadIO m, MonadMask m) => (TL.LazyText -> IO ()) -> LogLevel -> LoggerT m a -> m a
runCustomLoggerT logFn lower (LoggerT m) = do
    chan <- liftIO newLogChanIO'
    bracket (liftIO $ forkIO $ loggerThread lower chan logFn) (liftIO . killThread) $ \_ ->
        runReaderT m chan

runHandleLoggerT :: (MonadIO m, MonadMask m) => Handle -> LogLevel -> LoggerT m a -> m a
runHandleLoggerT hdl lower logger =
    bracket (liftIO $ hGetBuffering hdl) (liftIO . hSetBuffering hdl) $ \_ -> do
        liftIO $ hSetBuffering hdl LineBuffering
        runCustomLoggerT logFn lower logger
  where
    logFn = TL.hPutStrLn hdl

runStdoutLoggerT, runStderrLoggerT :: (MonadIO m, MonadMask m) => LogLevel -> LoggerT m a -> m a
runStdoutLoggerT = runHandleLoggerT stdout
runStderrLoggerT = runHandleLoggerT stderr

runFileLoggerT :: (MonadIO m, MonadMask m) => FilePath -> LogLevel -> LoggerT m a -> m a
runFileLoggerT path lower logger =
    bracket (liftIO $ openFile path WriteMode) (liftIO . hClose) $ \hdl -> runHandleLoggerT hdl lower logger

writeLogChan' :: (MonadLogger m, MonadIO m) => LogEvent -> m ()
writeLogChan' event = do
    logChan <- askLogChan
    _ <- liftIO $ forkIO $ atomically $ writeLogChan logChan event
    return ()

logWithLevel :: (MonadLogger m, MonadIO m) => LogLevel -> TL.LazyText -> m ()
logWithLevel level content = do
    currTime <- liftIO getCurrentTime
    writeLogChan' $ LogEvent level currTime content

logWithLevelS :: (MonadLogger m, MonadIO m, Show a) => LogLevel -> a -> m ()
logWithLevelS level content = do
    currTime <- liftIO getCurrentTime
    writeLogChan' $ LogEvent level currTime $ TL.show content

-- | log with name
logWithLevelN :: (MonadLogger m, MonadIO m) => LogLevel -> TL.LazyText -> TL.LazyText -> m ()
logWithLevelN level name content = logWithLevel level $ TL.concat [name, ": ", content]

logWithLevelNS :: (MonadLogger m, MonadIO m, Show a) => LogLevel -> TL.LazyText -> a -> m ()
logWithLevelNS level name content = logWithLevel level $ TL.concat [name, ": ", TL.show content]

logTrace, logDebug, logInfo, logWarn, logError :: (MonadLogger m, MonadIO m) => TL.LazyText -> m ()
logTrace = logWithLevel TRACE
logDebug = logWithLevel DEBUG
logInfo  = logWithLevel INFO
logWarn  = logWithLevel WARN
logError = logWithLevel ERROR

logTraceS, logDebugS, logInfoS, logWarnS, logErrorS :: (MonadLogger m, MonadIO m, Show a) => a -> m ()
logTraceS = logWithLevelS TRACE
logDebugS = logWithLevelS DEBUG
logInfoS  = logWithLevelS INFO
logWarnS  = logWithLevelS WARN
logErrorS = logWithLevelS ERROR

logTraceN, logDebugN, logInfoN, logWarnN, logErrorN :: (MonadLogger m, MonadIO m) => TL.LazyText -> TL.LazyText -> m ()
logTraceN = logWithLevelN TRACE
logDebugN = logWithLevelN DEBUG
logInfoN  = logWithLevelN INFO
logWarnN  = logWithLevelN WARN
logErrorN = logWithLevelN ERROR

logTraceNS, logDebugNS, logInfoNS, logWarnNS, logErrorNS :: (MonadLogger m, MonadIO m, Show a) => TL.LazyText -> a -> m ()
logTraceNS = logWithLevelNS TRACE
logDebugNS = logWithLevelNS DEBUG
logInfoNS  = logWithLevelNS INFO
logWarnNS  = logWithLevelNS WARN
logErrorNS = logWithLevelNS ERROR
