{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elementary.Logger.Internal
    ( LogLevel(..)
    , LogEvent(..)
    , LogChan(..)
    , LoggerT(..)

    , MonadLogger(..)

    , writeLogChan
    , readLogChan
    , defaultLogChanCapacity
    , newLogChan
    , newLogChan'
    , newLogChanIO
    , newLogChanIO'
    , toText
    , loggerThread
    ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Exception.Safe
import Control.Monad (forever, when)
import Control.Monad.Reader
import Data.Coerce
import Data.Ix (Ix)
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.IO

-- >>> minBound :: LogLevel
-- TRACE
-- >>> maxBound :: LogLevel
-- ERROR
data LogLevel
    = TRACE
    | DEBUG
    | INFO
    | WARN
    | ERROR
    deriving (Show, Eq, Ord, Ix, Bounded)

data LogEvent = LogEvent !LogLevel !UTCTime !TL.LazyText
    deriving (Show)

newtype LogChan = LogChan (TBChan LogEvent)

newtype LoggerT m a = LoggerT { unLoggerT :: ReaderT LogChan m a }
    deriving newtype ( Functor, Applicative, Monad, MonadTrans
                     , MonadIO
                     , MonadFail
                     , MonadReader LogChan
                     )

class (Monad m) => MonadLogger m where
    askLogChan :: m LogChan

instance (Monad m) => MonadLogger (LoggerT m) where
    {-# INLINABLE askLogChan #-}
    askLogChan = ask

writeLogChan :: LogChan -> LogEvent -> STM ()
{-# INLINABLE writeLogChan #-}
writeLogChan = writeTBChan . coerce

readLogChan :: LogChan -> STM LogEvent
{-# INLINABLE readLogChan #-}
readLogChan = readTBChan . coerce

defaultLogChanCapacity :: Int
{-# INLINABLE defaultLogChanCapacity #-}
defaultLogChanCapacity = 256

newLogChan :: Int -> STM LogChan
{-# INLINABLE newLogChan #-}
newLogChan cap = LogChan <$> newTBChan cap

newLogChan' :: STM LogChan
{-# INLINABLE newLogChan' #-}
newLogChan' = newLogChan defaultLogChanCapacity

newLogChanIO :: Int -> IO LogChan
{-# INLINABLE newLogChanIO #-}
newLogChanIO cap = LogChan <$> newTBChanIO cap

newLogChanIO' :: IO LogChan
{-# INLINABLE newLogChanIO' #-}
newLogChanIO' = newLogChanIO defaultLogChanCapacity

toText :: LogEvent -> TL.LazyText
{-# INLINEABLE toText #-}
toText (LogEvent level time content) = TL.concat ["[", TL.show level, "] [", TL.pack $ iso8601Show time, "] ", content]

loggerThread :: LogLevel -> LogChan -> (TL.LazyText -> IO ()) -> IO a
loggerThread lower logChan fn = forever $
    handle (\(e :: IOException) -> hPrint stderr e) $ do
        event@(LogEvent level _ _) <- atomically $ readLogChan logChan
        when (level >= lower) $
            fn $ toText event
