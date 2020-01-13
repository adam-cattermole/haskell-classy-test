{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Types where

import Control.Monad.Reader
import Control.Lens.Combinators (makeClassy)
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Concurrent (threadDelay)

data ConfOne = ConfOne
    { _initZero :: Int
    , _delayFive :: Int
    }

data ConfTwo = ConfTwo
    { _initOne :: Int
    , _delayOne :: Int
    }

data StreamConfig = StreamConfOne ConfOne
                  | StreamConfTwo ConfTwo

data AppConfig = AppConfig
    { _ingress    :: StreamConfig
    , _globalconf :: Int
    }
makeClassy ''AppConfig

newtype App a =
    App {
        unApp :: ReaderT AppConfig IO a
    } deriving (
        Functor,
        Applicative,
        Monad,
        MonadReader AppConfig,
        MonadIO
    )

class Controller a where
    ingressFn :: a -> IO [Int]
    -- egressFn :: a -> IO ()

instance Controller StreamConfig where
    ingressFn (StreamConfOne c) = buildS  c
    ingressFn (StreamConfTwo c) = buildS' c

buildS :: ConfOne -> IO [Int]
buildS co = go (_delayFive co)  (_initZero co)
    where
        go delay i = unsafeInterleaveIO $ do
            threadDelay delay
            xs <- go delay (i+1)
            return (i:xs)

buildS' :: ConfTwo -> IO [Int]
buildS' ct = go ( _delayOne ct) (_initOne ct)
    where
        go delay i = unsafeInterleaveIO $ do
            threadDelay delay
            xs <- go delay (i+1)
            return (i*2:xs)