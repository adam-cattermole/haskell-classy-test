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
