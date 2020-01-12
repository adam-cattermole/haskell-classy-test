{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Types where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens.Combinators (makeClassy, makeClassyPrisms)

data ConfOne = ConfOne
    { _initZero :: Int
    , _delayFive :: Int
    }
makeClassy ''ConfOne

data ConfTwo = ConfTwo
    { _initOne :: Int
    , _delayOne :: Int
    }
makeClassy ''ConfTwo

data StreamConfig = StreamConfOne ConfOne
                  | StreamConfTwo ConfTwo
makeClassyPrisms ''StreamConfig

data AppError = AppError

data AppConfig = AppConfig
    { _ingress :: StreamConfig
    }
makeClassy ''AppConfig

newtype App a =
    App {
        unApp :: ReaderT AppConfig (ExceptT AppError IO) a
    } deriving (
        Functor,
        Applicative,
        Monad,
        MonadReader AppConfig,
        MonadError AppError,
        MonadIO
    )