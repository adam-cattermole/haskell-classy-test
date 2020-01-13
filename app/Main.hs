module Main where

import Types
import Control.Concurrent
import System.IO.Unsafe
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Lens

main :: IO ()
main = do
    -- Building some configs, needs to be have no specificity
    let confone = AppConfig (StreamConfOne $ ConfOne 1 5000000) 5
        conftwo = defaultConfig
    -- User needs to be able to call function like this
    simpleNode conftwo strFn


defaultConfig :: AppConfig
defaultConfig = AppConfig (StreamConfTwo $ ConfTwo 0 100000) 10


simpleNode :: AppConfig -> (Int -> (Int, Int)) -> IO ()
simpleNode config fn = runReaderT (unApp $ nodeInternal fn) config 


nodeInternal :: (MonadReader r m,
                 HasAppConfig r,
                 MonadIO m)
             => (Int -> (Int, Int))
             -> m ()
nodeInternal fn = do
    c <- ask
    liftIO $ do
        -- can use higher level config
        print (c ^. globalconf)
        -- extract ingress conf and run
        stream <- build (c ^. ingress)
        let result = map fn stream
        mapM_ print result


-- Fake stream op function
strFn :: Int -> (Int, Int)
strFn x = (x, x*2)

build :: (MonadIO m) => StreamConfig -> m [Int]
build (StreamConfOne c) = buildS  c
build (StreamConfTwo c) = buildS' c

buildS :: (MonadIO m) => ConfOne -> m [Int]
buildS co = liftIO $ go (_delayFive co)  (_initZero co)
    where
        go delay i = unsafeInterleaveIO $ do
            threadDelay delay
            xs <- go delay (i+1)
            return (i:xs)

buildS' :: (MonadIO m) => ConfTwo -> m [Int]
buildS' ct = liftIO $ go ( _delayOne ct) (_initOne ct)
    where
        go delay i = unsafeInterleaveIO $ do
            threadDelay delay
            xs <- go delay (i+1)
            return (i*2:xs)