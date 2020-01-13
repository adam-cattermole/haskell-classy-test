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
    simpleNode confone strFn


defaultConfig :: AppConfig
defaultConfig = AppConfig (StreamConfTwo $ ConfTwo 0 100000) 10


simpleNode :: AppConfig -> (Int -> (Int, Int)) -> IO ()
simpleNode config fn = do
    x <- runExceptT (runReaderT (unApp $ nodeInternal fn) config)
    case x of
        Left  e -> print "yikes error" >> return ()
        Right r -> print "done"        >> return () 


nodeInternal :: (MonadReader r m,
                 HasAppConfig r,
                 MonadIO m)
             => (Int -> (Int, Int))
             -> m ()
nodeInternal fn = do
    c <- ask
    -- can use higher level config
    liftIO $ print (c ^. globalconf)
    -- extract ingress conf and run
    stream <- liftIO $ ingressFn (c ^. ingress)
    let result = map fn stream
    liftIO $ mapM_ print result


-- Fake stream op function
strFn :: Int -> (Int, Int)
strFn x = (x, x*2)