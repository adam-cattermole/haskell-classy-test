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
        stream <- ingressFn (c ^. ingress)
        let result = map fn stream
        mapM_ print result


-- Fake stream op function
strFn :: Int -> (Int, Int)
strFn x = (x, x*2)