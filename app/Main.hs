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
    let confone = AppConfig $ StreamConfOne $ ConfOne 1 5000000
        conftwo = defaultConfig
    -- User needs to be able to call function like this
    simpleNode conftwo strFn


defaultConfig :: AppConfig
defaultConfig = AppConfig $ StreamConfTwo $ ConfTwo 0 100000


simpleNode :: AppConfig -> (Int -> (Int, Int)) -> IO ()
simpleNode config fn = do
    let buildFn = case config ^. ingress of
                    StreamConfOne _ -> runExceptT (runReaderT (unApp buildStream) config)
                    StreamConfTwo _ -> runExceptT (runReaderT (unApp buildStream') config)
    x <- buildFn
    case x of
        Left  e  -> return ()
        Right r -> do
            let result = map fn r
            mapM_ (\x -> liftIO $ print x) result


-- Fake stream op function
strFn :: Int -> (Int, Int)
strFn x = (x, x*2)


-- Function that builds dependent on ConfOne
buildStream :: (MonadReader r m,
                HasConfOne r,
                MonadIO m) => m [Int]
buildStream = do
    c <- ask
    liftIO $ go (c ^. delayFive) (c ^. initZero)
    where
        go delay i = unsafeInterleaveIO $ do
            threadDelay delay
            xs <- go delay (i+1)
            return (i:xs)

-- Function that builds dependent on ConfTwo
buildStream' :: (MonadReader r m,
                HasConfTwo r,
                MonadIO m) => m [Int]
buildStream' = do
    c <- ask
    liftIO $ go (c ^. delayOne) (c ^. initOne)
    where
        go delay i = unsafeInterleaveIO $ do
            threadDelay delay
            xs <- go delay (i+1)
            return (i*2:xs)


-- Non-Classy method but works
simpleNode' :: AppConfig -> (Int -> (Int, Int)) -> IO ()
simpleNode' conf fn = do
    x <- case conf ^. ingress of
            StreamConfOne co -> buildStreamNC  (_delayFive co)  (_initZero co)
            StreamConfTwo ct -> buildStreamNC' ( _delayOne ct) (_initOne ct)
    let result = map fn x
    mapM_ print result


buildStreamNC :: Int -> Int -> IO [Int]
buildStreamNC = go
    where
        go delay i = unsafeInterleaveIO $ do
            threadDelay delay
            xs <- go delay (i+1)
            return (i:xs)


buildStreamNC' :: Int -> Int -> IO [Int]
buildStreamNC' = go
    where
        go delay i = unsafeInterleaveIO $ do
            threadDelay delay
            xs <- go delay (i+1)
            return (i*2:xs)