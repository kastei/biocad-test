{-# LANGUAGE OverloadedStrings #-}

module SimpleServer where
{-
    ( runServer
    ) where

import           Control.Monad.Trans.Reader           (runReaderT)
import           Data.Text.Lazy                       (Text)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Scotty.Trans                     (ScottyT, defaultHandler,
                                                       get, middleware, scottyT)

import           Database.Bolt                        (BoltCfg)

import           Data

type Port = Int

-- |Run server with connection pool as a state
runServer :: Port -> BoltCfg -> IO ()
runServer port config = do state <- constructState config
                            scottyT port (`runReaderT` state) $ do
                                middleware logStdoutDev
-}