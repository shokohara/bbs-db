module Main where

import Lib
import Models
import Api
import Config
import Control.Monad.Except
import Network.Wai.Handler.Warp (run)
import Safe
import System.Environment (lookupEnv)
import Database.Persist.Postgresql

main :: IO ()
main = do
  env <- lookupSetting "ENV" Development
  port <- lookupSetting "PORT" 8080
  pool <- makePool env
  let cfg = Config { getPool = pool, getEnv = env }
      logger = setLogger env
  runSqlPool doMigrations pool
  putStrLn $ "Listening on port " ++ show port
  run port $ logger $ app cfg

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing -> return def
    Just str -> maybe (handleFailedRead str) return $ readMay str
  where
    handleFailedRead str = error $ mconcat ["Failed to read [[", str, "]] for environment variable ", env]

