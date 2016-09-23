{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Aeson
import GHC.Generics
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import qualified Data.Text as Text

data Article = Article { id :: Int, header :: String, sex :: String } deriving Generic

instance ToJSON Article
instance FromJSON Article

type API  = "articles" :> Get '[JSON] [Article]
  :<|> "articles" :> Capture "id" Int :> Get '[JSON] Article

api :: Proxy API
api = Proxy

article :: Article
article = Article 1 "first article" "male"
articles :: [Article]
articles = [ article ]

server :: Server API
server = return articles
 :<|> a
   where
     a :: Int -> Handler Article
     a x = return article

main :: IO ()
main = do
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve api server
