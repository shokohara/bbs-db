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

instance FromFormUrlEncoded Article where
  fromFormUrlEncoded inputs = Article <$> lkp "id" <*> lkp "header" <*> lkp "sex"
    where
      lkp l = case lookup l inputs of
                Nothing -> Left $ "label " ++ Text.unpack l ++ " not found"
                Just v -> Right $ read (Text.unpack v)

type API  = "articles" :> Get '[JSON] [Article]
  :<|> "articles" :> Capture "id" Int :> Get '[JSON] Article
  :<|> "articles" :> ReqBody '[JSON, FormUrlEncoded] Article :> Post '[JSON] Article

api :: Proxy API
api = Proxy

article :: Article
article = Article 1 "first article" "male"
articles :: [Article]
articles = [ article ]

server :: Server API
server = return articles
 :<|> a
  :<|> createArticle
    where
      a :: Int -> Handler Article
      a x = return article
      createArticle :: Article -> Handler Article
      createArticle v = return v

main :: IO ()
main = do
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve api server
