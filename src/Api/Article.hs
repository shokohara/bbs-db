{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Api.Article where
import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql
import           Network.Wai                 (Application)
import           Servant
import           Config                      (App (..), Config (..))
import           Models

type ArticleAPI  = "articles" :> Get '[JSON] [Entity Article]
  :<|> "articles" :> Capture "id" Int64 :> Get '[JSON] (Entity Article)
  :<|> "articles" :> ReqBody '[JSON] Article :> Post '[JSON] (Entity Article)
  :<|> "articles" :> Capture "id" Int64 :> Delete '[JSON] NoContent

articleServer :: ServerT ArticleAPI App
articleServer = listArticle :<|> findArticle :<|> createArticle :<|> deleteArticle

listArticle :: App [Entity Article]
listArticle = runDb $ selectList [] []

findArticle :: Int64 -> App (Entity Article)
findArticle v = do
  maybeArticle <- runDb $ selectFirst [ArticleId ==. toSqlKey v] []
  case maybeArticle of
    Nothing -> throwError err404
    Just x -> return x

createArticle :: Article -> App (Entity Article)
createArticle v = do
  articleId <- runDb . insert $ Article (articleSkypeId v) (articleName v) (articleTitle v) (articleTags v) (articleBody v) (articleSex v) (articleCreatedAt v)
  maybeArticle <- runDb $ selectFirst [ArticleId ==. articleId] []
  case maybeArticle of
    Nothing -> throwError err404
    Just x -> return x

deleteArticle :: Int64 -> App NoContent
deleteArticle v = do
  runDb $ deleteWhere [ArticleId ==. toSqlKey v]
  return NoContent

