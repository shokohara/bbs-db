{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Api.Article where
import Config (App (..), Config (..))
import Control.Monad.Except
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Database.Persist.Postgresql
import Models
import Network.Wai (Application)
import Servant

type ArticleAPI  = "articles" :> Get '[JSON] [Entity Article]
  :<|> "articles" :> Capture "id" Int64 :> Get '[JSON] (Entity Article)
  :<|> "articles" :> ReqBody '[JSON] [Article] :> Post '[JSON] [Entity Article]
  :<|> "articles" :> ReqBody '[JSON] Article :> Post '[JSON] (Entity Article)
  :<|> "articles" :> Capture "id" Int64 :> Delete '[JSON] NoContent

articleServer :: ServerT ArticleAPI App
articleServer = listArticle :<|> findArticle :<|> createArticles :<|> createArticle :<|> deleteArticle

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
  article <- runDb $ selectFirst [ArticleSkypeId ==. articleSkypeId v, ArticleName ==. articleName v, ArticleTags ==. articleTags v, ArticleBody ==. articleBody v, ArticleSex ==. articleSex v, ArticleCreatedAt ==. articleCreatedAt v] []
  case article of
    Nothing -> do
      articleId <- runDb . insert $ v
      maybeArticle <- runDb $ selectFirst [ArticleId ==. articleId] []
      case maybeArticle of
        Nothing -> throwError err400
        Just x -> return x
    Just x -> throwError err400

createArticles :: [Article] -> App [Entity Article]
createArticles as = do
  x <- forM as $ \v -> do
    article <- runDb $ selectFirst [ArticleSkypeId ==. articleSkypeId v, ArticleName ==. articleName v, ArticleTags ==. articleTags v, ArticleBody ==. articleBody v, ArticleSex ==. articleSex v, ArticleCreatedAt ==. articleCreatedAt v] []
    case article of
      Nothing -> do
        articleId <- runDb . insert $ v
        runDb $ selectFirst [ArticleId ==. articleId] []
      Just x -> return Nothing
  return $ catMaybes x

deleteArticle :: Int64 -> App NoContent
deleteArticle v = do
  runDb $ deleteWhere [ArticleId ==. toSqlKey v]
  return NoContent

