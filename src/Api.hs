{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import Config                      (App (..), Config (..))
import Models
import Api.Article
import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import Data.Aeson
import GHC.Generics
import Servant
import           Data.Int                    (Int64)

userApp :: Config -> Application
userApp cfg = serve (Proxy :: Proxy ArticleAPI) $ appToServer cfg

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat $ flip runReaderT cfg . runApp

appToServer :: Config -> Server ArticleAPI
appToServer cfg = enter (convertApp cfg) articleServer

appApi :: Proxy ArticleAPI
appApi = Proxy

app :: Config -> Application
app cfg = serve appApi (appToServer cfg)

