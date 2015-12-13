{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Pagina = Pagina { connPool :: ConnectionPool,
                       getStatic :: Static }

staticFiles "."

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Prancha
    tipo Text
    tamanho Int
    deriving Show
Pessoa
    nome Text
    idade Int
    profissao Text
    deriving Show
Usuario
    login Text
    senha Text sqltype=varchar(8)
    deriving Show
|]

mkYesodData "Pagina" pRoutes

-- Conexão com o DB (sempre igual)

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Pagina where

-- Form de alguma coisa
type Form a = Html -> MForm  Handler (FormResult a, Widget)

{- Internilizacao - Framework Skeleto -}
instance RenderMessage Pagina FormMessage where
  renderMessage _ _ = defaultFormMessage

