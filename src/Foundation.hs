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
    salario Double
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
    authRoute _ = Just $LoginR
    isAuthorized LoginR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser

isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Página do Adm"

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

-- Form de alguma coisa
type Form a = Html -> MForm  Handler (FormResult a, Widget)

{- Internilizacao - Framework Skeleto -}
instance RenderMessage Pagina FormMessage where
  renderMessage _ _ = defaultFormMessage

