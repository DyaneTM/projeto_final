{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Main where

import Import
import Yesod
import Yesod.Static
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius

import Database.Persist.Postgresql

mkYesodDispatch "Pagina" pRoutes

--Form Pessoa

formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
    areq textField "Nome:  " Nothing <*>
    areq intField "Idade:  " Nothing <*>
    areq textField "Profissão" Nothing

--Form Prancha

formPrancha :: Form Prancha
formPrancha = renderDivs $ Prancha <$>
    areq textField "Tipo: " Nothing <*>
    areq intField "Tamanho: " Nothing

-- Form Usuario

formUsuario :: Form Usuario
formUsuario = renderDivs $ Usuario <$>
    areq textField "Usuário" Nothing <*>
    areq textField "Senha" Nothing

-- Generic Form

widgetForm :: Route Pagina -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm i enctype widget j val = do
    msg <- getMessage
    $(whamletFile "form.hamlet")
    toWidget $(luciusFile "prj.lucius")

widgetHtmlNoticias :: Widget
widgetHtmlNoticias = ([whamlet|
<h3><center>Noticias

    <h6><p>O Stand Up World Tour é o circuito mundial de SUP wave - o WCT do stand up, realizado pela Waterman League desde 2010.
           Porém, a categoria feminina, apesar de receber algumas etapas esporádicas, ainda não possuia uma campeã mundial.
           Em 2013, foi inaugurado o circuto mundial de SUP wave feminino, com etapas no Havaí, Brasil, Califórnia e França.
    <h6><p>Nesse sábado (02/11) a paulista Nicole Pacelli acaba de sagrar-se a primeira campeã mundial da história do Stand Up World Tour.
           O título veio após a última etapa, realizada em La Torche, França. Nicole foi até as quartas de final e terminou o evento na 5ª colocação.
           Somou esse resultado ao 1º lugar no Havaí, 2º no Brasil e um 5º na Califórnia, para terminar o ano na primeira posição do ranking.
    <h6><p>“Não consigo descrever, esse é o melhor sentimento do mundo. Era meu sonho! Comecei no stand up por brincadeira e passei a me destacar.
           Esse ano meu foco era o Stand Up World Tour, é como se meu sonho se tornasse realidade. Estou muito feliz.""

    <h2><p><a href=@{HomeR}>Voltar

|]>> toWidget $(luciusFile "prj.lucius"))

widgetHtmlEstilo :: Widget
widgetHtmlEstilo = ([whamlet|
<h3><center>Estilo

    <h6><p>As modalidades mais praticadas do Stand Up Paddle são:Wave, Race, Freestyle e Rafting. Conheça cada uma delas logo abaixo.
    <h6><p> - WAVE: A modalidade Stand Up Paddle Wave tem como objetivo unir as habilidades
       e possibilidades de desempenho do surf clássico e moderno com o uso do remo.
       Desta forma, pretende-se que as potencialidades e características do
       equipamento (prancha e remo) sejam usadas em uma onda.
       Assim, somente surfar a onda sem o auxilio do remo não é o pretendido pelo Stand Up Paddle.
    <h6><p> - RACE: A modalidade race tem como objetivo creditar como vencedor o atleta com o maior
       potencial de rendimento da prancha com o remo, capaz de realizar o percurso estabelecido
       para prova em menor tempo, ultrapassando assim a linha de chegada à frente dos demais.
    <h6><p> - Freestyle: A modalidade Freestyle tem como objetivo avaliar a variedade de manobras
       realizadas sobre a prancha de Stand Up Paddle, apenas com a mobilidade do corpo e o auxílio do remo.
    <h6><p> - Rafting: A modalidade Rafting tem como objetivo descer corredeiras
       sobre a prancha de Stand Up Paddle apenas com a mobilidade do corpo e auxilio do remo.

    <h2><p><a href=@{HomeR}>Voltar

|]>> toWidget $(luciusFile "prj.lucius"))

-- Pagina dos Links

getHomeR :: Handler Html
getHomeR = do
    usr <- lookupSession "_ID"
    defaultLayout $ widgetForm $(whamletFile "home.hamlet") "Santos Sup Girl - Stand Up Paddle"

getNoticiasR :: Handler Html
getNoticiasR = defaultLayout (widgetHtmlNoticias)

getEstiloR :: Handler Html
getEstiloR = defaultLayout (widgetHtmlEstilo)


-- Form Pessoa

widgetFormPessoa :: Route Pagina -> Enctype -> Widget -> Text -> Widget
widgetFormPessoa x enctype widget y = [whamlet|
   <h3><center>Cadastro de #{y}
    <form method="POST" action="@{x}" enctype="#{enctype}">
        ^{widget}
        <input type="submit" value="Cadastrar Pessoa">

    <h2><p><a href=@{HomeR}>Voltar

|]>> toWidget $(luciusFile "prj.lucius")

getCadastroR :: Handler Html
getCadastroR = do
                 (widget, enctype) <- generateFormPost formPessoa
                 defaultLayout $ widgetFormPessoa CadastroR enctype widget "Pessoas"

getPessoaR :: PessoaId -> Handler Html
getPessoaR pid = do
                 pessoa <- runDB $ get404 pid
                 defaultLayout ([whamlet|
                     <h2><center><p>Perfil de #{pessoaNome pessoa}
                     <h2><center><p>Idade:   #{pessoaIdade pessoa}
                     <h2><center><p>Profissao: #{pessoaProfissao pessoa}
                     <h2><center><p><a href=@{HomeR}>Voltar

|]>> toWidget $(luciusFile "prj.lucius"))

getListarR :: Handler Html
getListarR = do
               listaP <- runDB $ selectList [] [Asc PessoaNome]
               defaultLayout ([whamlet|
                   <h3><center>Veja o perfil das pessoas cadastradas
                       $forall Entity pid pessoa <- listaP
                        <p><a href="@{PessoaR pid}">#{pessoaNome pessoa} #{fromSqlKey pid}
                        <h2><center><p><a href=@{HomeR}>Voltar

|]>> toWidget $(luciusFile "prj.lucius"))

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                        runDB $ insert pessoa
                        defaultLayout ([whamlet|
                          <body>
                          <h2><center><p>#{pessoaNome pessoa} Inserido com sucesso
                          <h2><center><p><a href=@{HomeR}>Voltar

                    |]>> toWidget $(luciusFile "prj.lucius"))

                    _ -> redirect CadastroR

-- Form Prancha

widgetFormPrancha :: Route Pagina -> Enctype -> Widget -> Text -> Widget
widgetFormPrancha w enctype widget z = ([whamlet|
   <h3><center>Cadastro de #{z}
    <form method="POST" action="@{w}" enctype="#{enctype}">
        ^{widget}
        <input type="submit" value="Cadastrar Prancha">

    <h2><p><a href=@{HomeR}>Voltar

|]>> toWidget $(luciusFile "prj.lucius"))

getCadastroPranchaR :: Handler Html
getCadastroPranchaR = do
                        (widget, enctype) <- generateFormPost formPrancha
                        defaultLayout $ widgetFormPrancha CadastroPranchaR enctype widget "Prancha"

getPranchaR :: PranchaId -> Handler Html
getPranchaR prid = do
                 prancha <- runDB $ get404 prid
                 defaultLayout ([whamlet|
                     <h2><center><p>Tipo: #{pranchaTipo prancha}
                     <h2><center><p>Tamanho:  #{pranchaTamanho prancha}
                     <h2><center><p><a href=@{HomeR}>Voltar
|]>> toWidget $(luciusFile "prj.lucius"))

getListarPranchaR :: Handler Html
getListarPranchaR = do
              listarPr<- runDB $ selectList [] [Asc PranchaTipo]
              defaultLayout ([whamlet|
              <h3><center>Pranchas cadastradas
                $forall Entity prid prancha <- listarPr
                     <p><a href="@{PranchaR prid}">#{pranchaTipo prancha} #{fromSqlKey prid}

    <h2><center><p><a href=@{HomeR}>Voltar
|]>> toWidget $(luciusFile "prj.lucius"))

postCadastroPranchaR :: Handler Html
postCadastroPranchaR = do
                ((result, _), _) <- runFormPost formPrancha
                case result of
                    FormSuccess prancha -> do
                        runDB $ insert prancha
                        defaultLayout ([whamlet|
                          <body>
                                <h2><center><p>#{pranchaTipo prancha} Inserida com sucesso
                                <h2><center><p><a href=@{HomeR}>Voltar

                    |]>> toWidget $(luciusFile "prj.lucius"))

                    _ -> redirect CadastroPranchaR

-- Pagina Usuario

getCadastroUsuarioR :: Handler Html
getCadastroUsuarioR = do
    (wid,enc)  <- generateFormPost formUsuario
    defaultLayout $ widgetForm (widgetForm UsuarioR enc wid "" "Cadastrar") "Cadastrar Surfista"

getUsuarioR :: UsuarioId -> Handler Html
getUsuarioR uid = do
                 usuario <- runDB $ get404 uid
                 defaultLayout ([whamlet|
                     <h2><center><p>Login: #{usuarioLogin usuario}
                     <h2><center><p>Senha: #{usuarioSenha usuario}
                     <h2><center><p><a href=@{HomeR}>Voltar

|]>> toWidget $(luciusFile "prj.lucius"))

getListarUsuarioR :: Handler Html
getListarUsuarioR = do
              listarUser<- runDB $ selectList [] [Asc UsuarioLogin]
              defaultLayout ([whamlet|
              <h3><center>Usuarios Cadastrados
                $forall Entity uid usuario <- listarUser
                     <p><a href="@{UsuarioR uid}"#{usuarioLogin usuario}>

    <h2><center><p><a href=@{HomeR}>Voltar
|]>> toWidget $(luciusFile "prj.lucius"))


postCadastroUsuarioR :: Handler Html
postCadastroUsuarioR = do
                ((result, _),_) <- runFormPost formUsuario
                case result of
                    FormSuccess usuario -> do
                        runDB $ insert usuario
                        defaultLayout ([whamlet|
                            <body>
                                  <h2><center><p>#{usuarioLogin usuario} Usuario inserido com sucesso!
                                  <h2><center><p><a href=@{HomeR}>Voltar
                    |]>> toWidget $(luciusFile "prj.lucius"))

                    _ -> redirect CadastroUsuarioR

-- Login Usuario

getLoginR :: Handler Html
getLoginR = do
    (wid,enc) <- generateFormPost formUsuario
    defaultLayout $ widgetForm( widgetForm UsuarioR enc wid "" "Login") "Login Usuario"

postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formUsuario
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [UsuarioLogin ==. usuarioLogin usr, UsuarioSenha ==. usuarioSenha usr] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_ID" (usuarioLogin usr)
                    redirect HomeR
                Nothing -> do
                    setMessage $ [shamlet| Usuario Invalido |]
                    redirect LoginR
        _ -> redirect LoginR

-- Pagina Admin

getAdminR :: Handler Html
getAdminR = defaultLayout $ widgetForm [whamlet|
                                <h2><center><p>Está Página é do Admin |] "Página somente do Admin"







connStr = "dbname=dcigsi9mv7g8dh host=ec2-107-21-219-201.compute-1.amazonaws.com user=qmevzvuburkfja password=WodTbIoLcjeAtfGSums5NrWj5k port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Pagina pool s)

