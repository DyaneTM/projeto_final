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

widgetForm :: Route Pagina -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm i enctype widget j val = do
    msg <- getMessage
    $(whamletFile "form.hamlet")
    toWidget $(luciusFile "prj.lucius")

formUsuario :: Form Usuario
formUsuario = renderDivs $ Usuario <$>
    areq textField "Login" Nothing <*>
    areq textField "Senha" Nothing

formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
    areq textField "Nome:  " Nothing <*>
    areq intField "Idade:  " Nothing <*>
    areq doubleField "Salario:  " Nothing

formPrancha :: Form Prancha
formPrancha = renderDivs $ Prancha <$>
    areq textField "Tipo: " Nothing <*>
    areq intField "Tamanho: " Nothing

widgetHtmlHome :: Widget
widgetHtmlHome = ([whamlet|
    <h1><center>Santos Sup Girl - Stand Up Paddle
    <ul>
        <li><a href=@{NoticiasR}>Noticias
        <li><a href=@{EstiloR}>Estilo
        <li><a href=@{CadastroR}>Cadastro
        <li><a href=@{ListarR}>Surfistas
        <li><a href=@{CadastroPranchaR}>Cadastro de Pranchas
        <li><a href=@{ListarPranchaR}> Lista de Pranchas
        <li><a href=@{CadastroUsuarioR}>Cadastro de Usuario
        <li><a href=@{ListarUsuarioR}> Lista de Usuarios

|]>> toWidget $(luciusFile "prj.lucius"))

widgetHtmlNoticias :: Widget
widgetHtmlNoticias = ([whamlet|
<h3><center>Noticias
    <h6><p>01 de Junho de 2013 - O local não poderia ser melhor. O santista Leco Salazar, atual campeão mundial profissional de Stand Up Paddle (SUP) Wave, a busca pelo bicampeonato nas poderosas ondas de Sunset Beach, no Havaí.
    <h6><p>A primeira etapa do Tour será disputada de 6 a 15 de fevereiro, com período de espera pela melhor ondulação Leco viaja no próximo sábado (2), dia em que comemora 25 anos de idade.
    <h6><p>“O SUP já está no meu DNA e vou surfar como faço todos os dias, me concentrando e com o meu pai como técnico, que é uma grande ajuda”, o filho mais velho de Picuruta Salazar, um dos maiores ídolos do surf brasileiro, nada menos que dez vezes campeão nacional de longboard.
<h2><p><a href=@{HomeR}>Voltar|] >> toWidget $(luciusFile "prj.lucius"))

widgetHtmlEstilo :: Widget
widgetHtmlEstilo = ([whamlet|
<h3><center>Estilo
    <h6><p>As modalidades mais praticadas do Stand Up Paddle são:Wave, Race, Freestyle e Rafting. Conheça cada uma delas logo abaixo.
    <h6><p> - WAVE: A modalidade Stand Up Paddle Wave tem como objetivo unir as habilidades possibilidades de desempenho do surf clássico e moderno com o uso do remo. forma, pretende-se que as potencialidades e características do (prancha e remo) sejam usadas em uma onda., somente surfar a onda sem o auxilio do remo não é o pretendido pelo Stand Up Paddle.
    <h6><p> - RACE: A modalidade race tem como objetivo creditar como vencedor o atleta com o maior de rendimento da prancha com o remo, capaz de realizar o percurso estabelecido prova em menor tempo, ultrapassando assim a linha de chegada à frente dos demais.
    <h6><p> - Freestyle: A modalidade Freestyle tem como objetivo avaliar a variedade de manobras sobre a prancha de Stand Up Paddle, apenas com a mobilidade do corpo e o auxílio do remo.
    <h6><p> - Rafting: A modalidade Rafting tem como objetivo descer corredeiras a prancha de Stand Up Paddle apenas com a mobilidade do corpo e auxilio do remo.
<h2><p><a href=@{HomeR}>Voltar|] >> toWidget $(luciusFile "prj.lucius"))

widgetFormPrancha :: Route Pagina -> Enctype -> Widget -> Text -> Widget
widgetFormPrancha w enctype widget z = ([whamlet|
<h3><center>Cadastro de #{z}
    <form method="POST" action="@{w}" enctype="#{enctype}">
        ^{widget}
        <input type="submit" value="Cadastrar Prancha">
<h2><p><a href=@{HomeR}>Voltar|] >> toWidget $(luciusFile "prj.lucius"))

widgetFormPessoa :: Route Pagina -> Enctype -> Widget -> Text -> Widget
widgetFormPessoa x enctype widget y = ([whamlet|
<h3><center>Cadastro de #{y}
    <form method="POST" action="@{x}" enctype="#{enctype}">
        ^{widget}
        <input type="submit" value="Cadastrar Pessoa">
<h2><p><a href=@{HomeR}>Voltar|]>> toWidget $(luciusFile "prj.lucius"))

widgetFormUsuario :: Route Pagina -> Enctype -> Widget -> Text -> Widget
widgetFormUsuario x enctype widget y = ([whamlet|
<h3><center>Cadastro de #{y}
    <form method="POST" action="@{x}" enctype="#{enctype}">
        ^{widget}
        <input type="submit" value="Cadastrar Usuario">
<h2><p><a href=@{HomeR}>Voltar |]>> toWidget $(luciusFile "prj.lucius"))

getHomeR :: Handler Html
getHomeR = defaultLayout (widgetHtmlHome)

getNoticiasR :: Handler Html
getNoticiasR = defaultLayout (widgetHtmlNoticias)

getEstiloR :: Handler Html
getEstiloR = defaultLayout (widgetHtmlEstilo)

getCadastroR :: Handler Html
getCadastroR = do
                (widget, enctype) <- generateFormPost formPessoa
                defaultLayout $ widgetFormPessoa CadastroR enctype widget "Pessoas"

getCadastroPranchaR :: Handler Html
getCadastroPranchaR = do
                    (widget, enctype) <- generateFormPost formPrancha
                    defaultLayout $ widgetFormPrancha CadastroPranchaR enctype widget "Prancha"

getCadastroUsuarioR :: Handler Html
getCadastroUsuarioR = do
                    (widget, enctype) <- generateFormPost formUsuario
                    defaultLayout $ widgetFormUsuario CadastroUsuarioR enctype widget "Usuario"

getPessoaR :: PessoaId -> Handler Html
getPessoaR pid = do
                pessoa <- runDB $ get404 pid
                defaultLayout ([whamlet|
<h2><center><p>Perfil de #{pessoaNome pessoa}
<h2><center><p>Idade:   #{pessoaIdade pessoa}
<h2><center><p>Salario: #{pessoaSalario pessoa}
<h2><center><p><a href=@{HomeR}>Voltar|]>> toWidget $(luciusFile "prj.lucius"))

getPranchaR :: PranchaId -> Handler Html
getPranchaR pid = do
                  prancha <- runDB $ get404 pid
                  defaultLayout ([whamlet|
<h2><center><p>Tipo: #{pranchaTipo prancha}
<h2><center><p>Tamanho:  #{pranchaTamanho prancha}
<h2><center><p><a href=@{HomeR}>Voltar |]>> toWidget $(luciusFile "prj.lucius"))

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsuario
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Usuario inserido com sucesso! |]
            redirect UsuarioR
        _ -> redirect UsuarioR

getLoginR :: Handler Html
getLoginR = do
    (wid,enc) <- generateFormPost formUsuario
    defaultLayout $ widgetForm LoginR enc wid "" "Login"

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
                    setMessage $ [shamlet| Invalid user |]
                    redirect LoginR
        _ -> redirect LoginR

getListarR :: Handler Html
getListarR = do
             listaP <- runDB $ selectList [] [Asc PessoaNome]
             defaultLayout ([whamlet|
<h1><center>Pessoas cadastradas:
    $forall Entity pid pessoa <- listaP
        <center><a href=@{PessoaR pid}> #{pessoaNome pessoa} <br>
<h2><center><p><a href=@{HomeR}>Voltar|]>> toWidget $(luciusFile "prj.lucius"))

getListarPranchaR :: Handler Html
getListarPranchaR = do
                      listarPr<- runDB $ selectList [] [Asc PranchaTipo]
                      defaultLayout ([whamlet|
<h3><center>Pranchas cadastradas
    $forall Entity pid prancha <- listarPr
        <center><p><a href="@{PranchaR pid}">#{pranchaTipo prancha} <br>
<h2><center><p><a href=@{HomeR}>Voltar |]>> toWidget $(luciusFile "prj.lucius"))


getListarUsuarioR :: Handler Html
getListarUsuarioR = do
    listarU <- runDB $ selectList [] [Asc UsuarioLogin]
    defaultLayout $(whamletFile "lista.hamlet")

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                        runDB $ insert pessoa
                        defaultLayout ([whamlet|
<body>
<h2><center><p>#{pessoaNome pessoa} Inserido com Sucesso
<h2><center><p><a href=@{HomeR}>Voltar |]>> toWidget $(luciusFile "prj.lucius"))
                    _ -> redirect CadastroR

postCadastroPranchaR :: Handler Html
postCadastroPranchaR = do
                    ((result, _), _) <- runFormPost formPrancha
                    case result of
                        FormSuccess prancha -> do
                            runDB $ insert prancha
                            defaultLayout ([whamlet|
<body>
<h2><center><p>#{pranchaTipo prancha} Inserida com Sucesso
<h2><center><p><a href=@{HomeR}>Voltar |]>> toWidget $(luciusFile "prj.lucius"))
                        _ -> redirect CadastroPranchaR

postCadastroUsuarioR :: Handler Html
postCadastroUsuarioR = do
                    ((result, _), _) <- runFormPost formUsuario
                    case result of
                        FormSuccess usuario -> do
                            runDB $ insert usuario
                            defaultLayout ([whamlet|
<body>
<h2><center><p>#{usuarioLogin usuario} Inserido com Sucesso
<h2><center><p><a href=@{HomeR}>Voltar |]>> toWidget $(luciusFile "prj.lucius"))
                        _ -> redirect CadastroUsuarioR

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet| <h1> Seja Bem-Vindo |]

connStr = "dbname=dcigsi9mv7g8dh host=ec2-107-21-219-201.compute-1.amazonaws.com user=qmevzvuburkfja password=WodTbIoLcjeAtfGSums5NrWj5k port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Pagina pool s)