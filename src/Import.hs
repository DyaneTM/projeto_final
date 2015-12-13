{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static

pRoutes = [parseRoutes|
  / HomeR GET
  /static StaticR Static getStatic
  /noticias NoticiasR GET
  /estilo   EstiloR GET
  /cadastro CadastroR GET POST
  /listar   ListarR GET
  /pessoa/#PessoaId PessoaR GET
  /cadastroPrancha CadastroPranchaR GET POST
  /listarPrancha ListarPranchaR GET
  /prancha/#PranchaId PranchaR GET
  /cadastroUsuario CadastroUsuarioR GET POST
  /listarUsuario ListarUsuarioR GET
  /usuario/#UsuarioId UsuarioR GET
  /login LoginR GET POST
  /admin AdminR GET
|]

--usuario/UsuarioR GET