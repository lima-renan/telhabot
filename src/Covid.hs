{-
Module: Lib
Description : Main component of your project.
Copyright   : (c) Renan Ferreira Lima, 2021
License     : GPL-3
Maintainer  : renan.lima@aluno.ufabc.edu.br
Stability   : experimental
Portability : POSIX
Adicionar bot ao servidor: https://discord.com/api/oauth2/authorize?client_id=875036424724947045&permissions=259846044736&scope=bot
Framework: discord-haskell https://hackage.haskell.org/package/discord-haskell
-}


{-# LANGUAGE OverloadedStrings, DeriveGeneric #-} -- definir "strings" sejam Data.Text
{-# LANGUAGE RecordWildCards   #-}  


module Covid where

import DataBaseFunctions
import Schema

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Conduit 
import Network.HTTP.Simple  
import Prelude.Compat.Repl ( Show, Int, Maybe, IO, FilePath )
import System.IO
import UnliftIO (liftIO)
import UnliftIO.Concurrent




         
       

-- Referências:
-- countriesName.json: https://gist.github.com/jonasruth/61bde1fcf0893bd35eea
-- Artigo sobre leitura de JSON: https://artyom.me/aeson
-- API de dados da COVID-19: https://covid19-brazil-api-docs.vercel.app/
-- Artigo sobre consumo de API: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json


--JSON dos estados - COVID - Amostra

{- 
{
  "data":[
    {
      "uid":35,
      "uf":"SP",
      "state":"São Paulo",
      "cases":4164587,
      "deaths":142528,
      "suspects":5334,
      "refuses":596,
      "datetime":"2021-08-14T22:34:41.818Z"
    },
    {
      "uid":31,
      "uf":"MG",
      "state":"Minas Gerais",
      "cases":2019435,
      "deaths":51849,
      "suspects":925,
      "refuses":104,
      "datetime":"2021-08-14T22:34:41.818Z"
    },
    {
      "uid":41,
      "uf":"PR",
      "state":"Paraná",
      "cases":1414605,
      "deaths":36432,
      "suspects":400,
      "refuses":119,
      "datetime":"2021-08-14T22:34:41.818Z"
    },
  ]
}
 -}


-- para um único estado
instance FromJSON State where
  parseJSON (Object v) = State
                         <$> (v .: "uid")
                         <*> (v .: "uf")
                         <*> (v .: "state")
                         <*> (v .: "cases")
                         <*> (v .: "deaths")
                         <*> (v .: "suspects")
                         <*> (v .: "refuses")
                         <*> (v .: "datetime")
  parseJSON _ = mzero

newtype States = States { 
  states :: [State] 
} deriving Show

toState :: States -> [State]
toState (States st) = st
                     
instance FromJSON States where
    parseJSON (Object v) = do
        ms <- v .: "data"
        States <$> parseJSON ms
    parseJSON _ = mzero


-- JSON dos paises - COVID - Amostra

{- 
{
  "data": [
    {
      "country": "Canada",
      "cases": 1299,
      "confirmed": 1328,
      "deaths": 19,
      "recovered": 10,
      "updated_at": "2020-03-21T23:43:02.000Z"
    },
    {
      "country": "Maldives",
      "cases": 13,
      "confirmed": 13,
      "deaths": 0,
      "recovered": 0,
      "updated_at": "2020-03-15T18:20:18.000Z"
    },
    {
      "country": "Lithuania",
      "cases": 97,
      "confirmed": 99,
      "deaths": 1,
      "recovered": 1,
      "updated_at": "2020-03-22T02:13:18.000Z"
    }
  ]
}
 -}


-- para um único país
instance FromJSON Country where
  parseJSON (Object v) = Country
                         <$> (v .: "country")
                         <*> (v .: "cases")
                         <*> (v .: "confirmed")
                         <*> (v .: "deaths")
                         <*> (v .: "recovered")
                         <*> (v .: "updated_at")
  parseJSON _ = mzero

-- para vários países
newtype Countries = Countries { 
  countries :: [Country] 
} deriving Show

toCountry :: Countries -> [Country]
toCountry (Countries ct) = ct
                     
instance FromJSON Countries where
  parseJSON (Object v) = do
      ms <- v .: "data"
      Countries <$> parseJSON ms
  parseJSON _ = mzero

{- instance ToJSON Country where
 toJSON (Country country cases confirmed deaths recovered updated_at) =
    object [ "country"  .= country
           , "cases"   .= cases
           , "confirmed"       .= confirmed
           , "deaths" .= deaths
           , "recovered" .= recovered 
           , "updated_at" .= updated_at
             ]  -}

-- JSON nome de países - amostra
{- [ { "gentilico" : "afegãne",
    "nome_pais" : "Afeganistão",
    "nome_pais_int" : "Afghanistan",
    "sigla" : "AF"
  },
  { "gentilico" : "sul-africana",
    "nome_pais" : "África do Sul",
    "nome_pais_int" : "South Africa",
    "sigla" : "ZA"
  },
  { "gentilico" : "albanesa",
    "nome_pais" : "Albânia",
    "nome_pais_int" : "Albania",
    "sigla" : "AL"
  }
] -}


instance FromJSON CountryName where
  parseJSON (Object v) = CountryName 
           <$> v .: "gentilico"
           <*> v .: "nome_pais"
           <*> v .: "nome_pais_int"
           <*> v .: "sigla"
  parseJSON _ = mzero


jsonFile:: FilePath
jsonFile = "src/countriesNames.json"


getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile


getJSONurl :: String -> IO B.ByteString
getJSONurl = simpleHttp


                    

{- filterCN :: [CountriesN] -> T.Text  -> Maybe T.Text
filterCN xs s | not $ any (\x -> nome_pais x == s || nome_pais_int x == s) xs = Nothing
              | otherwise = Just $ nome_pais_int $ head $ filter (\x -> nome_pais x == s || nome_pais_int x == s) xs

filterCNptBr :: [CountriesN] -> T.Text  -> Maybe T.Text
filterCNptBr xs s | not $ any (\x -> nome_pais x == s || nome_pais_int x == s) xs = Nothing
              | otherwise = Just $ nome_pais $ head $ filter (\x -> nome_pais x == s || nome_pais_int x == s) xs


filterCountry :: Countries -> T.Text -> Maybe Country
filterCountry xs s | not $ any (\x -> country x == Just s) (countries xs) = Nothing
                 | otherwise = Just $ head $ filter (\x -> country x == Just s) (countries xs)

filterState :: States -> T.Text  -> Maybe State 
filterState xs s | not $ any (\x -> state x == Just s) (states xs) = Nothing
               | otherwise = Just $ head $ filter (\x -> state x == Just s) (states xs) 



fromInt :: Maybe Int -> T.Text 
fromInt (Just n) =  T.pack $ show n
fromInt _ = "Sem dados"


covidestmsgSt :: Maybe State -> T.Text
covidestmsgSt Nothing = "Desculpe não tenho esses dados! =("
covidestmsgSt (Just st) = "🤖 Tenho os seguintes dados:\n\
          \Estado: " <> fromMaybe "" (state st) <> "\n" <>
          "Total de casos: " <> fromInt(cases st) <> "\n" <>
          "Total de mortes: " <> fromInt(deaths st) <> "\n" <>
          "Total de casos suspeitos: " <> fromInt(suspects st) <> "\n" <>
          "Atualização em: " <> T.pack (show (fromJust (updatedAt st))) <> "\n"


covidestmsgCt :: Maybe Country -> T.Text -> T.Text
covidestmsgCt Nothing name = "Desculpe não tenho esses dados para " <> name <> "! =("
covidestmsgCt (Just ct) name = "🤖 Tenho os seguintes dados:\n\
          \País: " <> name <> "\n" <>
          "Total de casos: " <> fromInt(casesCountry ct) <> "\n" <>
          "Total de mortes: " <> fromInt(deathsCountry ct) <> "\n" <>
          "Total de recuperados: " <> fromInt(recovered ct) <> "\n" <>
          "Atualização em: " <> T.pack (show (fromJust (updatedAtCountry ct))) <> "\n"

       
isNot :: T.Text -> Bool
isNot = ("não" `T.isPrefixOf`) . T.toLower
-}

jsonCountriesN :: IO ()
jsonCountriesN = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [CountryName])
  case d of
      Left err   ->  TIO.putStrLn $ T.pack "Erro ao ler dicionário de dados. Erro: " <> T.pack (show err)
      Right ps -> do 
                    insertCountriesN ps :: IO ([Key CountryName])
                    pure()

jsonCountries :: IO ()
jsonCountries = do
                let countriesurl = "https://covid19-brazil-api.now.sh/api/report/v1/countries"
                d <- (eitherDecode <$> getJSONurl countriesurl) :: IO (Either String Countries)
                case d of
                    Left err ->  TIO.putStrLn $ T.pack "Erro ao ler dados de países. Erro: " <> T.pack (show err)
                    Right ps -> do 
                                  insertCountries (toCountry ps) :: IO ([Key Country])
                                  pure()
jsonStates :: IO ()
jsonStates = do
  let statesurl = "https://covid19-brazil-api.now.sh/api/report/v1"
  d <- (eitherDecode <$> getJSONurl statesurl) :: IO (Either String States)
  case d of
      Left err ->  TIO.putStrLn $ T.pack "Erro ao ler dados de estados. Erro: " <> T.pack (show err)
      Right ps -> do 
                    insertStates (toState ps) :: IO ([Key State])
                    pure()


jsonCovid :: IO ()
jsonCovid = do
  response <- httpLBS "https://covid19-brazil-api.now.sh/api/status/v1"
  case getResponseStatusCode response of 
    200 -> do
            jsonCountries
            jsonStates
            pure()
    _ -> TIO.putStrLn $ T.pack "Dados não disponíveis no momento =(" 
 


{- callIO :: IO()
callIO = do
        TIO.putStrLn "Digite o nome de um estado ou país para que eu possa mostrar os dados, ou então Não para sair \n \
                      \ Ah! E não esqueça os acentos 😉"
        let statesurl = "https://covid19-brazil-api.now.sh/api/report/v1"
        let countriesurl = "https://covid19-brazil-api.now.sh/api/report/v1/countries"
        str <- getLine
        d <- (eitherDecode <$> getJSON) :: IO (Either String [CountriesN])
        case d of
            Left err   ->  TIO.putStrLn $ T.pack "Não consegui ler os dados =( Erro: " <> T.pack (show err)
            Right ps -> do
                          k <- insertCountriesN (head ps) :: IO (Key CountriesN)
                          pure()
 


 -}