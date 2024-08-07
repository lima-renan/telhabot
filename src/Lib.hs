{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- https://siskam.link/2018-02-22-telegram-bot-and-haskell.html
-- https://medium.com/@misha.kobelev.99/telegram-bot-on-haskell-with-db-connection-e0031f109166
-- https://github.com/mike-kob/HaskellBot/blob/master/app/Main.hs
-- https://gist.github.com/tfausak/cdcc2e33c9c418278262650eb96b7b05

module Lib where

import DataBaseFunctions
import           Covid
import Schema



import           Control.Applicative              ((<|>))
import           Control.Monad.IO.Class           (liftIO)
import           Data.Maybe
import           Data.Text
import           Data.Time                        (getCurrentTime)
import           System.Environment               (getEnv)
import           Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

data ChatState
  = InitSate
  deriving (Show, Eq)

newtype ChatModel =
  ChatModel ChatState
  deriving (Show, Eq)

data Action
  = NoAction
  | RecordMsg Int (Maybe Text) Int Text
  deriving (Show, Read)

botStartup :: IO ()
botStartup
    -- logging
 = do
  env_token <- liftIO $ readFile "./src/auth-token.secret"  
  let token = Token . pack $ env_token
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId incexpBotApp) env
  

emptyChatModel :: ChatModel
emptyChatModel = ChatModel InitSate

incexpBotApp :: BotApp ChatModel Action
incexpBotApp =
  BotApp {botInitialModel = emptyChatModel, botAction = flip handleUpdate, botHandler = handleAction, botJobs = []}

handleUpdate :: ChatModel -> Update -> Maybe Action
handleUpdate model update =
  let msg = fromJust $ updateMessage update
      usr = fromJust $ messageFrom msg
      Telegram.UserId usrId = Telegram.userId usr
      Telegram.MessageId msgId = Telegram.messageMessageId msg
      usrIdInt = fromIntegral usrId :: Int
      msgIdInt = fromIntegral msgId :: Int
      usrName = Telegram.userUsername usr
      parser = RecordMsg usrIdInt usrName msgIdInt <$> plainText
   in parseUpdate parser update

handleAction :: Action -> ChatModel -> Eff Action ChatModel
handleAction action model =
  case action of
    NoAction -> pure model
    RecordMsg usrId usrname msgId txt ->
      model <# do
        maybeUser <- liftIO $ getUserById usrId
        now <- liftIO getCurrentTime
        case maybeUser of
          Just user -> liftIO $ jsonCountriesN >> jsonCovid
          Nothing -> do
            userKy <- liftIO $ createUser (Schema.User usrId usrname now)
            replyString "Hi. Nice to meet you, I'm a simple bot that records msgs. Recording your msg..."
        _ <- liftIO $ insertMsg $ Schema.Message msgId (Schema.UserKey usrId) txt now
        replyString "Done"
        pure NoAction

replyString :: String -> BotM ()
replyString = reply . toReplyMessage . pack


{- 
handleAction :: Action -> ChatModel -> Eff Action ChatModel
handleAction action model =
  case action of
    NoAction -> pure model
    RecordMsg usrId usrname msgId txt ->
      model <# do
        maybeUser <- liftIO $ getUserById usrId
        now <- liftIO getCurrentTime
        case maybeUser of
          Just user -> replyString "Hi again. Recording your msg..."
          Nothing -> do
            userKy <- liftIO $ createUser (Schema.User usrId usrname now)
            replyString "Hi. Nice to meet you, I'm a simple bot that records msgs. Recording your msg..."
        _ <- liftIO $ insertMsg $ Schema.Message msgId (Schema.UserKey usrId) txt now
        replyString "Done"
        pure NoAction -}