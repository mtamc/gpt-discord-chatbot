module App.Config (Config (..), load) where

import Dhall (FromDhall, input, auto)

data Config = Config
  { openAiKey        ∷ Text
  , discordBotToken  ∷ Text
  , dmEnabled        ∷ Bool
  , debugGuildId     ∷ Word64
  , postgresHost     ∷ String
  , postgresPort     ∷ Word16
  , postgresUser     ∷ String
  , postgresPassword ∷ String
  , postgresDb       ∷ String
  } deriving (Generic, Show)
instance FromDhall Config

load ∷ MonadIO m ⇒ m Config
load = liftIO $ input auto "./config.dhall"
