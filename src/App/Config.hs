module App.Config (ApiKey (..), Config (..), debugGuildId, load) where

import Dhall         (FromDhall, auto, input)
import Discord.Types (DiscordId (..), GuildId, Snowflake (..))

data Config
  = Config
    { apiKey           ∷ ApiKey
    , openAIModel      ∷ Text
    , anthropicModel   ∷ Text
    , discordBotToken  ∷ Text
    , debugGuildId     ∷ Word64
    , postgresHost     ∷ String
    , postgresPort     ∷ Word16
    , postgresUser     ∷ String
    , postgresPassword ∷ String
    , postgresDb       ∷ String
    }
  deriving (Generic, Show)
instance FromDhall Config

data ApiKey
  = OpenAI Text
  | Anthropic Text
  deriving (Generic, Show)
instance FromDhall ApiKey

load ∷ MonadIO m ⇒ m Config
load = liftIO $ input auto "./config.dhall"

debugGuildId ∷ Config → GuildId
debugGuildId cfg = DiscordId $ Snowflake cfg.debugGuildId
