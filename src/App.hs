{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module App (App, Env (..), mkEnv, run) where

import App.Config                 (Config)
import App.Config qualified
import App.Personality            (Personality)
import App.Personality qualified
import Database.PostgreSQL.Simple qualified as PGS
import Discord                    (DiscordHandler)
import Discord.Types              (ApplicationId)
import Network.HTTP.Client.TLS    (newTlsManager)
import OpenAI.Client              (OpenAIClient, makeOpenAIClient)
import Relude.Extra.Newtype       (un)

type App a = ReaderT Env DiscordHandler a

data Env
  = Env
    { db            ∷ PGS.Connection
    , cfg           ∷ Config
    , personalities ∷ [Personality]
    , appId         ∷ MVar ApplicationId
    , openAi        ∷ OpenAIClient
    }
  deriving (Generic)

mkEnv ∷ MonadIO m ⇒ m Env
mkEnv = liftIO do
  cfg           ← App.Config.load
  personalities ← App.Personality.load
  appId         ← newEmptyMVar
  manager       ← newTlsManager
  let openAi = makeOpenAIClient cfg.openAiKey manager 5
  db  ← PGS.connect PGS.ConnectInfo
    { PGS.connectHost     = cfg.postgresHost
    , PGS.connectPort     = cfg.postgresPort
    , PGS.connectUser     = cfg.postgresUser
    , PGS.connectPassword = cfg.postgresPassword
    , PGS.connectDatabase = cfg.postgresDb
    }
  pure $ Env db cfg personalities appId openAi

run ∷ Env → App a → DiscordHandler a
run env = usingReaderT env . un
