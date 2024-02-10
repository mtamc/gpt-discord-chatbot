{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module App (App, Env (..), mkEnv, run) where

import App.Config
import App.Personality            (Personality)
import App.Personality qualified
import Database.PostgreSQL.Simple qualified as PGS
import Discord                    (DiscordHandler)
import Discord.Types              (ApplicationId)
import Relude.Extra.Newtype       (un)

type App a = ReaderT Env DiscordHandler a

data Env
  = Env
    { db            ∷ PGS.Connection
    , cfg           ∷ Config
    , personalities ∷ [Personality]
    , appId         ∷ MVar ApplicationId
    , apiKey        ∷ ApiKey
    }
  deriving (Generic)

mkEnv ∷ MonadIO m ⇒ m Env
mkEnv = liftIO do
  cfg           ← App.Config.load
  personalities ← App.Personality.load
  appId         ← newEmptyMVar
  db  ← PGS.connect PGS.ConnectInfo
    { PGS.connectHost     = cfg.postgresHost
    , PGS.connectPort     = cfg.postgresPort
    , PGS.connectUser     = cfg.postgresUser
    , PGS.connectPassword = cfg.postgresPassword
    , PGS.connectDatabase = cfg.postgresDb
    }
  pure $ Env db cfg personalities appId cfg.apiKey

run ∷ Env → App a → DiscordHandler a
run env = usingReaderT env . un
