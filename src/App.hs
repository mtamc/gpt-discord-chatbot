module App (App, Env (..), mkEnv, withEnv) where

import App.Config qualified
import App.Config (Config)
import App.Personality (Personality)
import App.Personality qualified
import Database.PostgreSQL.Simple qualified as PGS
import Discord (DiscordHandler)
import Relude.Extra.Newtype (un)

type App a = ReaderT Env DiscordHandler a

data Env = Env
  { db            ∷ PGS.Connection
  , cfg           ∷ Config
  , personalities ∷ [Personality]
  } deriving (Generic)

mkEnv ∷ MonadIO m ⇒ m Env
mkEnv = do
  cfg ← App.Config.load
  db  ← liftIO $ PGS.connect PGS.ConnectInfo
    { PGS.connectHost     = cfg.postgresHost
    , PGS.connectPort     = cfg.postgresPort
    , PGS.connectUser     = cfg.postgresUser
    , PGS.connectPassword = cfg.postgresPassword
    , PGS.connectDatabase = cfg.postgresDb
    }
  personalities ← App.Personality.load
  pure $ Env { db            = db
             , cfg           = cfg
             , personalities = personalities
             }

withEnv ∷ Env → App a → DiscordHandler a
withEnv env = usingReaderT env . un
