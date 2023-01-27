module Main (main) where

import App
import App.Config (Config (..))
import Discord (def, runDiscord, RunDiscordOpts (..), )
import Discord.Types (GatewayIntent (..))

main ∷ IO ()
main = do
  echo "Bot started."
  env ← mkEnv
  botTerminationError ← runDiscord def
    { discordToken = env.cfg.discordBotToken
    , discordGatewayIntent = def { gatewayIntentMessageContent = False }
    -- , onEvent = App.withEnv env . onDiscordEvent
    }
  echo $ "A fatal error occurred: " ⊕ botTerminationError

