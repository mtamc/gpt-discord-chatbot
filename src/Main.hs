{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import App                (Env (..), mkEnv, run)
import App.Config         (Config (..))
import App.Discord.Events (onDiscordEvent)
import App.Discord.Lenses
import Control.Lens       ((.~))
import Discord            (def, runDiscord)

main ∷ IO ()
main = do
  echo "Bot started."
  env ← mkEnv
  botTerminationError ← runDiscord $ def
    & token         .~ env.cfg.discordBotToken
    & gatewayIntent .~ (def & messageContent .~ False)
    & onEvent       .~ App.run env . onDiscordEvent
  echo $ "A fatal error occurred: " ⊕ botTerminationError
