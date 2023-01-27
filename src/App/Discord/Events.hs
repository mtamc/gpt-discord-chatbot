module App.Discord.Events where

import App (App, Env (..))
import App.Personality (Personality (..))
import App.Personality qualified as Personality
import App.Config (Config (..))
import Discord (RestCallErrorCode(..), restCall)
import Discord.Types (Event (..), PartialApplication (..), ApplicationId)
import Discord.Interactions (ApplicationCommand (..))
import Discord.Internal.Rest.ApplicationCommands (ApplicationCommandRequest(CreateGuildApplicationCommand, CreateGlobalApplicationCommand, GetGlobalApplicationCommands, DeleteGlobalApplicationCommand, DeleteGuildApplicationCommand))

-- | Good to know:
-- [1] If an event handler throws, discord-haskell will carry on.
-- [2] onDiscordEvent/discordOnEvent is run in a thread
-- [3] Unsure what the Event data constructors' parameters are? Check out
--     <https://discord.com/developers/docs/topics/gateway-events#receive-events>
--     Parameters in discord-haskell are in the same order as in the above
--     documentation.
onDiscordEvent ∷ Event → App ()
onDiscordEvent = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) → onReady appId
  MessageCreate msg                              → error "" -- onMsg msg
  InteractionCreate intr                         → error "" -- onInteractionCreate intr
  _                                              → pass

onReady ∷ ApplicationId → App ()
onReady appId = do
  echo "Bot ready!"
  env ← ask
  appCmdRegistrations ← mapM tryRegistering env.personalities

  case sequence appCmdRegistrations of
    Left _err  → echo "[!] Failed to register some commands (check API rules)"
    Right cmds → do
      echo $ "Registered " ⊕ show (length cmds) ⊕ " command(s)."
      unregisterOutdatedCmds cmds
  where
  tryRegistering ∷ Personality → App (Either RestCallErrorCode ApplicationCommand)
  tryRegistering pers = do
    env ← ask
    let reg = Personality.toCmdReg pers
    lift . void . restCall $ CreateGuildApplicationCommand appId env.cfg.debugGuildId reg
    lift . restCall $ CreateGlobalApplicationCommand appId reg

  unregisterOutdatedCmds ∷ [ApplicationCommand] → App ()
  unregisterOutdatedCmds validCmds = do
    env ← ask
    registered ← lift . restCall $ GetGlobalApplicationCommands appId
    case registered of
      Left err → echo $ "Failed to get registered slash commands: " ⊕ show err
      Right cmds → do
        let outdatedIds =
              filter (∉ (validCmds <&> (.applicationCommandId)))
                (cmds <&> (.applicationCommandId))
        lift . forM_ outdatedIds $ \cmdId → do
          void . restCall $ DeleteGlobalApplicationCommand appId cmdId
          void . restCall $ DeleteGuildApplicationCommand appId env.cfg.debugGuildId cmdId

