module App.Discord.Events where

import App                                       (App, Env (..))
import App.Config qualified
import App.Convo qualified
import App.Discord.Lenses                        (HasApplicationCommandData (applicationCommandData),
                                                  HasChannelId (channelId), HasId (id),
                                                  HasName (name), HasOptionsData (optionsData),
                                                  HasToken (token), HasUser (user))
import App.Discord.SendMessage                   (replyIntr)
import App.Personality                           (Personality)
import App.Personality                           qualified as Personality
import Control.Lens                              (_Just, _Left, to, (^.), (^?))
import Data.Maybe                                (fromJust)
import Discord                                   (RestCallErrorCode (..), restCall)
import Discord.Interactions                      (ApplicationCommand (..),
                                                  ApplicationCommandData (ApplicationCommandDataChatInput, ApplicationCommandDataMessage),
                                                  CreateApplicationCommand, Interaction,
                                                  InteractionResponse (InteractionResponseDeferChannelMessage),
                                                  MemberOrUser (MemberOrUser),
                                                  OptionDataValue (OptionDataValueString),
                                                  OptionsData (OptionsDataValues), createChatInput)
import Discord.Internal.Rest.ApplicationCommands (ApplicationCommandRequest (..))
import Discord.Requests                          qualified as R
import Discord.Types                             (ApplicationId, ChannelId, Event (..),
                                                  GuildMember (..), PartialApplication (..),
                                                  User (..))

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
  InteractionCreate intr                         → onInteractionCreate intr
  MessageCreate msg                              → error "" -- onMsg msg
  _                                              → pass

onReady ∷ ApplicationId → App ()
onReady appId = do
  echo "Bot ready!"
  env ← ask
  liftIO $ putMVar env.appId appId
  let regs = helpCmd : concatMap Personality.toCmdRegs env.personalities
  appCmdRegistrations ← mapM tryRegistering regs

  case sequence appCmdRegistrations of
    Left _err  → echo "[!] Failed to register some commands (check API rules)"
    Right cmds → do
      echo $ "Registered " ⊕ show (length cmds) ⊕ " command(s)."
      unregisterOutdatedCmds cmds
  where
  helpCmd ∷ CreateApplicationCommand
  helpCmd = fromJust $ createChatInput "help" "AI Chatbot Help"

  tryRegistering ∷ CreateApplicationCommand → App (Either RestCallErrorCode ApplicationCommand)
  tryRegistering reg = do
    cfg ← asks (.cfg)
    lift . void . restCall $ CreateGuildApplicationCommand appId (App.Config.debugGuildId cfg) reg
    lift . restCall $ CreateGlobalApplicationCommand appId reg

  unregisterOutdatedCmds ∷ [ApplicationCommand] → App ()
  unregisterOutdatedCmds validCmds = do
    cfg ← asks (.cfg)
    registered ← lift . restCall $ GetGlobalApplicationCommands appId
    case registered of
      Left err → echo $ "Failed to get registered slash commands: " ⊕ show err
      Right cmds → do
        let validIds = validCmds <&> (.applicationCommandId)
            outdatedIds = filter (∉ validIds) (cmds <&> (.applicationCommandId))
        lift . forM_ outdatedIds $ \cmdId → do
          void . restCall $ DeleteGlobalApplicationCommand appId cmdId
          void . restCall $ DeleteGuildApplicationCommand appId (App.Config.debugGuildId cfg) cmdId

onInteractionCreate ∷ Interaction → App ()
onInteractionCreate i = do
  personalities ← asks (.personalities)
  case intrData personalities i of
    Left err → echo err
    Right (cmdData, cmdName, chanId, mem, opts) → do
      let pers = fromJust $ find ((== cmdName) . (.cmd)) personalities
      deferInteraction
      case cmdName of
        "help" → void . replyIntr i $ mkHelp personalities
        _ →
          case opts of
            Just (OptionsDataValues [OptionDataValueString _ (Right opTxt)]) →
              App.Convo.start pers mem (App.Convo.SlashStarter i $ Just opTxt)

            _ →
              case cmdData of
                ApplicationCommandDataChatInput {} →
                  App.Convo.start pers mem (App.Convo.SlashStarter i Nothing)

                ApplicationCommandDataMessage _ _ _ msgId → do
                  msgEither ← lift . restCall $ R.GetChannelMessage (chanId, msgId)
                  case msgEither of
                    Left _    → echo "error getting channel msg"
                    Right msg → App.Convo.start pers mem (App.Convo.CtxStarter msg)

                _ →
                  echo "Weird application command received"
  where
  intrData ∷ [Personality] → Interaction → Either Text (ApplicationCommandData, Text, ChannelId, GuildMember, Maybe OptionsData)
  intrData personalities intr = do
    cmdData ← intr ^? applicationCommandData
                & onFail "Unexpected/unsupported interaction type"
    cmdName ← find (== cmdData ^. name) ("help" : map (.cmd) personalities)
                & onFail "Unknown command (regs out of date?)"
    member  ← intr ^? user . to unMemberOrUser . _Left
               & onFail "Command has no guild member (they likely left)"
    chanId  ← intr ^? channelId . _Just
               & onFail "Command has no guild member (they likely left)"
    pure (cmdData, cmdName, chanId, member, cmdData ^? optionsData . _Just)

  deferInteraction ∷ App ()
  deferInteraction = lift do
    result ← restCall . R.CreateInteractionResponse (i ^. id) (i ^. token) $
      InteractionResponseDeferChannelMessage
    case result of
      Left _   → echo "Failed to defer interaction."
      Right () → pass

  unMemberOrUser ∷ MemberOrUser → Either GuildMember User
  unMemberOrUser (MemberOrUser unwrapped) = unwrapped

  mkHelp ∷ [Personality] → Text
  mkHelp personalities = let
    renderPersonalities predicate
      = unlines
      . map (\p → "`/" ⊕ p.cmd ⊕ "` - " ⊕ p.description)
      . filter predicate
      $ personalities
    in "Start a conversation with a chatbot using one of the below slash commands.\n"
    ⊕ "Some chatbots require you to write the first message. You can also start a conversation with those bots on a message's context menu in Apps → {name of the chatbot}. This works on other people's messages as well.\n"
    ⊕ "Remember to check with server moderators when, where, and how you may use the chatbots.\n\n"
    ⊕ "Chatbots requiring you to write the first message or using message context menu:\n"
    ⊕ renderPersonalities (isNothing . (.greeting))
    ⊕ "\n\nChatbots that start the conversation:\n"
    ⊕ renderPersonalities (isJust . (.greeting))
