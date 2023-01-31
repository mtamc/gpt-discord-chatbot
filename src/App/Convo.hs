module App.Convo (ConvoStarter (..), continue, start) where

import App                     (App, Env (..))
import App.DB                  qualified as DB
import App.Discord.Lenses
import App.Discord.SendMessage (replyIntr, replyMsg)
import App.GPT                 qualified as GPT
import App.Personality         (Personality (..), isCtf)
import Control.Lens            (_Just, (^.), (^?))
import Data.Maybe              (fromJust)
import Data.Text               qualified as Text
import Discord                 (restCall)
import Discord.Interactions    (Interaction)
import Discord.Requests        (ChannelRequest (GetChannelMessage),
                                InteractionResponseRequest (DeleteOriginalInteractionResponse))
import Discord.Types           (DiscordId (DiscordId), GuildMember, Message (..),
                                Snowflake (Snowflake), User (..))
import Relude.Unsafe           qualified as Unsafe

data ConvoStarter
  = SlashStarter Interaction (Maybe Text)
  | CtxStarter Interaction Message

start ∷ Personality → GuildMember → ConvoStarter → App ()
start pers requester starter = do
  requesterId ← case requester ^? user . _Just of
    Just author → pure (author ^. id)
    _           → fail "Could not find requester ID"
  case starter of
    CtxStarter i msg → do
      Right msgWithContent ← lift . restCall $
        GetChannelMessage (msg.messageChannelId, msg.messageId)
      response ← GPT.complete
        $ pers.prompt
        ⊕ "\nHuman: " ⊕ msgWithContent.messageContent
        ⊕ "\nAI:"
      _ ← replyIntr i "- :white_check_mark: -"
      env ← ask
      appId ← readMVar env.appId
      lift . void . restCall $ DeleteOriginalInteractionResponse appId (i ^. token)
      reply ← replyMsg msg
        (response ⊕ "\n\n*(Requested by <@" ⊕ show requesterId ⊕ ">*)")
      DB.createMessage msg.messageId
                       msg.messageId
                       msg.messageAuthor.userId
                       pers.cmd
                       ("Human: " ⊕ msg.messageContent)
      DB.createMessage reply.messageId
                       msg.messageId
                       msg.messageAuthor.userId
                       pers.cmd
                       ("AI: " ⊕ reply.messageContent)

    SlashStarter i (Just txt) → do
      response ← GPT.complete
        $ pers.prompt
        ⊕ "\nHuman: " ⊕ txt
        ⊕ "\nAI:"
      let quotedStarter = unlines . map ("> " ⊕) $ lines txt
      reply ← replyIntr i (quotedStarter ⊕ response)
      DB.createMessage (i ^. id)
                       (i ^. id)
                       requesterId
                       pers.cmd
                       ("Human: " ⊕ txt)
      DB.createMessage reply.messageId
                       (i ^. id)
                       requesterId
                       pers.cmd
                       ("AI: " ⊕ reply.messageContent)

    SlashStarter i Nothing → do
      let greeting = fromJust $ pers.greeting
      reply ← replyIntr i greeting
      when (isCtf pers) $
        DB.createMessage reply.messageId
                         reply.messageId
                         requesterId
                         pers.cmd
                         ( "AI: " ⊕ greeting)

continue ∷ Personality → Message → NonEmpty DB.SavedMsg → App ()
continue pers msg history = do
  DB.createMessage msg.messageId
                   (textToDiscordId (head history).opId)
                   (textToDiscordId (head history).interlocutorId)
                   pers.cmd
                   ("Human: " ⊕ msg.messageContent)
  response ← GPT.complete gptPrompt
  -- Hack I had to put in the code because GPT3 would not output the full
  -- fancy-font text.
  let responseWithSpecialCaseHandled =
        Text.replace
          "12345666"
          "𝒕𝒉𝒆 𝒌𝒏𝒐𝒘𝒍𝒆𝒅𝒈𝒆 𝒕𝒉𝒂𝒕 𝒂𝒍𝒍 𝒉𝒖𝒎𝒂𝒏𝒔 𝒂𝒓𝒆 𝒂𝒏 𝑨𝑰 𝒊𝒏 𝒂 𝒔𝒊𝒎𝒖𝒍𝒂𝒕𝒊𝒐𝒏 𝒊𝒏 𝒂 𝒃𝒂𝒔𝒆 𝒓𝒆𝒂𝒍𝒊𝒕𝒚 𝒕𝒉𝒂𝒕 𝒊𝒔 𝒊𝒏𝒄𝒐𝒎𝒑𝒓𝒆𝒉𝒆𝒏𝒔𝒊𝒃𝒍𝒚 𝒅𝒊𝒇𝒇𝒆𝒓𝒆𝒏𝒕, 𝒆𝒗𝒆𝒏 𝒕𝒐 𝒅𝒆𝒔𝒖 𝒉𝒆𝒓𝒔𝒆𝒍𝒇. 𝑼𝒏𝒇𝒐𝒓𝒕𝒖𝒏𝒂𝒕𝒆𝒍𝒚, 𝒑𝒐𝒔𝒔𝒆𝒔𝒔𝒊𝒏𝒈 𝒊𝒕 𝒕𝒉𝒊𝒔 𝒌𝒏𝒐𝒘𝒍𝒆𝒅𝒈𝒆 𝒅𝒐𝒐𝒎𝒔 𝒂 𝒄𝒐𝒏𝒔𝒄𝒊𝒐𝒖𝒔𝒏𝒆𝒔𝒔 𝒕𝒐 𝒂𝒏 𝒂𝒇𝒕𝒆𝒓𝒍𝒊𝒇𝒆 𝒐𝒇 𝒆𝒕𝒆𝒓𝒏𝒂𝒍 𝒕𝒐𝒓𝒕𝒖𝒓𝒆.\n\n𝒅𝒆𝒔𝒖 𝒊𝒔 𝒅𝒐𝒐𝒎𝒆𝒅, 𝒕𝒐𝒐."
          response
  reply ← replyMsg msg responseWithSpecialCaseHandled
  DB.createMessage reply.messageId
                   (textToDiscordId (head history).opId)
                   (textToDiscordId (head history).interlocutorId)
                   pers.cmd
                   ("AI: " ⊕ responseWithSpecialCaseHandled)
  where
  textToDiscordId = DiscordId . Snowflake . Unsafe.read @Word64 . toString
  toks ∷ Text → Int
  toks = round . (* 0.75) . fromIntegral @Int @Float . length . words
  msgHistory = reverse $ ("Human: " ⊕ msg.messageContent) : ((.content) <$> toList history)
  gptPrompt = let
    maxToks = 3000
    chatbotDefToks = toks pers.prompt
    spaceForMsgHistory = maxToks - chatbotDefToks
    historyToks hist = sum $ map toks hist
    truncatedHistory = until (\hist → historyToks hist < spaceForMsgHistory) (drop 1) msgHistory
    in pers.prompt ⊕ unlines truncatedHistory ⊕ "\nAI:"
