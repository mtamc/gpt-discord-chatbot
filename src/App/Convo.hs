{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module App.Convo (ConvoStarter (..), continue, start) where

import App                     (App, Env (..))
import App.DB                  qualified as DB
import App.Discord.Lenses
import App.Discord.SendMessage (replyIntr, replyMsg)
import App.GPT                 qualified as GPT
import App.Personality         (Personality (..))
import App.Prompt
import Control.Lens            (_Just, (^.), (^?))
import Data.Maybe              (fromJust)
import Discord                 (restCall)
import Discord.Interactions    (Interaction)
import Discord.Requests        (ChannelRequest (GetChannelMessage, TriggerTypingIndicator),
                                InteractionResponseRequest (DeleteOriginalInteractionResponse))
import Discord.Types           (DiscordId (DiscordId), GuildMember, Message (..),
                                Snowflake (Snowflake), User (..))
import Relude.Unsafe           qualified as Unsafe
import UnliftIO.Concurrent     (forkIO, threadDelay)

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
        (Prompt
          [ systemMsg pers.prompt
          , PromptMsg App.Prompt.User (Just "User") msgWithContent.messageContent
          ]
        )
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
                       "User"
                       False
                       msg.messageContent
      DB.createMessage reply.messageId
                       msg.messageId
                       msg.messageAuthor.userId
                       pers.cmd
                       pers.cmd
                       True
                       reply.messageContent

    SlashStarter i (Just txt) → do
      response ← GPT.complete
        (Prompt
          [ systemMsg pers.prompt
          , PromptMsg App.Prompt.User (Just "User") txt
          ]
        )
      let quotedStarter = unlines . map ("> " ⊕) $ lines txt
      reply ← replyIntr i (quotedStarter ⊕ response)
      DB.createMessage (i ^. id)
                       (i ^. id)
                       requesterId
                       pers.cmd
                       "User"
                       False
                       txt
      DB.createMessage reply.messageId
                       (i ^. id)
                       requesterId
                       pers.cmd
                       pers.cmd
                       True
                       reply.messageContent
    SlashStarter i Nothing → do
      let greeting = fromJust pers.greeting
      reply ← replyIntr i greeting
      DB.createMessage reply.messageId
                       reply.messageId
                       requesterId
                       pers.cmd
                       pers.cmd
                       True
                       greeting

continue ∷ Personality → Message → NonEmpty DB.SavedMsg → App ()
continue pers msg history = do
  responsePosted ← newMVar False
  let triggerLongTypingIndicator = do
        isPosted ← readMVar responsePosted
        if isPosted
          then pass
          else do
            void . restCall $ TriggerTypingIndicator msg.messageChannelId
            threadDelay 5_000_000
            triggerLongTypingIndicator
  void . lift $ forkIO triggerLongTypingIndicator
  DB.createMessage msg.messageId
                   (textToDiscordId (head history).opId)
                   (textToDiscordId (head history).interlocutorId)
                   pers.cmd
                   "User"
                   False
                   msg.messageContent
  response ← GPT.complete gptPrompt
  reply ← replyMsg msg response
  void $ swapMVar responsePosted True
  DB.createMessage reply.messageId
                   (textToDiscordId (head history).opId)
                   (textToDiscordId (head history).interlocutorId)
                   pers.cmd
                   pers.cmd
                   True
                   ("AI: " ⊕ response)
  where
  textToDiscordId = DiscordId . Snowflake . Unsafe.read @Word64 . toString
  toks ∷ Text → Int
  toks = round . (* 0.75) . fromIntegral @Int @Float . length . words
  msgHistory =
    ( map
        (\savedMsg →
          PromptMsg
            (if savedMsg.isBot then App.Prompt.Assistant else App.Prompt.User)
            (Just savedMsg.author)
            savedMsg.content
        )
    . reverse
    $ toList history
    ) ⊕ [PromptMsg App.Prompt.User (Just "User") msg.messageContent]
  gptPrompt = let
    maxToks = 3000
    chatbotDefToks = toks pers.prompt
    spaceForMsgHistory = maxToks - chatbotDefToks
    historyToks hist = sum $ map ((.msgBody) ⋙ toks) hist
    truncatedHistory = until (\hist → historyToks hist < spaceForMsgHistory) (drop 1) msgHistory
    in Prompt $ systemMsg pers.prompt : truncatedHistory
