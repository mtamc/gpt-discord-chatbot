{-# OPTIONS -Wno-orphans -Wno-missing-fields #-}

module App.Discord.SendMessage (replyIntr, replyMsg) where

import App                  (App, Env (..))
import App.Discord.Lenses
import App.Discord.Lenses   qualified as L
import Control.Lens         ((.~), (?~), (^.))
import Data.Default         (Default (def))
import Discord              (restCall)
import Discord.Interactions (Interaction, InteractionResponseMessage (..),
                             interactionResponseMessageBasic)
import Discord.Requests     qualified as R
import Discord.Types        (Message, MessageReference (..))

instance Default InteractionResponseMessage where
  def = InteractionResponseMessage Nothing Nothing Nothing Nothing Nothing Nothing Nothing

replyIntr ∷ Interaction → Text → App Message
replyIntr intr md = do
  env ← ask
  appId ← liftIO $ readMVar env.appId
  result ← lift . restCall . R.CreateFollowupInteractionMessage appId (intr ^. token)
    $ interactionResponseMessageBasic md
  case result of
    Left _      → fail "Failed to reply to interaction"
    Right reply → pure reply

replyMsg ∷ Message → Text → App Message
replyMsg msg md = lift do
  result ← restCall . R.CreateMessageDetailed (msg ^. messageChannelId) $
    def & content   .~ md
        & reference ?~ ( MessageReference {}
                           & messageId ?~ (msg ^. messageId)
                           & channelId ?~ (msg ^. messageChannelId)
                           & guildId   .~ (msg ^. messageGuildId)
                           & L.failIfNotExists .~ True
                       )
  case result of
    Left _      → fail "Failed to reply"
    Right reply → pure reply
