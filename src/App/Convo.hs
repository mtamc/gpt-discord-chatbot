module App.Convo (ConvoStarter (..), start) where

import App                     (App)
import App.DB                  qualified as DB
import App.Discord.Lenses
import App.Discord.SendMessage (replyIntr, replyMsg)
import App.GPT                 qualified as GPT
import App.Personality         (Personality (..))
import Control.Lens            (_Just, (^.), (^?))
import Data.Maybe              (fromJust)
import Discord.Interactions    (Interaction)
import Discord.Types           (GuildMember, Message (..), User (..))

data ConvoStarter
  = SlashStarter Interaction (Maybe Text)
  | CtxStarter Message

start ∷ Personality → GuildMember → ConvoStarter → App ()
start pers requester starter = do
  requesterId ← case requester ^? user . _Just of
    Just author → pure (author ^. id)
    _           → fail "Could not find requester ID"
  case starter of
    CtxStarter msg → do
      response ← GPT.complete
        $ pers.prompt
        ⊕ "\nHuman: " ⊕ msg.messageContent
        ⊕ "\nAI:"
      reply ← replyMsg msg
        (response ⊕ "\n\n*Requested by <@" ⊕ show requesterId ⊕ ">*")
      DB.createMessage msg.messageId
                       msg.messageId
                       msg.messageAuthor.userId
                       ("Human: " ⊕ msg.messageContent)
      DB.createMessage reply.messageId
                       msg.messageId
                       msg.messageAuthor.userId
                       ("AI: " ⊕ reply.messageContent)

    SlashStarter i (Just txt) → do
      response ← GPT.complete
        $ pers.prompt
        ⊕ "\nHuman: " ⊕ txt
        ⊕ "\nAI:"
      reply ← replyIntr i response
      DB.createMessage (i ^. id)
                       (i ^. id)
                       requesterId
                       ("Human: " ⊕ txt)
      DB.createMessage reply.messageId
                       (i ^. id)
                       requesterId
                       ("AI: " ⊕ reply.messageContent)

    SlashStarter i Nothing → do
      let greeting = fromJust $ pers.greeting
      reply ← replyIntr i greeting
      DB.createMessage reply.messageId
                       reply.messageId
                       requesterId
                       ("AI: " ⊕ greeting)
