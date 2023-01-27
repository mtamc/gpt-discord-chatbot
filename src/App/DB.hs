module App.DB (createMessage, getThreadByMsgId) where

import App (App, Env (..))
import Data.Profunctor.Product(p5)
import Opaleye (Field, Insert (..), Select, SqlInt4, SqlText, SqlVarcharN, Table, rCount, runInsert, runSelectI, selectTable, table, tableField, toFields, where_, (.==), orderBy, desc, limit)
import Discord.Types (MessageId, UserId, DiscordId (..), Snowflake (..))
import Control.Lens ((^.), _1, _2, _3)
import App.SavedMsg (SavedMsg (..))

createMessage ∷ MessageId → MessageId → UserId → Text → App ()
createMessage discordMsgId discordOpMsgId interlocutorId content = do
  db ← asks (.db)
  void . liftIO . runInsert db $
    messageInsert discordMsgId discordOpMsgId interlocutorId content

getThreadByMsgId ∷ MessageId → App [SavedMsg]
getThreadByMsgId (discordIdToText → discordMsgId) = do
  db ← asks (.db)
  [(_,_,op,_,_)] ← liftIO . runSelectI db $ findMessageByDiscordMsgId discordMsgId
  msgs ← liftIO . runSelectI db $ findThreadByDiscordOpMsgId op
  pure $ map toHask msgs
  where
  toHask ∷ (Int, Text, Text, Text, Text) → SavedMsg
  toHask (dbId, msgId, opId, uId, content) =
    SavedMsg dbId msgId opId uId content

type MessagesRow dbId =
  ( dbId
  , Field SqlVarcharN
  , Field SqlVarcharN
  , Field SqlVarcharN
  , Field SqlText
  )
type MessagesRowWrite = MessagesRow (Maybe (Field SqlInt4))
type MessagesRowRead  = MessagesRow (Field SqlInt4)

messagesTable ∷ Table MessagesRowWrite MessagesRowRead
messagesTable = table "messages" $ p5
  ( tableField "db_id"
  , tableField "discord_msg_id"
  , tableField "discord_op_msg_id"
  , tableField "interlocutor_id"
  , tableField "content"
  )

messageInsert ∷ MessageId → MessageId → UserId → Text → Insert Int64
messageInsert discordMsgId discordOpMsgId interlocutorId content =
  Insert messagesTable [row] rCount Nothing
  where row = ( Nothing
              , toFields $ discordIdToText discordMsgId
              , toFields $ discordIdToText discordOpMsgId
              , toFields $ discordIdToText interlocutorId
              , toFields content
              )

discordIdToText ∷ DiscordId a → Text
discordIdToText = show . unSnowflake . unId

findMessageByDiscordMsgId ∷ Text → Select MessagesRowRead
findMessageByDiscordMsgId (toFields → discordMsgId) = do
  row ← selectTable messagesTable
  let rowDiscordMsgId = row ^. _2
  where_ (discordMsgId .== rowDiscordMsgId)
  pure row

findThreadByDiscordOpMsgId ∷ Text → Select MessagesRowRead
findThreadByDiscordOpMsgId (toFields → discordOpMsgId) =
  limit 30 . orderBy (desc (^. _1)) $ do
    row ← selectTable messagesTable
    let rowDiscordOpMsgId = row ^. _3
    where_ (discordOpMsgId .== rowDiscordOpMsgId)
    pure row
