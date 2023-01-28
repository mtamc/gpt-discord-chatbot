module App.DB (SavedMsg (..), createMessage, getThreadByMsgId) where

import App                        (App, Env (..))
import Control.Exception          (handle)
import Control.Lens               (_1, _3, _4, (^.))
import Data.Profunctor.Product    (p7)
import Data.Time.LocalTime        (LocalTime)
import Database.PostgreSQL.Simple (SqlError (..))
import Discord.Types              (DiscordId (..), MessageId, Snowflake (..), UserId)
import Opaleye                    (Field, Insert (..), Select, SqlInt4, SqlText, SqlTimestamp,
                                   SqlVarcharN, Table, desc, limit, orderBy, rCount, runInsert,
                                   runSelectI, selectTable, table, tableField, toFields, where_,
                                   (.==))

data SavedMsg
  = SavedMsg
    { dbId           ∷ Int
    , msgId          ∷ Text
    , opId           ∷ Text
    , interlocutorId ∷ Text
    , personality    ∷ Text
    , content        ∷ Text
    }
  deriving (Show)

createMessage ∷ DiscordId a → DiscordId b → UserId → Text → Text → App ()
createMessage discordMsgId discordOpMsgId interlocutorId personality content = do
  db ← asks (.db)
  void . liftWithLog . runInsert db $
    messageInsert discordMsgId discordOpMsgId interlocutorId personality content

getThreadByMsgId ∷ MessageId → App [SavedMsg]
getThreadByMsgId (discordIdToText → discordMsgId) = do
  db ← asks (.db)
  rows ← liftWithLog . runSelectI db $ findMessageByDiscordMsgId discordMsgId
  (_,_,_,op,_,_,_):_ ← pure rows
  msgs ← liftWithLog . runSelectI db $ findThreadByDiscordOpMsgId op
  pure $ map toHask msgs
  where
  toHask ∷ (Int, LocalTime, Text, Text, Text, Text, Text) → SavedMsg
  toHask (dbId, _, msgId, opId, uId, personality, content) =
    SavedMsg dbId msgId opId uId personality content

liftWithLog ∷ IO a → App a
liftWithLog = liftIO . handle \(e ∷ SqlError) → do
  echo ("SQL error received: " ⊕ show e)
  fail (show e)

type MessagesRow dbId ts =
  ( dbId
  , ts
  , Field SqlVarcharN
  , Field SqlVarcharN
  , Field SqlVarcharN
  , Field SqlVarcharN
  , Field SqlText
  )
type MessagesRowWrite = MessagesRow (Maybe (Field SqlInt4)) (Maybe (Field SqlTimestamp))
type MessagesRowRead  = MessagesRow (Field SqlInt4) (Field SqlTimestamp)

messagesTable ∷ Table MessagesRowWrite MessagesRowRead
messagesTable = table "messages" $ p7
  ( tableField "db_id"
  , tableField "tstamp"
  , tableField "discord_msg_id"
  , tableField "discord_op_msg_id"
  , tableField "interlocutor_id"
  , tableField "personality"
  , tableField "content"
  )

messageInsert ∷ DiscordId a → DiscordId b → UserId → Text → Text → Insert Int64
messageInsert discordMsgId discordOpMsgId interlocutorId personality content =
  Insert messagesTable [row] rCount Nothing
  where row = ( Nothing
              , Nothing
              , toFields $ discordIdToText discordMsgId
              , toFields $ discordIdToText discordOpMsgId
              , toFields $ discordIdToText interlocutorId
              , toFields personality
              , toFields content
              )

discordIdToText ∷ DiscordId a → Text
discordIdToText = show . unSnowflake . unId

findMessageByDiscordMsgId ∷ Text → Select MessagesRowRead
findMessageByDiscordMsgId (toFields → discordMsgId) = do
  row ← selectTable messagesTable
  let rowDiscordMsgId = row ^. _3
  where_ (discordMsgId .== rowDiscordMsgId)
  pure row

findThreadByDiscordOpMsgId ∷ Text → Select MessagesRowRead
findThreadByDiscordOpMsgId (toFields → discordOpMsgId) =
  limit 30 . orderBy (desc (^. _1)) $ do
    row ← selectTable messagesTable
    let rowDiscordOpMsgId = row ^. _4
    where_ (discordOpMsgId .== rowDiscordOpMsgId)
    pure row
