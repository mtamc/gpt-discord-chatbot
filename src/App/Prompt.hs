module App.Prompt (MsgType (..), Prompt (..), PromptMsg (..), systemMsg) where

newtype Prompt
  = Prompt [PromptMsg]
  deriving (Eq, Generic, Show)

data PromptMsg
  = PromptMsg
    { msgType   ∷ MsgType
    , msgAuthor ∷ Maybe Text
    , msgBody   ∷ Text
    }
  deriving (Eq, Generic, Show)

data MsgType = System | User | Assistant deriving (Eq, Generic, Show)

systemMsg ∷ Text → PromptMsg
systemMsg body = PromptMsg System Nothing body

