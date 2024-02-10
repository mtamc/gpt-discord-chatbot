{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module App.AI (complete) where

import App
import App.Config
import App.Prompt                (MsgType (Assistant, System, User), Prompt (..),
                                  PromptMsg (msgBody, msgType))
import Control.Lens              ((^.))
import Data.Aeson
import Data.Text                 qualified as Text
import Network.HTTP.Client       (responseTimeoutMicro)
import Network.HTTP.Simple       (addRequestHeader, getResponseBody, httpJSONEither, parseRequest_,
                                  setRequestBodyLBS, setRequestResponseTimeout)
import Network.HTTP.Types.Header (hAuthorization, hContentType)

complete ∷ Prompt → App Text
complete prompt = asks (.apiKey) ≫= \case
  OpenAI key    → asks (^. #cfg . #openAIModel) ≫= openAIComplete key prompt
  Anthropic key → asks (^. #cfg . #anthropicModel) ≫= anthropicComplete key prompt


-- OPENAI

openAIComplete ∷ MonadIO m ⇒ Text → Prompt → Text → m Text
openAIComplete key (Prompt promptMsgs) model = do
  resp ← "POST https://api.openai.com/v1/chat/completions"
    & parseRequest_
    & addRequestHeader hAuthorization ("Bearer " ⊕ encodeUtf8 key)
    & addRequestHeader hContentType "application/json"
    & setRequestBodyLBS
      (encode $ object
        [ "max_tokens"        .= (500 ∷ Int)
        , "temperature"       .= (0.7 ∷ Float)
        , "frequency_penalty" .= (0 ∷ Float)
        , "presence_penalty"  .= (0 ∷ Float)
        , "model"             .= model
        , "stream"            .= False
        , "messages"          .= map toOpenAIMessage promptMsgs
        ]
       )
    & setRequestResponseTimeout (responseTimeoutMicro 30_000_000)
    & httpJSONEither
  case getResponseBody resp of
    Left err                            → print err ≫ print resp $> problemMsg
    Right (OpenAICompletion completion) → pure completion

data OpenAIMessage
  = OpenAIMessage
    { role    ∷ Text
    , content ∷ Text
    }
  deriving (Eq, Generic, Show, ToJSON)

toOpenAIMessage ∷ PromptMsg → OpenAIMessage
toOpenAIMessage promptMsg =
  OpenAIMessage
    { role = Text.toLower $ show promptMsg.msgType
    , content = promptMsg.msgBody
    }

data OpenAICompletion
  = OpenAICompletion Text
  deriving (Eq, Generic, Show)

instance FromJSON OpenAICompletion where
  parseJSON = withObject "resp" \obj → do
    choices ← obj .: "choices"
    case nonEmpty choices of
      Nothing → pure (OpenAICompletion problemMsg)
      Just (fstChoice:|_) → do
        msg ← fstChoice .: "message"
        content ← msg .: "content"
        pure $ OpenAICompletion content


-- ANTHROPIC

anthropicComplete ∷ MonadIO m ⇒ Text → Prompt → Text → m Text
anthropicComplete key prompt model = do
  resp ← "POST https://api.anthropic.com/v1/complete"
    & parseRequest_
    & addRequestHeader "x-api-key" (encodeUtf8 key)
    & addRequestHeader hContentType "application/json"
    & addRequestHeader "anthropic-version" "2023-06-01"
    & setRequestBodyLBS
      (encode $ object
        [ "max_tokens_to_sample" .= (500 ∷ Int)
        , "temperature"          .= (0.8 ∷ Float)
        , "model"                .= model
        , "stream"               .= False
        , "prompt"               .= (toString (toAnthropicPrompt prompt) ∷ String)
        ]
       )
    & setRequestResponseTimeout (responseTimeoutMicro 30_000_000)
    & httpJSONEither
  case getResponseBody resp of
    Left err → print err ≫ print resp $> problemMsg
    Right (AnthropicCompletion { completion }) →
      case completion of
        Nothing  → print resp $> problemMsg
        Just txt → pure txt


toAnthropicPrompt ∷ Prompt → Text
toAnthropicPrompt (Prompt msgs) =
  case nonEmpty msgs of
    Nothing → "\n\nAssistant:"
    Just (firstMsg:|otherMsgs) → let
      userMsg = ("\n\nHuman: " ⊕) . (.msgBody)
      assistantMsg = ("\n\nAssistant: "<>) . (.msgBody)
      firstMsgTxt =
        case firstMsg.msgType of
          System    → firstMsg.msgBody
          User      → userMsg firstMsg
          Assistant → assistantMsg firstMsg
      otherMsgsTxt =
        otherMsgs & map \msg → case msg.msgType of
          System    → "\n\nSystem: " ⊕ msg.msgBody
          User      → userMsg msg
          Assistant → assistantMsg msg
      in Text.concat (firstMsgTxt:otherMsgsTxt) ⊕ "\n\nAssistant:"

data AnthropicCompletion
  = AnthropicCompletion
    { completion ∷ Maybe Text
    }
  deriving (Eq, FromJSON, Generic, Show)


-- UTIL

problemMsg ∷ Text
problemMsg = "[An error occurred. Check console logs.]"
