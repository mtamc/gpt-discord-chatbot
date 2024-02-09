module App.GPT (complete) where

import App           (App, Env (..))
import App.Prompt
import Data.Text     qualified as Text
import Data.Vector   qualified as Vector
import OpenAI.Client

complete ∷ Prompt → App Text
complete (Prompt promptMsgs) = do
  openAi ← asks (.openAi)
  resp ← liftIO $ completeChat openAi ChatCompletionRequest
    { chcrModel = ModelId "gpt-3.5-turbo"
    , chcrMessages =
        promptMsgs & map \msg → ChatMessage
          { chmContent = Just msg.msgBody
          , chmRole = Text.toLower (show msg.msgType)
          , chmName = Nothing
          , chmFunctionCall = Nothing
          }
    , chcrTemperature = Just 0.7
    , chcrTopP = Nothing
    , chcrN = Nothing
    , chcrStream = Nothing
    , chcrStop = Nothing
    , chcrMaxTokens = Nothing
    , chcrPresencePenalty = Just 0
    , chcrFrequencyPenalty = Just 0
    , chcrLogitBias = Nothing
    , chcrUser = Nothing
    , chcrFunctions = Nothing
    }
  let problem = pure "[An OpenAI error occurred.]"
  case resp of
    Left err → print err ≫ problem
    Right completion →
      case nonEmpty completion.chrChoices of
        Nothing           → problem
        Just (choice:|[]) → pure $ Text.strip $ fromMaybe "" choice.chchMessage.chmContent
        _                 → problem
