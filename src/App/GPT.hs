{-# LANGUAGE OverloadedLists #-}

module App.GPT (complete) where

import App           (App, Env (..))
import Data.Text     qualified as Text
import Data.Vector   qualified as Vector
import OpenAI.Client

complete ∷ Text → App Text
complete prompt = do
  openAi ← asks (.openAi)
  resp ← liftIO $ completeText openAi (EngineId "text-davinci-003") $
    (defaultTextCompletionCreate prompt)
      { tccrMaxTokens = Just 1000
      , tccrTemperature = Just 0.9
      , tccrStop = Just ["Human:"]
      , tccrPresencePenalty = Just 0.6
      }
  let problem = pure "[An OpenAI error occurred.]"
  case resp of
    Left err → print err ≫ problem
    Right completion →
      case nonEmpty $ Vector.toList completion.tcChoices of
        Nothing           → print completion ≫ problem
        Just (choice:|[]) → pure $ Text.strip choice.tccText
        _                 → print completion ≫ problem
