{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module App.Personality (Personality (..), load, toCmdRegs) where

import Dhall (FromDhall, input, auto)
import Discord.Interactions (createChatInput, OptionValue (..), CreateApplicationCommand (..), Options (..), createMessage)

data Personality = Personality
  { cmd         ∷ Text
  , description ∷ Text
  , greeting    ∷ Maybe Text
  , prompt      ∷ Text
  } deriving (Generic, Show)
instance FromDhall Personality

load ∷ MonadIO m ⇒ m [Personality]
load = liftIO $ input auto "./personalities.dhall"

toCmdRegs ∷ Personality → [CreateApplicationCommand]
toCmdRegs pers =
  if length (words pers.prompt) > 1500
    then crash
    else slashCmd : maybeToList msgCtxMenuCmd
  where
  slashCmd ∷ CreateApplicationCommand
  slashCmd =
    ( fromMaybe crash
    $ createChatInput pers.cmd pers.description
    ) { createOptions = Just . OptionsValues $ case pers.greeting of
          Just _ → []
          Nothing →
            [ OptionValueString
              { optionValueName                 = "message"
              , optionValueLocalizedName        = Nothing
              , optionValueDescription          = "Your conversation starter"
              , optionValueLocalizedDescription = Nothing
              , optionValueRequired             = True
              , optionValueStringChoices        = Left False
              , optionValueStringMinLen         = Nothing
              , optionValueStringMaxLen         = Nothing
              }
            ]
      }
  msgCtxMenuCmd ∷ Maybe CreateApplicationCommand
  msgCtxMenuCmd =
    case pers.greeting of
      Just _  → Nothing
      Nothing → Just . fromMaybe crash $ createMessage pers.cmd
  crash ∷ a
  crash = error
    $ "Something is wrong with the personality " ⊕ pers.cmd ⊕ "."
    ⊕ "`cmd` must be lowercase and between 1~32 characters. \n"
    ⊕ "`description` must be between 1~100 characters. \n"
    ⊕ "`prompt` should be no more than 1500 words."
