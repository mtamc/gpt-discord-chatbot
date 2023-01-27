{-# OPTIONS_GHC -Wno-missing-fields #-}

module App.Personality (Personality (..), load, toCmdRegs) where

import App.Discord.Lenses
import Control.Lens
import Dhall                (FromDhall, auto, input)
import Discord.Interactions (CreateApplicationCommand (..), OptionValue (..), Options (..),
                             createChatInput, createMessage)

data Personality
  = Personality
    { cmd         ∷ Text
    , description ∷ Text
    , greeting    ∷ Maybe Text
    , prompt      ∷ Text
    }
  deriving (Generic, Show)
instance FromDhall Personality

load ∷ MonadIO m ⇒ m [Personality]
load = liftIO $ input auto "./personalities.dhall"

toCmdRegs ∷ Personality → [CreateApplicationCommand]
toCmdRegs pers =
  if length (words pers.prompt) > 1000
    then crash
    else slashCmd : maybeToList msgCtxMenuCmd
  where
  slashCmd ∷ CreateApplicationCommand
  slashCmd = fromMaybe crash (createChatInput pers.cmd pers.description)
    & options ?~ OptionsValues case pers.greeting of
      Just _  → []
      Nothing → [ OptionValueString {}
                    & name                 .~ "message"
                    & localizedName        .~ Nothing
                    & description          .~ "Your conversation starter"
                    & localizedDescription .~ Nothing
                    & required             .~ True
                    & stringChoices        .~ Left False
                    & stringMinLen         .~ Nothing
                    & stringMaxLen         ?~ 1500
                ]

  msgCtxMenuCmd ∷ Maybe CreateApplicationCommand
  msgCtxMenuCmd = case pers.greeting of
    Just _  → Nothing
    Nothing → Just . fromMaybe crash $ createMessage pers.cmd

  crash ∷ a
  crash = error
    $ "Something is wrong with the personality " ⊕ pers.cmd ⊕ "."
    ⊕ "`cmd` must be lowercase and between 1~32 characters. \n"
    ⊕ "`description` must be between 1~100 characters. \n"
    ⊕ "`prompt` should be no more than 1000 words."
