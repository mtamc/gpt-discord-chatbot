{-# LANGUAGE FunctionalDependencies, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module App.Discord.Lenses where

import Control.Lens
import Discord
import Discord.Interactions
import Discord.Requests
import Discord.Types

makeFieldsOptionalPrefix "interaction" ''Interaction
makeFieldsOptionalPrefix "reference" ''MessageReference
makeFieldsOptionalPrefix "messageDetailed" ''MessageDetailedOpts
makeLensesWith abbreviatedFields ''RunDiscordOpts
makeLensesWith abbreviatedFields ''CreateApplicationCommand
makeFields ''GatewayIntent
makeFields ''OptionValue
makeFieldsOptionalPrefix "applicationCommandData" ''ApplicationCommandData
makeFields ''CreateEmbed
makeFields ''InteractionResponseMessage
makeFieldsOptionalPrefix "" ''Message
makeFieldsOptionalPrefix "member" ''GuildMember
makeFieldsOptionalPrefix "user" ''User
