# gpt-discord-chatbot

A Discord chatbot powered by OpenAI/Anthropic, written in Haskell, supporting multiple chatbot personalities.

## How to use

- Copy `./config.example.dhall` to `./config.dhall`, put in your OpenAI or Anthropic API key and Discord bot token.
- Define your personalities in `personalities.dhall`, copy the established format and read the comments for further instructions.
- Start the database with `docker compose up --build`.
- Run `cabal run`, or, if you don't want to compile from source, [download the appropriate binary for your OS](./releases) and place it at the repository's root. Then run the binary in a terminal.

## Examples

![1707566677](https://github.com/mtamc/gpt-discord-chatbot/assets/132402596/358a4193-33f2-4dfc-9286-babd964664df)

![1707566685](https://github.com/mtamc/gpt-discord-chatbot/assets/132402596/2a964bee-2113-4fbf-aa01-8a73cf757185)
