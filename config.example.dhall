let ApiKey = < OpenAI : Text | Anthropic : Text>
in
{ apiKey = ApiKey.OpenAI "your_api_key_here" -- or ApiKey.Anthropic "your_api_key_here"
, openAIModel = "gpt-4-1106-preview"
, anthropicModel = "claude-2"
, discordBotToken = "your_discord_bot_token_here"
, debugGuildId = "your_guild_id_here"
-- You don't need to change the postgres values but for security
-- you should change the password here and in `./docker-compose.yml`
, postgresHost = "localhost"
, postgresPort = 15432
, postgresUser = "admin"
, postgresPassword = "admin"
, postgresDb = "admin"
}
