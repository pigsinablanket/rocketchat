# rocketchat-api

Client side API for communicating with a Rocket.Chat server, in Haskell.

## Building

Current GHC version is `8.10.1`. [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/) is an easy way to set versions.

### Building RocketChat

`cabal build lib:rocketchat`

### Building RocketChat-Polysemy

`cabal build lib:rocketchat-polysemy`

### Building RocketChat-mtl

`cabal build lib:rocketchat-mtl`

## Making Rocket.Chat bot user

Instructions for creating a bot user can be found at the official documentation [https://docs.rocket.chat/guides/bots/create-and-run-a-bot](https://docs.rocket.chat/guides/bots/create-and-run-a-bot)
