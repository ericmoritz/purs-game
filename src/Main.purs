module Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Game (Command(..), GameState, Monster(..), Player(..), RoundResult(..), currentRoom, generateWorld, runRound)
import Node.Process (exit)
import Node.ReadLine (Completer, Interface, createConsoleInterface, question)
import Random.LCG (randomSeed)

parseCommand :: String -> Either String Command
parseCommand = case _ of
  "attack" -> Right Attack
  "run" -> Right Run
  "leave" -> Right Leave
  _ -> Left "unknown command"

commandCompleter :: Completer
commandCompleter _ = pure { completions: ["attack", "run", "leave"], matched: ""}

renderCurrentState :: GameState -> Effect Unit
renderCurrentState s = do
  let (Player playerStats) = s.player
  case currentRoom s of
    Nothing -> do
      log "You've won!"
    Just room -> case room.monster of
      Nothing -> do
        log "There's nothing here"
      Just (Monster monsterStats) -> do
        log $ "There's a monster here!"
        log $ "Your HP: " <> show playerStats.hp
        log $ "Their HP: " <> show monsterStats.hp
        log ""

gamePrompt :: GameState -> String
gamePrompt state =
  "HP: " <> show stats.hp <> " / Gold: " <> show stats.gold <> " λ. "
  where
    stats = case state.player of (Player s) -> s

loop :: Interface ->  GameState -> Effect Unit
loop interface currentState = do
      renderCurrentState currentState
      question (gamePrompt currentState) handler interface
    where
      handler s = do
        case parseCommand s of
          Left err -> do
            log err
            loop interface currentState
          Right cmd -> do
            let state /\ result = runRound cmd currentState
            let (Player player) = state.player
            when (player.hp == 0) do
              log "You died"
              exit 0
            renderResult result state
            loop interface state

renderResult :: RoundResult -> GameState -> Effect Unit
renderResult NoRoomResult _ = do
  log "You can't do that because you're not in a room"
renderResult AttackingAirResult _ = do
  log "You swing wildly at the air"
renderResult (BattleEnsuedResult { playerHit, monsterHit }) _ = do
  if playerHit
    then log "You hit!"
    else log "You miss"
  if monsterHit
     then log "Monster hit!"
     else log "Monster miss"
renderResult ExitResult _ = do
  log "Goodbye"
  exit 0 # void
renderResult NextRoomResult _ = do
  log "You go to the next room"
renderResult RunFailedResult _ = do
  log "You failed to escaped and the monster hit you"
renderResult RunSucceededResult _ = do
  log "You escaped to the next room!"

main :: Effect Unit
main = do
  log "Generating world..."
  seed <- randomSeed
  let state = generateWorld seed
  log ""
  log "You have entered a dungeon. You don't know how long the dungeon is."
  log "Your goal is to gather as much gold as possible without dying"
  log "You can do the following actions:"
  log " - attack: Attack the current monster if there is any"
  log " - run: Try to run to the next room, if the monster stops you, they will attack"
  log " - leave: Take your winnings and go home"
  log ""
  log "Good luck!"
  log ""
  interface <- createConsoleInterface commandCompleter
  loop interface state
