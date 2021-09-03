module Main where

import Prelude

import Control.Monad.RWS (RWS, RWSResult(..), runRWS)
import Control.Monad.State.Trans (get, modify_)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Node.Process (exit)
import Node.ReadLine (Completer, Interface, createConsoleInterface, question)
import Random.LCG (Seed, lcgNext, randomSeed, unSeed)

-- Data Types

type Log = Unit

type Env
  = {}

type GameState
  = { seed :: Seed
    , player :: Player
    , rooms :: List Room
    }

emptyState :: Seed -> GameState
emptyState s = { seed: s, player: Player emptyStats, rooms: Nil}

type Stats
  -- hp is the number of hit points a being has
  = { hp :: Int
  -- atk is the 1-20 chance of hitting
    , atk :: Int
  -- dex is the 1-20 chance of dodging or running away
    , dex :: Int
    , gold :: Int
    }

emptyStats :: Stats
emptyStats = { hp: 0, atk: 0, dex: 0, gold: 0 }

newtype Monster
  = Monster Stats

newtype Player
  = Player Stats

derive instance genericPlayer :: Generic Player _

instance showPlayer :: Show Player where
  show = genericShow

derive instance genericMonster :: Generic Monster _

instance showMonster :: Show Monster where
  show = genericShow

type Room
  = { monster :: Maybe Monster }

-- Commands
data Command
  = Attack
  | Run
  | Leave

parseCommand :: String -> Either String Command
parseCommand = case _ of
  "attack" -> Right Attack
  "next" -> Right Run
  "leave" -> Right Leave
  _ -> Left "unknown command"

data GameResult
  = NoopResult
  | AttackingAirResult
  | BattleEnsuedResult BattleResult
  | ExitResult

-- Game values

type Game
  = RWS Env Log GameState

roll :: Game Int
roll = do
  state <- get
  let seed' = lcgNext state.seed
  modify_ (_ { seed = seed' })
  pure $ unSeed seed'

rollDice :: Int -> Game Int
rollDice sides = do
  n <- roll
  pure $ n `mod` sides + 1

rollD100 :: Game Int
rollD100 = rollDice 100

rollD20 :: Game Int
rollD20 = rollDice 20

mkMonster :: Game (Maybe Monster)
mkMonster = do
  n <- rollD20
  if n < 10
    then
      pure Nothing
    else
      do
        --_give the monster random gold
        stats <- (_ { gold = _ }) <$> rollStats <*> rollD100
        pure $ Just $ Monster stats

rollStats :: Game Stats
rollStats = emptyStats {hp=_, atk=_, dex=_} <$> rollD100 <*> rollD20 <*> rollD20

mkRoom :: Game Room
mkRoom = do
  monster <- mkMonster
  pure $ { monster }

mkPlayer :: Game Player
mkPlayer = Player <$> rollStats

generateWorld :: Game Unit
generateWorld = do
    p <- mkPlayer
    modify_ (updatePlayer p)
    go 10
  where
    updatePlayer p = _ { player=p }
    updateRooms r s = s { rooms=(r:s.rooms) }
    go 0 = pure unit

    go n = do
      r <- mkRoom
      modify_ (updateRooms r)
      go (n - 1)

type BattleResult
  = { playerHit :: Boolean, monsterHit :: Boolean }

battle :: Player -> Monster -> Game BattleResult
battle (Player player) (Monster monster) = do
  playerAtk <- (_ + player.atk) <$> rollD20
  playerDodge <- (_ + player.dex) <$> rollD20
  monsterAtk <- (_ + monster.atk) <$> rollD20
  monsterDodge <- (_ + monster.dex) <$> rollD20

  let playerHit = playerAtk > monsterDodge
  let monsterHP = if playerHit
    then max 0 $ monster.hp - playerAtk
    else monster.hp

  let playerGold = if monsterHP == 0
    then player.gold + monster.gold
    else player.gold

  let monsterHit = monsterHP > 0 && monsterAtk > playerDodge
  let playerHP = if monsterHit
    then max 0 $ player.hp - monsterAtk
    else player.hp

  let monster' = if monsterHP == 0
    then Nothing
    else Just <<< Monster $ monster { hp = monsterHP }

  let player' = Player $ player { hp = playerHP, gold = playerGold }

  modify_ $ _ { player = player' }
  modify_ $ updateRoomMonster monster'
  pure { playerHit, monsterHit }

  where
    updateRoomMonster :: Maybe Monster -> GameState -> GameState
    updateRoomMonster m s = 
      let
        rooms = mapHead (_ { monster = m }) s.rooms
      in
        s { rooms = rooms }

mapHead :: forall a. (a -> a) -> List a -> List a
mapHead _ Nil = Nil
mapHead f (x:xs) = f x : xs

interpret :: Room -> Command -> Game GameResult
interpret room cmd = case cmd of
    Leave ->
      pure ExitResult
    Attack -> do
      case room.monster of
        Nothing -> do
          pure AttackingAirResult
        Just monster -> do
          state <- get
          result <- battle state.player monster
          pure $ BattleEnsuedResult result
    Run -> do
      -- TODO
      pure NoopResult



--_Effects
commandCompleter :: Completer
commandCompleter _ = pure { completions: ["attack", "run", "leave"], matched: ""}

displayCurrentState :: GameState -> Effect Unit
displayCurrentState s = do
  let (Player playerStats) = s.player
  case s.rooms of
    Nil -> log "You win!"
    (r:_) -> case r.monster of
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

initializeState :: Env -> Effect GameState
initializeState env = do
  seed <- randomSeed
  let
    RWSResult state _ _ = runRWS (generateWorld) env (emptyState seed)
  pure $ state

loop :: Interface -> Env -> GameState -> Effect Unit
loop _ _ { rooms: Nil } = do
  log "You win!"
  exit 0
loop interface env currentState@{ rooms: (room:_) } = do
    displayCurrentState currentState
    question (gamePrompt currentState) handler interface
  where
    handler s = do
      case parseCommand s of
        Left err -> do
          log err
          loop interface env currentState
        Right cmd -> do
          let RWSResult state result _ = runRWS (interpret room cmd) env currentState

          renderResult result state
          loop interface env state

renderResult :: GameResult -> GameState -> Effect Unit
renderResult AttackingAirResult _ = do
  log "You swing wildly at the air"
renderResult (BattleEnsuedResult { playerHit, monsterHit }) _ = do
  when playerHit $ log $ "Player hit!"
  when monsterHit $ log $ "Monster hit!"
renderResult NoopResult _ = do
  pure unit
renderResult ExitResult _ = do
  log "Goodbye"
  exit 0 # void

main :: Effect Unit
main = do
  let env = { }
  log "Generating world..."
  state <- initializeState env
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
  loop interface env state
