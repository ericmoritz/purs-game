module Game
  ( GameState(..)
  , BattleResult(..)
  , Command(..)
  , Monster(..)
  , Player(..)
  , Room(..)
  , RoundResult(..)
  , Stats(..)
  , currentRoom
  , generateWorld
  , runRound
  ) where

import Prelude
import Control.Monad.RWS (RWS, RWSResult(..), runRWS)
import Control.Monad.State.Trans (get, modify_)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), head, (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\), type (/\))
import Random.LCG (Seed, lcgNext, unSeed)

type GameState
  = { seed :: Seed
    , player :: Player
    , rooms :: List Room
    }

newtype Player
  = Player Stats

-- Data Types
derive instance genericPlayer :: Generic Player _

instance showPlayer :: Show Player where
  show = genericShow

newtype Monster
  = Monster Stats

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

--| `generateWorld` generates a random dungeon
generateWorld :: Seed -> GameState
generateWorld seed = case runGame _generateWorld (emptyState seed) of
  state /\ _ -> state

--| `currentRoom` returns the current room if there any left or Nothing if the player has won
currentRoom :: GameState -> Maybe Room
currentRoom { rooms } = head rooms

data RoundResult
  = NextRoomResult
  | RunFailedResult
  | RunSucceededResult
  | NoRoomResult
  | AttackingAirResult
  | BattleEnsuedResult BattleResult
  | ExitResult

--| `runRound` runs a command in the current room
runRound :: Command -> GameState -> GameState /\ RoundResult
runRound cmd state = runGame (interpret cmd) state

-- internal
type Game
  = RWS Unit Unit GameState

runGame :: forall a. Game a -> GameState -> GameState /\ a
runGame game currentState = case runRWS game unit currentState of
  RWSResult state x _ -> state /\ x

interpret :: Command -> Game RoundResult
interpret cmd = do
  state <- get
  let
    mbRoom = currentRoom state
  let
    mbMonster = do
      room <- mbRoom
      room.monster
  case cmd /\ mbRoom /\ mbMonster of
    Leave /\ _ /\ _ -> do
      pure ExitResult
    _ /\ Nothing /\ _ -> do
      pure NoRoomResult
    Attack /\ Just _ /\ Nothing -> do
      pure AttackingAirResult
    Attack /\ Just _ /\ Just monster -> do
      -- player attacks the monster
      playerHit /\ player /\ mbMonster' <- playerAttack state.player monster

      -- monster attacks if there is a monster left
      monsterHit /\ player' <- case mbMonster' of
        Nothing -> pure $ false /\ player
        Just monster' -> monsterAttacks monster' player

      modify_ $ updatePlayer player'
      modify_ $ updateMonster mbMonster'
      pure $ BattleEnsuedResult $ { playerHit, monsterHit }
    Run /\ Just _ /\ Nothing -> do
      modify_ popRoom
      pure NextRoomResult
    Run /\ Just _ /\ Just monster -> do
      monsterHit /\ player' <- monsterAttacks monster state.player
      modify_ $ updatePlayer player'
      case monsterHit of
        true -> pure RunFailedResult
        false -> do
          modify_ popRoom
          pure RunSucceededResult

popRoom :: GameState -> GameState
popRoom s@{ rooms: Nil } = s

popRoom s@{ rooms: (_ : xs) } = s { rooms = xs }

updatePlayer :: Player -> GameState -> GameState
updatePlayer = flip $ _ { player = _ }

updateMonster :: Maybe Monster -> GameState -> GameState
updateMonster m s = let
  rooms = mapHead (_ { monster = m }) s.rooms
  in s { rooms = rooms }

_generateWorld :: Game Unit
_generateWorld = do
  p <- mkPlayer
  modify_ (updatePlayer p)
  go 10
  where
  updateRooms r s = s { rooms = (r : s.rooms) }

  go 0 = pure unit

  go n = do
    r <- mkRoom
    modify_ (updateRooms r)
    go (n - 1)

emptyState :: Seed -> GameState
emptyState s = { seed: s, player: Player emptyStats, rooms: Nil }

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

roll :: Game Int
roll = do
  state <- get
  let
    seed' = lcgNext state.seed
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
  if n < 10 then
    pure Nothing
  else do
    --_give the monster random gold
    stats <- (_ { gold = _ }) <$> rollStats <*> rollD100
    pure $ Just $ Monster stats

rollStats :: Game Stats
rollStats = emptyStats { hp = _, atk = _, dex = _ } <$> rollD100 <*> rollD20 <*> rollD20

mkRoom :: Game Room
mkRoom = do
  monster <- mkMonster
  pure $ { monster }

mkPlayer :: Game Player
mkPlayer = Player <$> rollStats

type BattleResult
  = { playerHit :: Boolean, monsterHit :: Boolean }

playerAttack :: Player -> Monster -> Game (Boolean /\ Player /\ Maybe Monster)
playerAttack (Player player) (Monster monster) = do
  playerAtk <- (_ + player.atk) <$> rollD20
  monsterDodge <- (_ + monster.dex) <$> rollD20
  let
    playerHit = playerAtk > monsterDodge
  let
    monsterHP =
      if playerHit then
        max 0 $ monster.hp - playerAtk
      else
        monster.hp
  let
    playerGold =
      if monsterHP == 0 then
        player.gold + monster.gold
      else
        player.gold
  let
    monster' =
      if monsterHP == 0 then
        Nothing
      else
        Just <<< Monster $ monster { hp = monsterHP }
  let
    player' = Player $ player { gold = playerGold }
  pure $ playerHit /\ player' /\ monster'

monsterAttacks :: Monster -> Player -> Game (Boolean /\ Player)
monsterAttacks (Monster monster) (Player player) = do
  monsterAtk <- (_ + monster.atk) <$> rollD20
  playerDodge <- (_ + player.dex) <$> rollD20
  let
    monsterHit = monster.hp > 0 && monsterAtk > playerDodge
  let
    playerHP =
      if monsterHit then
        max 0 $ player.hp - monsterAtk
      else
        player.hp
  pure $ monsterHit /\ Player (player { hp = playerHP })

mapHead :: forall a. (a -> a) -> List a -> List a
mapHead _ Nil = Nil

mapHead f (x : xs) = f x : xs
