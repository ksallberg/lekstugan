{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/type-families-and-pokemon
module Third where

class (Show pokemon, Show (Move pokemon)) => Pokemon pokemon where
  data Move pokemon :: *
  pickMove :: pokemon -> Move pokemon

data Fire = Charmander | Charmeleon | Charizard deriving Show
instance Pokemon Fire where
  data Move Fire = Ember | FlameThrower | FireBlast deriving Show
  pickMove Charmander = Ember
  pickMove Charmeleon = FlameThrower
  pickMove Charizard = FireBlast

data Water = Squirtle | Wartortle | Blastoise deriving Show
instance Pokemon Water where
  data Move Water = Bubble | WaterGun deriving Show
  pickMove Squirtle = Bubble
  pickMove _ = WaterGun

data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show
instance Pokemon Grass where
  data Move Grass = VineWhip deriving Show
  pickMove _ = VineWhip

printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pokemonOne moveOne pokemonTwo moveTwo winner = do
  putStrLn $ pokemonOne ++ " used " ++ moveOne
  putStrLn $ pokemonTwo ++ " used " ++ moveTwo
  putStrLn $ "Winner is: " ++ winner ++ "\n"

class (Show (Winner pokemon foe), Pokemon pokemon, Pokemon foe) =>
      Battle pokemon foe where
  type Winner pokemon foe :: *
  type Winner pokemon foe = pokemon

  battle :: pokemon -> foe -> IO ()
  battle pokemon foe = do
    printBattle (show pokemon) (show move) (show foe)
      (show foeMove) (show winner)
   where
    foeMove = pickMove foe
    move = pickMove pokemon
    winner = pickWinner pokemon foe

  pickWinner :: pokemon -> foe -> (Winner pokemon foe)

instance Battle Water Fire where
  pickWinner pokemon foe = pokemon

instance Battle Fire Water where
  type Winner Fire Water = Water
  pickWinner = flip pickWinner

instance Battle Grass Water where
  pickWinner pokemon foe = pokemon

instance Battle Water Grass where
  type Winner Water Grass = Grass
  pickWinner = flip pickWinner

instance Battle Fire Grass where
  pickWinner pokemon foe = pokemon

instance Battle Grass Fire where
  type Winner Grass Fire = Fire
  pickWinner = flip pickWinner

main :: IO ()
main = do
  battle Squirtle Charmander
  battle Charmeleon Wartortle
  battle Bulbasaur Blastoise
  battle Wartortle Ivysaur
  battle Charmeleon Ivysaur
  battle Venusaur Charizard
