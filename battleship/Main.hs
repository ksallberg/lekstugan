{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

{-
Board exhausted example:
mmmmmmmmsm
msmsmmmmmm
msmmmmssmm
msmmsmmmmm
msmmsmsmmm
mmmmmmmmmm
mmsmmmmmmm
mmsmmmsssm
mmsmmmmmmm
mmmsssmmmm

Perfect hits example:
~~~~~~~~s~
~s~s~~~~~~
~s~~~~ss~~
~s~~s~~~~~
~s~~s~s~~~
~~~~~~~~~~
~~s~~~~~~~
~~s~~~sss~
~~s~~~~~~~
~~~sss~~~~
-}

module Main where

import Control.Concurrent
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import Data.Maybe
import GHC.Generics
import Network.HTTP.Conduit
import System.Random
import Data.ByteString.Lazy.Char8 (ByteString)

data RequestData = RequestData
    { player_name :: String
    , shoot_at    :: Maybe [Int]
    } deriving (Show, Generic)

data ResponseData = ResponseData
    { status :: String
    } deriving (Show, Generic)

data Radar = Radar
    { board :: Board
    , won   :: String
    } deriving (Generic)

data Reset = Reset
    { password :: String
    } deriving (Show, Generic)

data State = State
    { shots  :: [[Int]]
    , player :: RequestData
    } deriving (Show)

type PlayerName = String
type Route      = String
type Board      = [String]

instance FromJSON ResponseData
instance FromJSON Radar
instance ToJSON   Reset

instance Show Radar where
    show (Radar board won) = "Radar feedback\n" ++
                             "Won? " ++ (show won) ++ "\n" ++
                             (concat [line ++ "\n" | line <- board])

instance ToJSON RequestData where
    toJSON (RequestData name Nothing)     = object ["player_name" .= name]
    toJSON (RequestData name (Just shot)) = object ["player_name" .= name,
                                                    "shoot_at"    .= shot]

hostName :: String
hostName = "http://192.168.1.89:28251"

main :: IO ()
main = do
    let user = RequestData{player_name = "berra", shoot_at = Nothing}
        initialState = State{shots = [], player = user}
--    reset
    regPlayer <- signup user
    putStrLn $ show regPlayer
    gameLoop initialState

getInt :: IO Int
getInt = do
    s <- getLine
    return (read s)

gameLoop :: State -> IO ()
gameLoop state@(State shots player) = do
    rad1 <- radar player
    case (hasHit $ board rad1) of
        False ->
            do let (x,y) = firstWater (board rad1)
                   shot  = RequestData{player_name = player_name player,
                                       shoot_at = Just [x, y]}
               sht   <- shoot shot
               putStrLn $ show sht
               radar <- radar player
               putStrLn $ show radar
               gameLoop $ state{shots = shots ++ [[x, y]]}
        True -> putStrLn "hej"

firstWater :: Board -> (Int, Int)
firstWater board = let ls     = map (\x->(elem '~' x, x)) board
                       row    = first ls 0
                       column = elemIndex '~' (board !! row)
                   in case (row == -1 || isNothing column) of
                       True  -> (0, 0)
                       False -> (fromJust column, row)

first :: [(Bool, String)] -> Int -> Int
first []       num = (-1)
first ((True, _):xs) num = num
first (x:xs)   num = first xs (num+1)

hasHit :: Board -> Bool
hasHit b = elem 'h' (concat b)

signup :: RequestData -> IO ResponseData
signup user = do body <- sendAndReadResponse user "/battleship/register/"
                 return $ fromJust ((decode body) :: Maybe ResponseData)

shoot :: RequestData -> IO ResponseData
shoot user = do body <- sendAndReadResponse user "/battleship/shoot/"
                return $ fromJust ((decode body) :: Maybe ResponseData)

radar :: RequestData -> IO Radar
radar user = do body <- sendAndReadResponse user "/battleship/radar/"
                return $ fromJust ((decode body) :: Maybe Radar)

reset :: IO ()
reset = makeRequest (hostName++"/battleship/reset/")
                    (encode Reset{password = "pretty please"})
          >>= \_ -> return ()

sendAndReadResponse :: RequestData -> Route -> IO ByteString
sendAndReadResponse req route =
    do response <- makeRequest (hostName++route) (encode req)
       return $ responseBody response

makeRequest :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) =>
               String -> ByteString -> m (Response ByteString)
makeRequest url reqBody =
    do req <- parseUrl url
       withManager $ httpLbs req
                  { method      = "POST"
                  , requestBody = RequestBodyLBS reqBody
                  }
