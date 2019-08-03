module Ch26.Morra where

import           Control.Monad.Trans.State
import           Control.Monad
import           Data.Map.Strict               as M

type PlayerScore = Int
data Player = Player {playerName :: String, score :: PlayerScore}
type GameState = StateT Int IO [Player]


gameLoop :: GameState -> IO ()
gameLoop state = do

    

main :: IO ()
main = do
    print "Enter first players name: "
    firstPlayerName <- readLn
    print "Enter second players name: "
    secondPlayerName <- readLn
    let firstPlayer  = Player 0 firstPlayerName
        secondPlayer = Player 0 secondPlayerName
        playerScores = [firstPlayer, secondPlayer]
        forever $ gameLoop $ stateT
