module Ch26.Morra where

import           Control.Monad.Trans.State
import           System.Random
import           Control.Monad.IO.Class

data Round = Round {
    guesses :: (Int, Int),
    shownFingers :: Int
}
data GameState = GameState {
    score :: (Int, Int),
    rounds :: [Round]
}

type Morra = StateT GameState IO

initialState :: GameState
initialState = GameState (0, 0) []


askAndGenerateRandomAnswer :: String -> (Int, Int) -> IO (Int, Int)
askAndGenerateRandomAnswer prompt range = do
    print prompt
    playerInput <- getLine >>= return . read
    gen         <- getStdGen
    let (computerInput, newGen) = randomR range gen
    setStdGen newGen
    return (playerInput, computerInput)


getFingers :: IO (Int, Int)
getFingers =
    askAndGenerateRandomAnswer "Enter the amount of fingers to show: " (0, 5)


getGuesses :: IO (Int, Int)
getGuesses = askAndGenerateRandomAnswer "Guess the total of fingers: " (0, 10)


evaluateGuesses :: Int -> Int -> Int -> Morra ()
evaluateGuesses total humanGuess computerGuess = do
    (GameState (humanPoints, computerPoints) r) <- get
    let newScore =
            ( if humanGuess == total then humanPoints + 1 else humanPoints
            , if computerGuess == total then computerPoints + 1 else humanPoints
            )
    put (GameState newScore (Round (humanGuess, computerGuess) total : r))

gameLoop :: Morra ()
gameLoop = do
    (playerFingers, computerFingers) <- liftIO getFingers
    (playerGuess  , computerGuess  ) <- liftIO getGuesses
    evaluateGuesses (playerFingers + computerFingers) playerGuess computerGuess
    liftIO $ print ("Fingers shown: " ++ show (playerFingers + computerFingers))
    liftIO $ print
        ("Human guessed " ++ show (playerGuess) ++ ", computer guessed " ++ show
            (computerGuess)
        )


main :: IO ()
main = do
    ((), GameState (humanResults, computerResults) _) <- runStateT
        gameLoop
        initialState
    print ("Human: " ++ show humanResults)
    print ("Computer: " ++ show computerResults)
    print "Game Over"
