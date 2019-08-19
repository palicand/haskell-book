module Ch26.Morra where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           System.Random
import           Control.Monad.IO.Class


type Fingers = Int
type Guess = Int
type Score = Int

data Round = Round {
    guesses :: (Guess, Guess),
    shownFingers :: Fingers
}
data GameState = GameState {
    score :: (Score, Score),
    rounds :: [Round]
}


data GameMode = PvAI | PvP

type GameReader = ReaderT GameMode IO

type Morra = StateT GameState GameReader


initialState :: GameState
initialState = GameState (0, 0) []


winningScore :: Score
winningScore = 3


askFor :: (Read a) => String -> IO a
askFor prompt = do
    print prompt
    getLine >>= return . read

generateRandomAnswer :: (Random a) => (a, a) -> IO a
generateRandomAnswer range = do
    gen <- getStdGen
    let (computerInput, newGen) = randomR range gen
    setStdGen newGen
    return computerInput


getSecondPlayerAnswer
    :: (Random a, Read a) => GameMode -> String -> (a, a) -> IO a
getSecondPlayerAnswer PvAI _      range = generateRandomAnswer range
getSecondPlayerAnswer PvP  prompt _     = askFor prompt


getFingers :: GameReader (Fingers, Fingers)
getFingers = do
    let prompt = "How many fingers do you want to show: "
    player1Guess <- liftIO $ askFor prompt
    mode         <- ask
    player2Guess <- liftIO $ getSecondPlayerAnswer mode prompt (0, 5)
    return (player1Guess, player2Guess)



getGuesses :: Fingers -> GameReader (Guess, Guess)
getGuesses computerFingers = do
    let prompt = "Enter your guess:"
    player1Guess <- liftIO $ askFor prompt
    mode         <- ask
    player2Guess <- liftIO $ getSecondPlayerAnswer
        mode
        prompt
        (computerFingers, computerFingers + 5)
    return (player1Guess, player2Guess)


evaluateGuesses :: Fingers -> Guess -> Guess -> GameState -> GameState
evaluateGuesses total humanGuess computerGuess (GameState (humanPoints, computerPoints) r)
    = GameState newScore (Round (humanGuess, computerGuess) total : r)  where
    newScore =
        ( if humanGuess == total then humanPoints + 1 else humanPoints
        , if computerGuess == total then computerPoints + 1 else humanPoints
        )


gameLoop :: Morra ()
gameLoop = do
    (playerFingers, computerFingers) <- lift getFingers
    (playerGuess  , computerGuess  ) <- lift $ getGuesses computerFingers
    oldState                         <- get
    let newState = evaluateGuesses (playerFingers + computerFingers)
                                   playerGuess
                                   computerGuess
                                   oldState
    put newState
    liftIO $ putStrLn
        ("Fingers shown: " ++ show (playerFingers + computerFingers))
    liftIO $ putStrLn
        (  "Human guessed "
        ++ show playerGuess
        ++ ", computer guessed "
        ++ show computerGuess
        )
    let (humanScore, computerScore) = score newState
    liftIO $ putStrLn ("Human: " ++ show humanScore)
    liftIO $ putStrLn ("Computer: " ++ show computerScore)
    unless (humanScore == winningScore || computerScore == winningScore)
           gameLoop


askForGameMode :: IO (Maybe GameMode)
askForGameMode = do
    liftIO $ putStrLn
        "Enter 1 to play against AI or 2 to play against another player"
    opt <- getLine >>= return . read
    case opt of
        "1" -> return $ Just PvAI
        "2" -> return $ Just PvP
        _   -> return Nothing


morra :: IO ()
morra = do
    gameMode <- askForGameMode
    case gameMode of
        Nothing     -> putStrLn "Incorrect game mode"
        (Just mode) -> do
            ((), GameState (humanResults, computerResults) _) <- runReaderT
                (runStateT gameLoop initialState)
                mode
            case compare humanResults computerResults of
                EQ -> putStrLn "Draw!"
                LT -> putStrLn "The computer wins!"
                GT -> putStrLn "Congratulation, human wins!"
            putStrLn "Game Over"
