module Main where
  import Control.Monad (forever)
  import Data.Char (toLower)
  import Data.Maybe (isJust)
  import Data.List (intersperse)
  import System.Exit (exitSuccess)
  import System.Random (randomRIO)
  
  maxWrongGuesses :: Int
  maxWrongGuesses = 7

  type QtyWrongGuesses = Int
  data Puzzle = Puzzle String [Maybe Char] [Char] QtyWrongGuesses
  
  instance Show Puzzle where
    show (Puzzle _ discovered guessed wrongGuesses) =
      (intersperse ' ' $
      fmap renderPuzzleChar discovered)
      ++ " Guessed so far: " ++ guessed
      ++ " Remaining lives: " ++ (show (maxWrongGuesses - wrongGuesses))

  newtype WordList = WordList [String] deriving (Eq, Show)

  minWordLength :: Int
  minWordLength = 5
  maxWordLength :: Int
  maxWordLength = 9

  allWords :: IO WordList
  allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

  gameWords :: IO WordList
  gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
      where gameLength w =
              let l = length (w :: String)
              in l >= minWordLength && l < maxWordLength
    
  randomWord :: WordList -> IO String
  randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, (length wl) - 1 )
    return $ wl !! randomIndex
  
  randomWord' :: IO String
  randomWord' = gameWords >>= randomWord

  freshPuzzle :: String -> Puzzle
  freshPuzzle xs = Puzzle xs (map (\_ -> Nothing) xs) [] 0

  charInWord :: Puzzle -> Char -> Bool
  charInWord (Puzzle str _ _ _) c = elem c str

  alreadyGuessed :: Puzzle -> Char -> Bool
  alreadyGuessed (Puzzle _ _ guessed _) c = elem c guessed

  renderPuzzleChar :: Maybe Char -> Char
  renderPuzzleChar Nothing = '_'
  renderPuzzleChar (Just c) = c

  fillInCharacter :: Puzzle -> Char -> Puzzle
  fillInCharacter (Puzzle word filledInSoFar s wG) c = Puzzle word newFilledInSoFar (c : s) wG
    where zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
          newFilledInSoFar = zipWith (zipper c) word filledInSoFar
      
  handleGuess :: Puzzle -> Char -> IO Puzzle
  handleGuess puzzle@(Puzzle a b c d) guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
      (_, True) -> do
        putStrLn "You already guessed that\
                \ character, pick \
                \ something else!"
        return puzzle
      (True, _) -> do
        putStrLn "This character was in the\
                  \ word, filling in the word\
                  \ accordingly"
        return (fillInCharacter puzzle guess)
      (False, _) -> do
        putStrLn "This character wasn't in\
                  \ the word, try again."
        return (fillInCharacter (Puzzle a b c (d+1)) guess)
      
  gameOver :: Puzzle -> IO ()
  gameOver (Puzzle wordToGuess _ guessed wrongGuesses) =
    if wrongGuesses > maxWrongGuesses then
    do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
    else return ()

  gameWin :: Puzzle -> IO ()
  gameWin (Puzzle _ filledInSoFar _ _) =
    if all isJust filledInSoFar then
    do
      putStrLn "You win!"
      exitSuccess
    else return ()


  runGame :: Puzzle -> IO ()
  runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _ -> putStrLn "Your guess must\
                  \ be a single character"

  main :: IO ()
  main =
    do
      word <- randomWord'
      let puzzle = freshPuzzle (fmap toLower word)
      runGame puzzle
