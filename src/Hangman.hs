

import System.IO
import System.Random
import Data.Char (toLower, isLetter)
import Data.List (break, transpose)



splitLine :: (a -> Bool) -> [a] -> [[a]]
splitLine _ [] = []
splitLine pred s = -- ex. pred = (== ' ')
  let s'          = dropWhile pred s --drop characters that match pred
      (word, s'') = break pred s' -- same as span (not . pred), and it gets characters until one matches the pred
    in word : splitLine pred s'' 

logo :: [String]
logo = 
 ["--------------------------------------------\n",
 "| #  #   #   #   #  #### #   #   #   #   # |\n",
 "| #  #  # #  ##  # #     ## ##  # #  ##  # |\n",
 "| #### ##### # # # #  ## # # # ##### # # # |\n",
 "| #  # #   # #  ## #   # #   # #   # #  ## |\n",
 "| #  # #   # #   #  ###  #   # #   # #   # |\n",
 "--------------------------------------------\n\n"]

results :: [String]
results = 
 ["---------------\n",
  "--- Results ---\n",
  "---------------\n\n"
  ]



mainMessage = ["Welcome to the game Hangman!\n\n",
 "The objective in this game is to guess the word.\n",
 "You can enter both uppercase and lowercase letters.\n",
 "If you think you know the word, you can type it in.\n",
 "You will lose if you have guessed 7 letters wrong.\n\n",
 "This is the word you need to guess: "] -- fix this later



showStringList = mapM_

hangmanImages :: [[String]]
hangmanImages =
    map (++ ["\n"])
    $ transpose 
     [ [ "   ", " O ", " O ", " O ", " O " , "_O " , "_O_", "_X_" ] , 
     [ "   ", "   ", " | ", " | ", " | " , " | " , " | "  , " | "], 
     [ "   ", "   ", "   ", "/  ", "/ \\", "/ \\", "/ \\" , "/ \\"]]



len = length hangmanImages


getState n = (!! (len - n - 1))

main :: IO ()
main = do 
  hSetBuffering stdin NoBuffering -- ghci defaults to NoBuffering, but running compiled code from ghc in the terminal seems to default to LineBuffering
  hSetBuffering stdout NoBuffering
  putStr "Does the file contain \"|\" or newlines? "
  c <- getLine
  word <- getWordFromFile "words.txt" (== c)
  putStrLn ("\nguessWord = " ++ word)
  showStringList putStr logo
  showStringList putStr mainMessage
  let puzzle = assoc word
  putStrLn (getValues puzzle ++ "\n")
  loop puzzle word (len - 1) 0

getWordFromFile :: FilePath -> IO String
getWordFromFile fileName p = 
  readFile fileName 
  >>=
  (\contents -> newStdGen >>= (return . flip getSecretWord {-. map (map toLower) . -}(splitLine p contents)))
  -- uncomment contents of {- -} if file contains any uppercase letters 

getSecretWord :: StdGen -> [String] -> String
getSecretWord gen xs =
    let (randIndex, _) = randomR (0, length xs - 1) gen :: (Int, StdGen)
     in xs !! randIndex

assoc :: String -> [(Char, Char)]
assoc = map (\x -> (x, '.'))

updateWithGuess :: Char -> [(Char, Char)] -> [(Char, Char)]
updateWithGuess c = map (\(x,y) -> if x == c then (x, x) else (x, y))



getValues :: [(a, b)] -> [b]
getValues = (snd <$>)

printWin = (showStringList putStr results) >> putStrLn "Congratulations you guessed the right word!"
makeGuess :: [(Char, Char)] -> String -> Char -> Int -> Int -> IO ()
makeGuess wordMap word '\n' guesses counter = loop wordMap word guesses counter --error "Newline was detected" --
makeGuess wordMap word l guesses counter
    | elem l word = 
        let nextPuzzle = updateWithGuess l wordMap 
            guessed = getValues nextPuzzle
         in putStrLn ("That letter was correct\n") 
            >> 
            putStrLn ("The word including the letters you guessed: " ++ show guessed ++ "\n")
            >>
            loop nextPuzzle word guesses (counter + 1)                                   
    | otherwise = 
        putStrLn ("That letter was incorrect\n") 
        >> 
        putStrLn ("The word including the letters you guessed: " ++ show guessed ++ "\n") 
        >> 
        loop wordMap word (guesses-1) (counter + 1)
          where guessed = getValues wordMap

loop :: [(Char, Char)] -> String -> Int -> Int -> IO ()
loop puzzle _ _ _ | notElem '.' guessed = 
   printWin where guessed = getValues puzzle
loop _ word 0 _ = 
  (showStringList putStr results) >> (showStringList putStrLn $ getState 0 hangmanImages) >> putStrLn ("You guessed the wrong word. The word was " ++ word ++ ". better luck next time!") 
loop puzzle word n counter = do
    putStrLn ("Amount of wrong letters: " ++ show (len - n - 1) ++ "\n")  
    showStringList putStrLn $ getState n hangmanImages
    putStr (show counter ++ ".    Enter the letter(s) you want to guess: ")
    --(c:_) <- getLine -- do this to ignore newline
    c <- getChar
    putChar '\n'
    if c == '\n' || isLetter c
       then makeGuess puzzle word (toLower c) n counter 
       else do 
             putStrLn "Only alphanumeric symbols are allowed (a-z, A-Z), try again:\n"
             putStrLn ("The word including the letters you guessed: " ++ getValues puzzle ++ "\n") 
             loop puzzle word n counter
  
  


 
