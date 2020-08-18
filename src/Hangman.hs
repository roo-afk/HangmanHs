import System.IO
import System.Directory
import System.Random
import Data.Char
import Data.List

hangmanImages :: [[String]]
hangmanImages =
    --map (++ ["\n"])
    transpose 
    [ [ "   ", " O ", " O ", " O ", " O " , "_O " , "_O_"  ] , 
    [ "   ", "   ", " | ", " | ", " | " , " | " , " | "  ], 
    [ "   ", "   ", "   ", "/  ", "/ \\", "/ \\", "/ \\" ]]

putState :: [String] -> IO ()
putState = mapM_ putStrLn

len = length hangmanImages

getState n = (!! (len - n - 1))
main :: IO ()
main = do 
  putStrLn "Hangman"
  word <- getWordFromFile "animals.txt"
  let puzzle = assoc word
  loop puzzle (len - 1) --(length word)

getWordFromFile :: FilePath -> IO String
getWordFromFile fileName = do
    contents <- readFile fileName
    gen <- newStdGen
    let word = getSecretWord gen (map toLowerCase $ lines contents)
    return word

getSecretWord :: StdGen -> [String] -> String
getSecretWord gen xs =
    let (randIndex, _) = randomR (0, length xs - 1) gen :: (Int, StdGen)
     in xs !! randIndex

assoc :: String -> [(Char, Char)]
assoc = map (\x -> (x, '*'))

updateWithGuess :: Char -> [(Char, Char)] -> [(Char, Char)]
updateWithGuess c = map (\(x,y) -> if x == c then (x, x) else (x, y))

toLowerCase :: String -> String
toLowerCase = map toLower

makeGuess :: [(Char, Char)] -> String -> Char -> Int -> IO ()
makeGuess wordMap word letter guesses
    | elem letter word = let nextPuzzle = updateWithGuess letter wordMap in if word == (map snd nextPuzzle) then putStrLn "You win" else loop nextPuzzle guesses                                        
    | otherwise = loop wordMap (guesses-1)

loop :: [(Char, Char)] -> Int -> IO ()
loop puzzle 0 = 
    (putState $ getState 0 hangmanImages)
    >>
    putStrLn ("You've lost, the word was " ++ map fst puzzle)
loop puzzle n = do
  let current = map snd puzzle
  putStrLn "Puzzle is: "
  putState $ getState n hangmanImages
  putStrLn ("Current guesses: " ++ current)
  putStrLn ("numGuesses = " ++ show n) 
  putStrLn "Next guess: "
  hFlush stdout
  (c:_) <- getLine
  let word = map fst puzzle
  makeGuess puzzle word c n
 
