import System.IO
import System.Random
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game 
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color



{-   -- TODO --

-}

{-
    Hangman (The random Word) (The correct guessed letters with index as key) (The faulty guessed letters)
-}
data Hangman = Hangman String [(Int,Char)] [Char]
    deriving (Show)

data World = World Scene Hangman

data Scene = Menu | Single | Multi | Exit

type Guess = String
type Word = String


numbOfGuesses :: Int
numbOfGuesses = 6

filepath = "wordlist.txt"

window :: Display
window = InWindow "Hangman" (1000 , 700) (10,10)

world :: World
world = World Menu (Hangman "" [] [])

background :: Color
background = white


splash :: IO ()
splash = do
    putStrLn "   ▄▄   ▄▄ ▄▄▄▄▄▄ ▄▄    ▄ ▄▄▄▄▄▄▄ ▄▄   ▄▄ ▄▄▄▄▄▄ ▄▄    ▄  \n\
              \  █  █ █  █      █  █  █ █       █  █▄█  █      █  █  █ █\n\
              \  █  █▄█  █  ▄   █   █▄█ █   ▄▄▄▄█       █  ▄   █   █▄█ █\n\
              \  █       █ █▄█  █       █  █  ▄▄█       █ █▄█  █       █\n\
              \  █   ▄   █      █  ▄    █  █ █  █       █      █  ▄    █\n\
              \  █  █ █  █  ▄   █ █ █   █  █▄▄█ █ ██▄██ █  ▄   █ █ █   █\n\
              \  █▄▄█ █▄▄█▄█ █▄▄█▄█  █▄▄█▄▄▄▄▄▄▄█▄█   █▄█▄█ █▄▄█▄█  █▄▄█\n\
              \     _______                                             \n\
              \     |     |                                             \n\ 
              \     |     O                                             \n\
              \     |    -|-                                            \n\
              \   __|__  J L                                            \n\
              \   |   |                                                 "
    putStrLn "Written by Erik Odhner, Edvard Axelman and Viktor Wallstén"

--hangmanGui :: Display -> Color -> (Picture -> Hangman -> Picture) -> IO ()

--guiMain :: IO ()
guiMain = play 
    window 
    background 
    20
    world 
    drawingFunc 
    menuSelector --inputHandler 
    updateFunc
    

-- If scene == Single then check for letter instead
menuSelector :: Event -> World -> World
menuSelector event world = case event of
        (EventKey (Char '1') Down _ _) -> World Single (Hangman "test" [] [])
        --2 -> Multi
        (EventKey (Char '3') Down _ _) -> World Exit (Hangman "" [] [])
        _ -> world




drawingFunc :: World -> Picture
drawingFunc (World scene h) = case scene of
        Menu    -> printScene (guiMenu)
        Single  -> printScene ([color green $ Circle 80])
        Multi   -> undefined--printScene multiplayer
        Exit    -> printScene (exitScene)
        _       -> printScene (guiMenu)



-- printScene (guiMenu)
printScene xs = pictures xs

reScale = scale 0.2 0.2

guiMenu = createPictureMenu menuList

exitScene = [reScale $ text "Bye Bye"]


createPictureMenu xs = foldl (\l x -> (translate 0 (realToFrac $ (-30) * (length l)) $ reScale $ color black $ text x) : l) [] xs

menuList = ["1. Singleplayer","2. Multiplayer","3. Quit Game"]

newText = text "TEST123"


updateFunc :: Float -> World -> World
updateFunc _ w = w
  where
    towardCenter :: Float -> Float
    towardCenter c = if abs c < 0.25
      then 0
      else if c > 0
        then c - 1.25
        else c + 1.25





inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) w = undefined
inputHandler _ w = w


















main :: IO ()
main = do
    splash
    menu

menu :: IO ()
menu = do
    putStrLn "--------------------"
    putStrLn "1. Singleplayer"
    putStrLn "2. Multiplayer"
    putStrLn "3. Quit game"
    putStrLn "--------------------"
    option <- getLine
    case option of
        "1" -> do singleGame
        "2" -> do multiGame
        "3" -> exit
        _   -> exit

exit :: IO ()
exit = do putStrLn "exited"

randomWord :: IO String
randomWord = do
        handle <- openFile filepath ReadMode -- Import wordlist
        contents <- hGetContents handle      -- Parse content in wordlist
        let list = lines contents            -- Creats a list of every word
            upper = length list              -- Creates a upper bound with the help of length of list
        ranInt <- randomRIO (1,upper-1)      -- Returns a random IO Int
        hClose handle                        -- closes handle
        let word = list !! ranInt            -- a random word from the list
            removedR = take (length word - 1) word -- removes the "\r" from the end of the word. For Linux OS only. 
        return removedR                     -- Returns a random word from the list with the help of the random number we get.


singleGame :: IO ()
singleGame =  do
    theWord <- randomWord
    let lengthWord = length theWord
        hangman = Hangman theWord [] []
    putStrLn ("Guess a letter or a string with " ++ show lengthWord ++ " letters\n")
    gameAux hangman


gameAux :: Hangman -> IO ()
gameAux hangman@(Hangman theWord correct guessed) = do
    if theWord == correctGuess correct -- check if you won
        then do
            win hangman
        else do
            if (numbOfGuesses - length guessed) <= 0 -- check if you have outrun your number of guesses
                then do
                    lose hangman
                else do
                    let underscore = foldl insertLetterinUnderscore (underscores (length theWord)) correct
                        guessesLeft = numbOfGuesses - length guessed
                    tree (guessesLeft)
                    putStrLn (underscore ++ "\n") -- print the current guessed word
                    --putStrLn ("the randomWord: " ++ theWord)
                    --putStrLn ("your guess so far: " ++ correctGuess correct)
                    putStrLn ("Bad guesses: " ++ guessed)
                    --putStrLn ("Guesses left: " ++ show guessesLeft)
                    newGuess <- getGuess hangman
                    if length newGuess == length theWord
                        then do
                            if newGuess == theWord
                                then do
                                    win hangman
                                else do
                                    let newHangman = Hangman theWord correct newGuess
                                    gameAux newHangman
                        else do    
                            if validGuess hangman newGuess
                                then do
                                    let newHangman = insertCorrectGuess hangman newGuess
                                    gameAux newHangman
                                else do
                                    putStrLn ("Incorrect, try another letter")
                                    let newHangman = insertWrongGuess hangman newGuess
                                    gameAux newHangman

getGuess :: Hangman -> IO String
getGuess hangman = do
    newG <- getLine
    if validInput newG hangman
        then return newG
        else do putStrLn "Bad guess, try again"
                getGuess hangman 

validInput :: String -> Hangman -> Bool
validInput "" hangman = False
validInput [x] hangman = True
validInput ls@(x:xs) (Hangman theWord _ _) = length ls == length theWord
 

underscores :: (Eq a, Num a) => a -> [Char]
underscores 1 = "_"
underscores x = '_' : ' ' : underscores (x-1)


{-  insertLetterinUnderscore xs (index,char)
    inserts a tuple (index,char) into the underscores xs
    RETURNS: String
    EXAMPLE: foldl (\l x -> insertLetterinUnderscore l x ) "_ _ _" [(0,'h'),(1,'e'),(2,'j')] == "h e j"
-}
insertLetterinUnderscore :: (Eq a, Num a) => [Char] -> (a, Char) -> [Char]
insertLetterinUnderscore [x] (0,char)           = [char]
insertLetterinUnderscore [x] (index, char)      = "_"
insertLetterinUnderscore (x:y:xs) (0, char)     = char : ' ' : insertLetterinUnderscore xs (-1, char)
insertLetterinUnderscore (x:y:xs) (index, char) = x : y : insertLetterinUnderscore xs (index-1, char)

printWord :: String -> String
printWord [x]    = [x]
printWord (x:xs) = x : ' ' : printWord xs 

correctGuess :: [(a, b)] -> [b]
correctGuess = map snd

multiGame :: IO ()
multiGame = do
    putStrLn "Input the word to guess: "
    getWord <- getLine
    gameAux (Hangman getWord [] [])

win :: Hangman -> IO ()
win (Hangman word correct guessed) = do
    putStrLn $ printWord word
    putStrLn "\nCongratulations! You won!\n"
    endgame

lose :: Hangman -> IO ()
lose (Hangman word _ guessed)
    | (length guessed) >= numbOfGuesses = do
                                    putStrLn $ "You lost!\n"
                                    tree 0  -- print the hangman
                                    putStrLn $ "The actual word was: \n" ++ printWord word
                                    endgame
    | otherwise                   = return ()
    --where wrongGuess = length guessed - length [x | x <- guessed, x `elem` word]


endgame :: IO ()
endgame = do
    putStrLn "Do you want play again? (y/n): "
    usrInp <- getLine
    when (usrInp == "yes" || usrInp == "y") main


validGuess hangman@(Hangman w _ _) [c] =  c `elem` w && (not $ alreadyGuessed hangman [c]) -- kollar om din gissning är i ordet

alreadyGuessed (Hangman _ k g) [c] = c `elem` g || c `elem` correctGuess k -- kollar om din gissning redan har gissats

insertWrongGuess (Hangman w k g) [c] = Hangman w k (g ++ [c])

insertCorrectGuess (Hangman w k g) [c] = Hangman w (insert) g
                                       where insert = foldl (\l x -> insertCinK [] l (x,c)) k (getIndex w c 0)

h1 = Hangman "test" [(1,'e')] "l"

{-  insertCinK ys xs (i,c)
    inserts (i,c) in xs according to the index i
    EXAMPLE: insertCinK [] [(0,'h'),(2,'j')] (1,'e') == [(0,'h'),(1,'e'),(2,'j')]
-}
insertCinK :: [(Int,Char)] -> [(Int,Char)] -> (Int, Char) -> [(Int,Char)]
insertCinK ys [] (i,c) = foldr (\x l -> x : l) [(i,c)] ys
insertCinK ys ((xi,xv):xs) (i,c)
    | i < xi    = foldr (\x l -> x : l) ((i,c) : ((xi,xv):xs)) ys
    | otherwise = insertCinK (ys ++ [(xi,xv)]) xs (i,c)


{-  getIndex xs c acc
    get the index of char c in list xs

-}
getIndex :: String -> Char -> Int -> [Int]
getIndex "" c acc = []
getIndex (x:xs) c acc
    | x == c = acc : getIndex xs c (acc + 1)
    | otherwise = getIndex xs c (acc + 1)


createTuples :: [Int] -> Char -> [(Int,Char)]
createTuples [x] c    = [(x,c)]
createTuples (x:xs) c = (x,c) : createTuples xs c



tree i = treePrint i   

treePrint 0 = do
    putStrLn "   ______ \n\
             \  |      |\n\
             \  |      O\n\
             \  |     -|- \n\
             \__|__   J L \n\
             \|   |"

treePrint 1 = do
    putStrLn "   ______ \n\
             \  |      |\n\
             \  |      O\n\
             \  |     -|- \n\
             \__|__     \n\
             \|   |"

treePrint 2 = do
    putStrLn "   ______ \n\
             \  |      |\n\
             \  |      O\n\
             \  |       \n\
             \__|__     \n\
             \|   |"

treePrint 3 = do
    putStrLn "   ______ \n\
             \  |       \n\
             \  |       \n\
             \  |       \n\
             \__|__     \n\
             \|   |"

treePrint 4 = do 
    putStrLn "  |  \n\
             \  |  \n\
             \  |  \n\
             \__|__\n\
             \|   |"

treePrint 5 = do 
    putStrLn "____\n\
             \|  |"

treePrint i = do
    putStrLn ""