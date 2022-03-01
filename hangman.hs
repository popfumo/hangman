import System.IO
import System.Exit
import System.Random

import Control.Monad

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game 
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Test.HUnit

{-
    Hangman (The random Word) (The correct guessed letters with index as key) (The faulty guessed letters)
-}

{- Hangman is based on Strings, list with a tuple that contains a pair with one int and one char 
and one list that contains one or more chars. The string denotes the random word that user is
guessing on. The list with the tuple that contains an inte and a char denotes how 
INVARIANT: The list cant be void, the Int must be a positive value.  
    -}
data Hangman = None | Hangman String [(Int,Char)] [Char]
    deriving (Show)

data World = World Scene Hangman Guess

data Scene = Menu | Single | MultiInput | Multi | BadInput Scene | Win | Lose | Exit

type Guess = String
type Word = String

numbOfGuesses :: Int
numbOfGuesses = 6

filepath = "wordlist.txt"

window :: Display
window = InWindow "Hangman" (1000 , 700) (10,10)

newWorld :: World
newWorld = World Menu None []

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
    putStrLn "----------------------------------------------"
    putStrLn "Erik Odhner | Edvard Axelman | Viktor Wallstén"
    putStrLn "----------------------------------------------"

guiMain = playIO 
    window 
    background 
    10
    newWorld 
    drawingSceneIO 
    eventHandlerIO
    updateFuncIO

eventHandlerIO :: Event -> World -> IO World
eventHandlerIO event world@(World Menu _ _) = do 
    theWord <- randomWord   -- create randomword
    case event of
        (EventKey (Char '1') Down _ _) -> return $ World Single (Hangman theWord [] []) []
        (EventKey (Char '2') Down _ _) -> return $ World MultiInput (Hangman "" [] []) []
        (EventKey (Char '3') Down _ _) -> return $ World Exit None []
        _ -> return world
eventHandlerIO (EventKey (SpecialKey KeyDelete) Down _ _) (World scene hangman guess) = do return $ World scene hangman (take (length guess - 1) guess)
eventHandlerIO (EventKey (SpecialKey KeyBackspace) Down _ _) (World scene hangman guess) = do return $ World scene hangman (take (length guess -1) guess)
eventHandlerIO (EventKey (Char input) Down _ _) (World scene hangman guess) = do return $ World scene hangman (guess ++ [input])
eventHandlerIO (EventKey (SpecialKey KeyEnter) Down _ _) (World MultiInput hangman guess) = do return $ World Multi (Hangman guess [] []) []
eventHandlerIO (EventKey (SpecialKey KeyEnter) Down _ _) (World Win _ option) = do return (replay option)
eventHandlerIO (EventKey (SpecialKey KeyEnter) Down _ _) (World Lose _ option) = do return (replay option)
eventHandlerIO (EventKey (SpecialKey KeyEnter) Down _ _) world = do return $ checkWholeWord world
eventHandlerIO event world = do return world

checkWholeWord :: World -> World
checkWholeWord world@(World scene hangman@(Hangman word correct guessed) guess)
    | length guess == length word = if guess == word
                                        then World Win hangman []
                                        else World scene (Hangman word correct (guessed ++ guess)) []
    | otherwise = checkGuess world

checkGuess :: World -> World
checkGuess (World scene hangman@(Hangman word correct guessed) guess)
    | validInput guess hangman = if validGuess hangman guess
                                    then let newHangman = insertCorrectGuess hangman guess
                                         in checkWin (World scene newHangman [])
                                    else let newHangman = insertWrongGuess hangman guess
                                         in checkLose (World scene newHangman [])
    | otherwise = World (BadInput scene) hangman []

checkWin :: World -> World
checkWin world@(World scene hangman@(Hangman word correct guessed) guess)
    | correctGuess correct == word = World Win hangman []
    | otherwise = world 

checkLose :: World -> World
checkLose world@(World scene hangman@(Hangman word correct guessed) guess)
    | length guessed == numbOfGuesses = World Lose hangman []
    | otherwise = world

replay :: String -> World
replay [input]
    | input == 'y' = newWorld
    | otherwise    = World Exit None []



drawingSceneIO :: World -> IO Picture
drawingSceneIO (World scene hangman guess) = do 
            case scene of
                Menu    -> printMenu guiMenu
                Single  -> printScene (hangScene hangman guess)
                MultiInput -> printScene (multiInputScene guess)
                Multi   -> printScene (hangScene hangman guess)
                (BadInput scene) -> printScene ([color red $ rectangleSolid 2000 2000, hangtext (0,0) "BAD INPUT"])
                Lose    -> printScene (loseScene hangman)
                Win     -> printScene (winScene hangman)
                Exit    -> exitSuccess

updateFuncIO :: Float -> World -> IO World
updateFuncIO _ (World (BadInput scene) hangman guess) = do 
    return (World scene hangman guess)
updateFuncIO _ w = do
    return w

printMenu xs = do
    image <- loadBMP "hangman.bmp"
    let newImage = translate 0 300 image
        withText = [newImage] ++ xs
        withTree = drawing6 ++ withText
    return (pictures withTree)

printScene :: [Picture] -> IO Picture
printScene xs = do
    return (pictures xs)

hangtext (x,y) s = centerText (x,y) s $ reScale $ text s

centerText (x,y) s = translate (x-(halfSize s)) (y)

halfSize :: String -> Float
halfSize s = realToFrac $ (length s) `div` 2 * 15

reScale = scale 0.2 0.2


hangScene hangman@(Hangman theWord correct guessed) guess = 
    let underscore = foldl insertLetterinUnderscore (underscores (length theWord)) correct
        --randomword = "The Randomword: " ++ theWord
        guessesLeft = "Guesses left: " ++ show (numbOfGuesses - length guessed)
        wrongGuesses = "Wrong Guesses: " ++ guessed
        --yourGuess = "Guess so far: " ++ correctGuess correct
        tree = drawStick hangman 
    in  tree ++ foldl (\l x -> (translate (-150) (realToFrac $ (-40) * (length l)-150) $ reScale $ color black $ text x) : l) [] [underscore, guessesLeft, wrongGuesses, guess]

drawStick hangman@(Hangman theWord correct guessed) =
    let gLeft = (numbOfGuesses - length guessed)
    in case gLeft of 
        6 -> []
        5 -> drawing1
        4 -> drawing2
        3 -> drawing3
        2 -> drawing4
        1 -> drawing5
        0 -> drawing6
        _ -> []

exitScene = do exitSuccess


guiMenu = createPictureMenu menuList

createPictureMenu xs = foldl (\l x -> (translate (-100) (realToFrac $ (-40) * (length l)-150) $ reScale $ color black $ text x) : l) [] xs

menuList = ["1. Singleplayer","2. Multiplayer","3. Quit Game"]


multiInputScene guess = [hangtext (0,0) "Enter a word", hangtext (0,-40) guess]


loseScene None = []
loseScene hangman@(Hangman word correct guessed) = drawing6 ++ [(hangtext (0,-180) $ "Correct word was: " ++ word), (hangtext (0,-140) "You lost"), replayScene]

winScene None = []
winScene hangman@(Hangman word correct guessed) = (drawStick hangman) ++ [(hangtext (0,-150) "You Won"), replayScene] 

replayScene = hangtext (0,-220) "Do you want to play again? (y/n) "

drawing1 = [translate (-20) (-100) $ color green $ circleSolid 100,
            translate (-20) (-150) $ color white $ rectangleSolid 200 100]

drawing2 = drawing1 ++ [translate (-20) (100) $ color black $ rectangleSolid 10 200 ]

drawing3 = drawing2 ++ [translate (15) (200) $ color black $ rectangleSolid 75 10 ]

drawing4 = drawing3 ++ [translate (50) (180) $ color black $ rectangleSolid 2 30,
                        translate (50) (140) $ color black $ circle 20]

drawing5 = drawing4 ++ [translate (50) (97) $ color black $ rectangleSolid 1 50,
                        translate (110) (46) $ color black $ line [(-30, -30), (-60, 30)],
                        translate (-10) (46) $ color black $ line [(30, -30), (60, 30)]]

drawing6 = drawing5 ++ [translate (110) (110) $ color black $ line [(-30, -30), (-60, 0)], 
                        translate (-10) (110) $ color black $ line [(30, -30), (60, 0)]]


{- Main IO 
Calls on the GUI function and the menu function. 
-}
main :: IO ()
main = do
    splash
    chooseInterface

chooseInterface :: IO ()
chooseInterface = do
    putStrLn ""
    putStrLn "1. Graphical Interface"
    putStrLn "2. Terminal  Interface"
    putStrLn ""
    option <- getLine
    case option of
        "1" -> do guiMain
        "2" -> do menu
        _   -> exit

{- Menu IO  

SIDE EFFECTS: Prints out the game modes as strings and calls on different
depending on different user input. 
-}
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

{- Exit IO

SIDE EFFECT: Prints out the string "exited" on the terminal 
-}
exit :: IO ()
exit = do putStrLn "exited"


{- randomWord IO 

SIDE EFFECT: Returns a random IO string from the text file. 
-}
randomWord :: IO String
randomWord = do
        handle <- openFile filepath ReadMode -- Import wordlist
        contents <- hGetContents handle      -- Parse content in wordlist
        let list = lines contents            -- Creats a list of every word
            upper = length list              -- Creates a upper bound with the help of length of list
        ranInt <- randomRIO (1,upper-1)      -- Returns a random IO Int
        hClose handle                        -- closes handle
        let word = list !! ranInt            -- a random word from the list
            removedR = removeR word          -- removes the "\r" from the end of the word. Linux problem ...
        return (removedR)                    -- Returns a random word from the list with the help of the random number we get.

removeR [x]
    | [x] == "\r" = []
    | otherwise   = [x]
removeR (x:xs) = x : removeR xs

{- singleGame IO 

SIDE EFFECT: Calls on the gameAux function with the predefined hangman as the argument. 
-}
singleGame :: IO ()
singleGame =  do
    theWord <- randomWord
    let lengthWord = length theWord
        hangman = Hangman theWord [] []
    putStrLn ("Guess a letter or a string with " ++ show lengthWord ++ " letters\n")
    gameAux hangman

{- gameAux IO 
Main body function of the hangman game. Checks if the user input is A): A valid guess and B): A correct guess. 
Depending on the outcomes of A and B, the gameAux will call on different functions. 

SIDE EFFECT: Calls on the lose or win function. 
-}
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
                    putStrLn ("Bad guesses: " ++ guessed)
                    newGuess <- getGuess hangman
                    if length newGuess == length theWord
                        then do
                            if newGuess == theWord
                                then do
                                    win hangman
                                else do
                                    let newHangman = Hangman theWord correct (guessed ++ newGuess)
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


{- getGuess IO 
Takes a user input as a guess and 
SIDE EFFECT: 
-}
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

h2 = Hangman "hello" [(0,'h'),(4,'o')] "g"

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
    get the index/es of char c in string xs

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


--------------------------------------------------------------------------------
-- Test Cases
--------------------------------------------------------------------------------

-- validInput
testVI1 = TestCase $ assertEqual "validInput" True (validInput "horse" h2)
testVI2 = TestCase $ assertEqual "validInput" False (validInput "" h2)
testVI3 = TestCase $ assertEqual "validInput" False (validInput "it" h2)

-- validGuess
testVG1 = TestCase $ assertEqual "validGuess" True (validGuess h2 "e")
testVG2 = TestCase $ assertEqual "validGuess" False (validGuess h2 "k")

-- alreadyGuessed


-- insertWrongGuess


-- insertCorrectGuess


runAllTests = runTestTT $ TestList [testVI1, testVI2, testVI3, testVG1, testVG2]