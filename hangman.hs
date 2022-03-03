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

{-  Hangman is representing the current game state and contains the word to guess,
    the correct guessed letters and the wrongly guessed letters.
    In the Hangman rWord correct wrong,
    rWord is the word to be guessed and is a String;
    correct is a list of tuples with the key as the index of that Char in rWord and the value is the Char;
    wrong is a list of wrongly guessed Chars, so i.e its a String.
    None denotes an empty Hangman and is used when starting new game or quiting.
    INVARIANT: the list correct must have tuples with positive integers as key and indexed in correct order of rWord  
-}
data Hangman = None | Hangman RandomWord CorrectGuessed WrongGuessed deriving (Show)

{-  World is representing the current gui game state and contains the current Scenem
    the Hangman state and the user input
    In the World Scene Hangman Input,
    Scene is a data type with different scenes for displaying different pictures;
    Hangman is the current game state;
    Input is a list of Chars from the user input.
-}
data World = World Scene Hangman Input

{-  Scene is representing the current scene to be printed.
    BadInput is a special case where it takes the previous Scene and prints
    an error screen and then returns to the previous Scene.
-}
data Scene = Menu | Single | MultiInput | Multi | BadInput Scene | Win | Lose | Exit

{-  RandomWord
    Represents the word to be guessed, can be a random word in
    singleplayer mode or a user input in multiplayer mode.
    INVARIANT: can't be an empty list
-}
type RandomWord = String

{-  WrongGuessed
    Represents the incorrect guessed letters in a list,
    i.e it's a string
-}
type WrongGuessed = [Char]

{-  CorrectGuessed
    Represents the correctly guessed letter and the index of that letter in the word to guess.
    It's list with tuples with the index as key and the Char as value.
    INVARIANT: the list must have tuples with positive integers as key and indexed in correct order of RandomWord 
-}
type CorrectGuessed = [(Int,Char)]

{-  Input
    Input represents the input taken from the users keybord.
-}
type Input = [Char]

{-  Coordinate
    Represents a coordinate in the picture from origio that's in the center of the picture
-}
type Coordinate = (Float,Float)

--------------------------------------------------------------------------------
-- Global Variables
--------------------------------------------------------------------------------

{-
    numbOfGuesses equals how many guesses the player have
-}
numbOfGuesses :: Int
numbOfGuesses = 6

{-
    filepath tells the program where our list of secret word are located
-}
filepath :: String
filepath = "wordlist.txt"

{-
    window contains the information about the GUI window
    with the title as a String, size as a tuple of integers 
    and position as tuple of integers
-}
window :: Display
window = InWindow "Hangman" (1000 , 700) (10,10) -- change to FullScreen for fullscreen

{-
    newWorld starts up the menu
-}
newWorld :: World
newWorld = World Menu None []

{-
    background is used to specify the background color for the GUI
-}
background :: Color
background = white

{-
    splash is used as a starting screen to welcome the user
    SIDE EFFECTS: prints a splash screen on the terminal
-}
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

--------------------------------------------------------------------------------
-- Main functions
--------------------------------------------------------------------------------

{-  main
    main is used to start the game loop
    SIDE EFFECTS: prints the splash and chooseInterface menu on the terminal
-}
main :: IO ()
main = do
    splash
    chooseInterface

{-  chooseInterface
    a case comparison menu for either starting the graphical or the terminal interface
    SIDE EFFECTS: prints a menu on the terminal
-}
chooseInterface :: IO ()
chooseInterface = do
    putStrLn ""
    putStrLn "1. Graphical Interface"
    putStrLn "2. Terminal  Interface"
    putStrLn ""
    option <- getLine
    case option of
        "1" -> do guiMain
        "2" -> do terminalMenu
        _   -> exit

{-  guiMain
    handles the gui game loop with the gloss in-built function playIO
    SIDE EFFECTS: displays a window
-}
guiMain :: IO ()
guiMain = playIO 
    window 
    background 
    10
    newWorld
    drawingSceneIO -- draws the scene
    eventHandlerIO -- interpreters the input 
    updateSceneIO  -- catches BadInput and returns previous scene

{-  eventHandlerIO event world
    depending on which Scene the world contains it handles the different events (button presses)
    and returns a updated world
    RETURNS: an IO World
    SIDE EFFECTS: 
-}
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
eventHandlerIO (EventKey (SpecialKey KeyEnter) Down _ _) (World MultiInput hangman guess) = do 
    if length guess == 0
        then return $ World (BadInput MultiInput) hangman []
        else return $ World Multi (Hangman guess [] []) []
eventHandlerIO (EventKey (SpecialKey KeyEnter) Down _ _) (World Win _ option) = do return (replay option)
eventHandlerIO (EventKey (SpecialKey KeyEnter) Down _ _) (World Lose _ option) = do return (replay option)
eventHandlerIO (EventKey (SpecialKey KeyEnter) Down _ _) world = do return $ checkWholeWord world
eventHandlerIO event world = do return world

{-  checkWholeWord world
    checks if the guess is a whole word and then if its correct or not. If the guess
    is not a whole word it will call on checkGuess
    RETURNS: a World    
    SIDE EFFECTS: 
-}
checkWholeWord :: World -> World
checkWholeWord world@(World scene hangman@(Hangman word correct guessed) guess)
    | length guess == length word = if guess == word
                                        then World Win hangman []
                                        else checkLose (World scene (Hangman word correct (guessed ++ guess)) [])
    | otherwise = checkGuess world

{-  checkGuess world
    first calls on validInput with the guess and then calls on validGuess with the guess,
    depending on the return it creates a new world and checks win or lose
    RETURNS: a World 
    SIDE EFFECTS:    
-}
checkGuess :: World -> World
checkGuess (World scene hangman@(Hangman word correct guessed) guess)
    | validInput guess hangman = if validGuess hangman guess
                                    then let newHangman = insertCorrectGuess hangman guess
                                         in checkWin (World scene newHangman [])
                                    else let newHangman = insertWrongGuess hangman guess
                                         in checkLose (World scene newHangman [])
    | otherwise = World (BadInput scene) hangman []

{-  checkWin world 
    checkWin checks if the winning criterias are satisfied or not 
    RETURNS: It returns a world, if the winning criterias are meet it returns a world Win otherwise it returns the same world
    SIDE EFFECTS: 
    EXAMPLES: checkWin World single Hangman "hello"  [(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')] [] [] == World Win hangman []
-}
checkWin :: World -> World
checkWin world@(World scene hangman@(Hangman word correct guessed) guess)
    | correctGuess correct == word = World Win hangman []
    | otherwise = world 

{-  checkLose world
    checkLose cheks if the use has run out of guesses.
    RETURNS: It returns a world, if the use has run out of guesses it returns a world lose otherwise it returns the same world        
    SIDE EFFECTS:   
    EXAMPLES: World _ Hangman _ _ ["h","e","l","l","o","w"] [] == World Lose hangman []    
-}
checkLose :: World -> World
checkLose world@(World scene hangman@(Hangman word correct guessed) guess)
    | length guessed >= numbOfGuesses = World Lose hangman []
    | otherwise = world

{-  replay String
    Replay takes a input from the user and either replays the game or exits the program            
    RETURNS: Returns a world, either a world exit or a newWorld   
    EXAMPLES: Replay 'y' == newWorld
              Replay 'n' == World Exit None []   
-}
replay :: String -> World
replay [input]
    | input == 'y' = newWorld
    | otherwise    = World Exit None []

{-  functionIdentifier arguments
    Prints a different picture depending on the scene            
    RETURNS: A picture 
    EXAMPLES:       
-}
drawingSceneIO :: World -> IO Picture
drawingSceneIO (World scene hangman guess) = do 
            case scene of
                Menu    -> printMenu guiMenu
                Single  -> printScene (hangScene hangman guess)
                MultiInput -> printScene (multiInputScene guess)
                Multi   -> printScene (hangScene hangman guess)
                (BadInput scene) -> printScene badInputScene
                Lose    -> printScene (loseScene hangman guess)
                Win     -> printScene (winScene hangman guess)
                Exit    -> exitSuccess

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
printMenu :: [Picture] -> IO Picture
printMenu xs = do
    image <- loadBMP "hangman.bmp"
    let newImage = translate 0 300 image
        withText = [newImage] ++ xs
        withTree = drawing6 ++ withText
    return (pictures withTree)

guiMenu :: [Picture]
guiMenu = createPictureMenu menuList

createPictureMenu :: [String] -> [Picture]
createPictureMenu xs = foldl (\l x -> (translate (-100) (realToFrac $ (-40) * (length l)-150) $ reScale $ color black $ text x) : l) [] xs

menuList :: [String]
menuList = ["1. Singleplayer","2. Multiplayer","3. Quit Game"]

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
printScene :: [Picture] -> IO Picture
printScene xs = do
    return (pictures xs)

hangtext :: Coordinate -> String -> Picture
hangtext (x,y) s = centerText (x,y) s $ reScale $ text s

centerText :: Coordinate -> String -> Picture -> Picture
centerText (x,y) s = translate (x-(halfSize s)) (y)

halfSize :: String -> Float
halfSize s = realToFrac $ (length s) `div` 2 * 15

reScale :: Picture -> Picture
reScale = scale 0.2 0.2

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
hangScene :: Hangman -> Input -> [Picture]
hangScene hangman@(Hangman theWord correct guessed) guess = 
    let underscore = foldl insertLetterinUnderscore (underscores (length theWord)) correct
        guessesLeft = "Guesses left: " ++ show (numbOfGuesses - length guessed)
        wrongGuesses = "Wrong Guesses: " ++ guessed
        tree = drawStick hangman 
    in  tree ++ foldl (\l x -> (translate (-150) (realToFrac $ (-40) * (length l)-150) $ reScale $ color black $ text x) : l) [] [underscore, guessesLeft, wrongGuesses, guess]

multiInputScene :: Input -> [Picture]
multiInputScene guess = [hangtext (0,0) "Enter a word", hangtext (0,-40) guess]

badInputScene :: [Picture]
badInputScene = [color red $ rectangleSolid 2000 2000, hangtext (0,0) "BAD INPUT"]

loseScene :: Hangman -> Input -> [Picture]
loseScene hangman@(Hangman word correct guessed) guess = drawing6 ++ [(hangtext (0,-180) $ "Correct word was: " ++ word), (hangtext (0,-140) "You lost"), replayScene, hangtext (0,-260) guess]

winScene :: Hangman -> Input -> [Picture]
winScene hangman@(Hangman word correct guessed) guess = (drawStick hangman) ++ [(hangtext (0,-150) "You Won"), replayScene, hangtext (0,-260) guess] 

replayScene :: Picture
replayScene = hangtext (0,-220) "Do you want to play again? (y/n) "

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
updateSceneIO :: Float -> World -> IO World
updateSceneIO _ (World (BadInput scene) hangman guess) = do 
    return (World scene hangman guess)
updateSceneIO _ w = do
    return w

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS: Prints out the game modes as strings and calls on 
                  different functions depending on the user input. 
    EXAMPLES:       
-}
terminalMenu :: IO ()
terminalMenu = do
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

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS: Prints out the string "Shutting down the game..." on the terminal
    EXAMPLES:       
-}
exit :: IO ()
exit = do putStrLn "Shutting down the game..."

{-  randomWord
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS: Returns a random IO string from the text file.
    EXAMPLES:       
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

{-  removeR xs
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:
    VARIANT: length xs
    EXAMPLES:       
-}
removeR :: String -> String
removeR [x]
    | [x] == "\r" = []
    | otherwise   = [x]
removeR (x:xs) = x : removeR xs

{-  singleGame
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS: Prints a string to the terminal and calls the game function with the acquired randomWord
    EXAMPLES:       
-}
singleGame :: IO ()
singleGame = do
    theWord <- randomWord
    let lengthWord = length theWord
        hangman = Hangman theWord [] []
    putStrLn ("Guess a letter or a string with " ++ show lengthWord ++ " letters\n")
    game hangman

{- game IO 
Main body function of the hangman game. Checks if the user input is A): A valid guess and B): A correct guess. 
Depending on the outcomes of A and B, the game will call on different functions. 

SIDE EFFECT: Calls on the lose or win function. 
-}
{-  game hangman
    Main body function of the hangman game. Checks if the user input is first a valid guess and then a correct guess. 
    Depending on the outcomes of the checks the game will call on different functions.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
game :: Hangman -> IO ()
game hangman@(Hangman theWord correct guessed) = do
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
                    putStrLn ("Wrong guesses: " ++ guessed)
                    newGuess <- getGuess hangman
                    if length newGuess == length theWord
                        then do
                            if newGuess == theWord
                                then do
                                    win hangman
                                else do
                                    let newHangman = Hangman theWord correct (guessed ++ newGuess)
                                    game newHangman
                        else do    
                            if validGuess hangman newGuess
                                then do
                                    let newHangman = insertCorrectGuess hangman newGuess
                                    game newHangman
                                else do
                                    putStrLn ("Incorrect, try another letter")
                                    let newHangman = insertWrongGuess hangman newGuess
                                    game newHangman

{-  getGuess hangman
    Takes a user input and checks with validInput if its valid otherwise it will call itself and repeat till valid input
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
getGuess :: Hangman -> IO String
getGuess hangman = do
    newG <- getLine
    if validInput newG hangman
        then return newG
        else do putStrLn "Bad guess, try again"
                getGuess hangman 

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
validInput :: String -> Hangman -> Bool
validInput "" hangman = False
validInput [x] hangman = True
validInput ls@(x:xs) (Hangman theWord _ _) = length ls == length theWord
 
{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
underscores :: Int -> [Char]
underscores 1 = "_"
underscores x = '_' : ' ' : underscores (x-1)


{-  insertLetterinUnderscore xs (index,char)
    inserts a tuple (index,char) into the underscores xs
    RETURNS: String
    EXAMPLE: foldl (\l x -> insertLetterinUnderscore l x ) "_ _ _" [(0,'h'),(1,'e'),(2,'j')] == "h e j"
-}
insertLetterinUnderscore :: [Char] -> (Int, Char) -> [Char]
insertLetterinUnderscore [x] (0,char)           = [char]
insertLetterinUnderscore (x:y:xs) (0, char)     = char : ' ' : xs
insertLetterinUnderscore (x:y:xs) (index, char) = x : y : insertLetterinUnderscore xs (index-1, char)

--iLu _ _ _ (1, "e") = _" " : iLu _ _ (1-1, "e")

--iLu _ _ (0, "e") = _" " : "e" " " : _ 

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
printWord :: String -> String
printWord [x]    = [x]
printWord (x:xs) = x : ' ' : printWord xs 

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
correctGuess :: [(Int, Char)] -> [Char]
correctGuess = map snd

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
multiGame :: IO ()
multiGame = do
    putStrLn "Input the word to guess: "
    getWord <- getLine
    putStrLn ("Continue or type another word? (y/n)")
    getOption <- getLine
    if getOption == "y"
        then game (Hangman getWord [] [])
        else multiGame
    
{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
win :: Hangman -> IO ()
win (Hangman word correct guessed) = do
    tree (numbOfGuesses - length guessed)
    putStrLn $ printWord word
    putStrLn "\nCongratulations! You won!\n"
    endgame

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
lose :: Hangman -> IO ()
lose (Hangman word _ guessed)
    | (length guessed) >= numbOfGuesses = do
                                    putStrLn $ "You lost!\n"
                                    tree 0  -- print the hangman
                                    putStrLn $ "The actual word was: \n" ++ printWord word
                                    endgame
    | otherwise                   = return ()
    --where wrongGuess = length guessed - length [x | x <- guessed, x `elem` word]

{-  endgame

    RETURNS: Either returns nothing or calls on main.     
    SIDE EFFECTS: Prints out a string  
    EXAMPLES: usrInp == yes => main    
-}
endgame :: IO ()
endgame = do
    putStrLn "Do you want play again? (y/n): "
    usrInp <- getLine
    when (usrInp == "yes" || usrInp == "y") main

{-  validGuess hangman@(Hangman w _ _) [c]
    Checks if the player guess is correct.          
    RETURNS: A boolean satement 
    SIDE EFFECTS: None   
    EXAMPLES: validGuess hangman@(Hangman w _ _) `a` = `a` elem w == True
-}
validGuess :: Hangman -> Input -> Bool
validGuess hangman@(Hangman w _ _) [c] =  c `elem` w && (not $ alreadyGuessed hangman [c]) -- kollar om din gissning är i ordet

{-  alreadyGuessed  (Hangman _ k g) [c]
    Checks if the player is guessed have already been guessed.          
    RETURNS: A boolean statement.    
    SIDE EFFECTS: None   
    EXAMPLES: alreadyGuessed (Hangman _ k g) [c] = c `elem` correctGuess k == True 
-}
alreadyGuessed :: Hangman -> Input -> Bool
alreadyGuessed (Hangman _ k g) [c] = c `elem` g || c `elem` correctGuess k -- kollar om din gissning redan har gissats

{-  insertWrongGuess (Hangman w k g)
    Inserts the wrong guess in the guess list. 
    PRE: Either a Char or a list of Chars. 
    RETURNS: A list of Chars.       
    SIDE EFFECTS: None. 
    EXAMPLES: insertWrongGuess (Hangman w k []) 'a' == insertWrongGuess (Hangman w k [a])       
-}
insertWrongGuess :: Hangman -> Input -> Hangman
insertWrongGuess (Hangman w k g) [c] = Hangman w k (g ++ [c])

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
insertCorrectGuess :: Hangman -> Input -> Hangman
insertCorrectGuess (Hangman w k g) [c] = Hangman w (insert) g
                                       where insert = foldl (\l x -> insertInput [] l (x,c)) k (getIndex w c 0)

h1 :: Hangman
h1 = Hangman "test" [(1,'e')] "l"

h2 :: Hangman
h2 = Hangman "hello" [(0,'h'),(4,'o')] "g"

{-  insertInput ys xs (i,c)
    inserts (i,c) in xs according to the index i
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES: insertInput [] [(0,'h'),(2,'j')] (1,'e') == [(0,'h'),(1,'e'),(2,'j')]   
-}
insertInput :: [(Int,Char)] -> [(Int,Char)] -> (Int, Char) -> [(Int,Char)]
insertInput ys [] (i,c) = foldr (\x l -> x : l) [(i,c)] ys
insertInput ys ((xi,xv):xs) (i,c)
    | i < xi    = foldr (\x l -> x : l) ((i,c) : ((xi,xv):xs)) ys
    | otherwise = insertInput (ys ++ [(xi,xv)]) xs (i,c)

{-  getIndex xs c acc
    get the index/es of char c in string xs
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
getIndex :: String -> Char -> Int -> [Int]
getIndex "" c acc = []
getIndex (x:xs) c acc
    | x == c = acc : getIndex xs c (acc + 1)
    | otherwise = getIndex xs c (acc + 1)

--------------------------------------------------------------------------------
-- Hangman Drawings
--------------------------------------------------------------------------------

-- GUI Hangman drawing

{-  functionIdentifier arguments
    A brief human-readable description of the purpose of the function.
    PRE:            
    RETURNS:        
    SIDE EFFECTS:   
    EXAMPLES:       
-}
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


-- Terminal Hangman drawing

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