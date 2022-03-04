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

{-  World is representing the current gui game state and contains the current Scene,
    the Hangman state and the user input
    In the World Scene Hangman Input,
    Scene is a data type with different scenes for displaying different pictures;
    Hangman is the current game state;
    Input is a list of Chars from the user input.
-}
data World = World Scene Hangman Input deriving (Show)

{-  Scene is representing the current scene to be printed.
    BadInput is a special case where it takes the previous Scene and prints
    an error screen and then returns to the previous Scene.
-}
data Scene = Menu | Single | MultiInput | Multi | BadInput Scene | Win | Lose | Exit deriving (Show)

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
eventHandlerIO (EventKey (SpecialKey KeyEsc) Down _ _) (World scene hangman guess) = do return $ World Exit None []
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
    EXAMPLES: checkGuess (World Single h2 "h") == World Single (Hangman "hello" [(0,'h'),(4,'o')] "gh") ""
              checkGuess (World Single h2 "e") == World Single (Hangman "hello" [(0,'h'),(1,'e'),(4,'o')] "g") ""
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
    EXAMPLES: checkWin World single Hangman "hello"  [(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')] [] [] == World Win hangman []
-}
checkWin :: World -> World
checkWin world@(World scene hangman@(Hangman word correct guessed) guess)
    | correctGuess correct == word = World Win hangman []
    | otherwise = world 

{-  checkLose world
    checkLose cheks if the use has run out of guesses.
    RETURNS: It returns a world, if the use has run out of guesses it returns a world lose otherwise it returns the same world
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

{-  drawingSceneIO world
    Prints a different picture depending on the scene            
    RETURNS: an IO Picture 
    SIDE EFFECTS: prints a picture in the window
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

{-  printMenu xs
    takes a list of Picture and prints the menu 
    RETURNS: an IO Picture    
    SIDE EFFECTS: prints a IO Picture in the window
-}
printMenu :: [Picture] -> IO Picture
printMenu xs = do
    image <- loadBMP "hangman.bmp"
    let newImage = translate 0 300 image
        withText = [newImage] ++ xs
        withTree = drawing6 ++ withText
    return (pictures withTree)

{-  guiMenu
    puts togheter the menu by calling createPictureMenu with the menuList
-}
guiMenu :: [Picture]
guiMenu = createPictureMenu menuList

{-  createPictureMenu xs
    takes a list of meny options as Strings and then converts each String to a Picture
    RETURNS: list of Picture
    EXAMPLES: createPictureMenu menuList == [Translate (-100.0) (-230.0) (Scale 0.2 0.2 (Color (RGBA 0.0 0.0 0.0 1.0) (Text "3. Quit Game"))),Translate (-100.0) (-190.0) (Scale 0.2 0.2 (Color (RGBA 0.0 0.0 0.0 1.0) (Text "2. Multiplayer"))),Translate (-100.0) (-150.0) (Scale 0.2 0.2 (Color (RGBA 0.0 0.0 0.0 1.0) (Text "1. Singleplayer")))]
-}
createPictureMenu :: [String] -> [Picture]
createPictureMenu xs = foldl (\l x -> (translate (-100) (realToFrac $ (-40) * (length l)-150) $ reScale $ color black $ text x) : l) [] xs

{-  
    menuList is a list of different menu options as Strings
-}
menuList :: [String]
menuList = ["1. Singleplayer","2. Multiplayer","3. Quit Game"]

{-  printScene xs
    converts a list of Picture to IO Picture
    RETURNS: an IO Picture
    EXAMPLES: printScene guiMenu == Pictures [Translate (-100.0) (-230.0) (Scale 0.2 0.2 (Color (RGBA 0.0 0.0 0.0 1.0) (Text "3. Quit Game"))),Translate (-100.0) (-190.0) (Scale 0.2 0.2 (Color (RGBA 0.0 0.0 0.0 1.0) (Text "2. Multiplayer"))),Translate (-100.0) (-150.0) (Scale 0.2 0.2 (Color (RGBA 0.0 0.0 0.0 1.0) (Text "1. Singleplayer")))]
-}
printScene :: [Picture] -> IO Picture
printScene xs = do
    return (pictures (instructions : xs))

{-  hangtext c s
    converts a String to a more suitable setting and places it at Coordinate c
    RETURNS: a Picture
    EXAMPLES: hangtext (20,40) "hej" == Translate 5.0 40.0 (Scale 0.2 0.2 (Text "hej"))
-}
hangtext :: Coordinate -> String -> Picture
hangtext (x,y) s = centerText (x,y) s $ reScale $ text s

{-  centerText c s
    places the String s at Coordinate c but centered over the x-coordinate
    RETURNS: a Picture
-}
centerText :: Coordinate -> String -> Picture -> Picture
centerText (x,y) s = translate (x-(halfSize s)) (y)

{-  halfSize s
    calculates half the size of the String s
    RETURNS: a Float
    EXAMPLES: halfSize "hej" == 22.5
-}
halfSize :: String -> Float
halfSize s = realToFrac (length s) / 2 * 15

{-
    reScale rescales the Picture to a more suitable scale
-}
reScale :: Picture -> Picture
reScale = scale 0.2 0.2

{-
    instructions is a Picture for showing the instructions of different buttons
-}
instructions :: Picture
instructions = hangtext (50,-330) "Enter: enter the input | Del: delete the recent input | Esc: quit game"

{-  hangScene hangman guess
    creates a list of Picture with the current game state
    PRE: hangman can't be None
    RETURNS: a list with Picture
    EXAMPLES: hangScene h2 "e" == [Translate (-20.0) (-100.0) (Color (RGBA 0.0 1.0 0.0 1.0) (ThickCircle 50.0 100.0)),Translate (-20.0) (-150.0) (Color (RGBA 1.0 1.0 1.0 1.0) (Polygon [(-100.0,-50.0),(-100.0,50.0),(100.0,50.0),(100.0,-50.0)])),Translate (-150.0) (-270.0) (Scale 0.2 0.2 (Color (RGBA 0.0 0.0 0.0 1.0) (Text "e"))),Translate (-150.0) (-230.0) (Scale 0.2 0.2 (Color (RGBA 0.0 0.0 0.0 1.0) (Text "Wrong Guesses: g"))),Translate (-150.0) (-190.0) (Scale 0.2 0.2 (Color (RGBA 0.0 0.0 0.0 1.0) (Text "Guesses left: 5"))),Translate (-150.0) (-150.0) (Scale 0.2 0.2 (Color (RGBA 0.0 0.0 0.0 1.0) (Text "h _ _ _ o")))]
-}
hangScene :: Hangman -> Input -> [Picture]
hangScene hangman@(Hangman theWord correct guessed) guess = 
    let underscore = foldl insertLetterinUnderscore (underscores (length theWord)) correct
        wrongGuesses = "Wrong Guesses: " ++ guessed
        tree = drawStick hangman 
    in  tree ++ foldl (\l x -> (translate (-150) (realToFrac $ (-40) * (length l)-150) $ reScale $ color black $ text x) : l) [] [underscore, wrongGuesses, guess] 

{-  multiInputScene input
    creates the list of Picture for the MultiInput scene
    RETURNS: a list of Picture
    EXAMPLES: multiInputScene "e" == [Translate (-90.0) 0.0 (Scale 0.2 0.2 (Text "Enter a word")),Translate (-7.5) (-40.0) (Scale 0.2 0.2 (Text "e"))]
-}
multiInputScene :: Input -> [Picture]
multiInputScene input = [hangtext (0,0) "Enter a word", hangtext (0,-40) input]

{-
    badInputScene is a list of Picture for the BadInput scene
-}
badInputScene :: [Picture]
badInputScene = [color red $ rectangleSolid 2000 2000, hangtext (0,0) "BAD INPUT"]

{-  loseScene hangman input
    creates the list of Picture for the Lose scene
    PRE: hangman can't be None 
    RETURNS: a list of Picture
    EXAMPLES: loseScene h2 "y" == [Translate (-20.0) (-100.0) (Color (RGBA 0.0 1.0 0.0 1.0) (ThickCircle 50.0 100.0)),Translate (-20.0) (-150.0) (Color (RGBA 1.0 1.0 1.0 1.0) (Polygon [(-100.0,-50.0),(-100.0,50.0),(100.0,50.0),(100.0,-50.0)])),Translate (-20.0) 100.0 (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-5.0,-100.0),(-5.0,100.0),(5.0,100.0),(5.0,-100.0)])),Translate 15.0 200.0 (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-37.5,-5.0),(-37.5,5.0),(37.5,5.0),(37.5,-5.0)])),Translate 50.0 180.0 (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-1.0,-15.0),(-1.0,15.0),(1.0,15.0),(1.0,-15.0)])),Translate 50.0 140.0 (Color (RGBA 0.0 0.0 0.0 1.0) (Circle 20.0)),Translate 50.0 97.0 (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-0.5,-25.0),(-0.5,25.0),(0.5,25.0),(0.5,-25.0)])),Translate 110.0 46.0 (Color (RGBA 0.0 0.0 0.0 1.0) (Line [(-30.0,-30.0),(-60.0,30.0)])),Translate (-10.0) 46.0 (Color (RGBA 0.0 0.0 0.0 1.0) (Line [(30.0,-30.0),(60.0,30.0)])),Translate 110.0 110.0 (Color (RGBA 0.0 0.0 0.0 1.0) (Line [(-30.0,-30.0),(-60.0,0.0)])),Translate (-10.0) 110.0 (Color (RGBA 0.0 0.0 0.0 1.0) (Line [(30.0,-30.0),(60.0,0.0)])),Translate (-172.5) (-180.0) (Scale 0.2 0.2 (Text "Correct word was: hello")),Translate (-60.0) (-140.0) (Scale 0.2 0.2 (Text "You lost")),Translate (-247.5) (-220.0) (Scale 0.2 0.2 (Text "Do you want to play again? (y/n) ")),Translate (-7.5) (-260.0) (Scale 0.2 0.2 (Text "y"))]
-}
loseScene :: Hangman -> Input -> [Picture]
loseScene hangman@(Hangman word correct guessed) input = drawing6 ++ [(hangtext (0,-180) $ "Correct word was: " ++ word), (hangtext (0,-140) "You lost"), replayScene, hangtext (0,-260) input]

{-  winScene hangman input
    creates the list of Picture for the Win scene
    PRE: hangman can't be None 
    RETURNS: a list of Picture
    EXAMPLES: winScene h2 "y" == [Translate (-20.0) (-100.0) (Color (RGBA 0.0 1.0 0.0 1.0) (ThickCircle 50.0 100.0)),Translate (-20.0) (-150.0) (Color (RGBA 1.0 1.0 1.0 1.0) (Polygon [(-100.0,-50.0),(-100.0,50.0),(100.0,50.0),(100.0,-50.0)])),Translate (-52.5) (-150.0) (Scale 0.2 0.2 (Text "You Won")),Translate (-247.5) (-220.0) (Scale 0.2 0.2 (Text "Do you want to play again? (y/n) ")),Translate (-7.5) (-260.0) (Scale 0.2 0.2 (Text "y"))]

-}
winScene :: Hangman -> Input -> [Picture]
winScene hangman@(Hangman word correct guessed) input = (drawStick hangman) ++ [(hangtext (0,-150) "You Won"), replayScene, hangtext (0,-260) input] 

{-
    replayScene is a Picture with the replay text
-}
replayScene :: Picture
replayScene = hangtext (0,-220) "Do you want to play again? (y/n) "

{-  updateSceneIO f world
    is used for changing from BadInput scene back to the previous scene
    RETURNS: an IO World
    EXAMPLES: updateSceneIO _ (World (BadInput Single) hangman guess) == (World Single hangman guess)
-}
updateSceneIO :: Float -> World -> IO World
updateSceneIO _ (World (BadInput scene) hangman guess) = do 
    return (World scene hangman guess)
updateSceneIO _ w = do
    return w

{-  terminalMenu
    prints out the terminal menu in the terminal and then calls different functions
    depending on user input
    SIDE EFFECTS: Prints out the terminal menu
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

{-  
    exit prints a string to the terminal       
-}
exit :: IO ()
exit = do putStrLn "Shutting down the game..."

{-  randomWord
    reads the words in filepath and returns a random word from that list
    PRE: filepath must be set and must be a .txt file with new word on each line
    RETURNS: an IO String
    SIDE EFFECTS: reads the filepath file and returns a word from that file
    EXAMPLES: randomWord == "overall"
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
    removes the \r from Strings that can appear when running randomWord on Linux
    RETURNS: a String
    VARIANT: length xs
    EXAMPLES: removeR "tester\r" == "tester"
              removeR "tester"   == "tester"
-}
removeR :: String -> String
removeR [x]
    | [x] == "\r" = []
    | otherwise   = [x]
removeR (x:xs) = x : removeR xs

{-  singleGame
    starts the single gamemode by getting a random word and calling game with that word
    SIDE EFFECTS: Prints a string to the terminal and calls the game function with the acquired randomWord
-}
singleGame :: IO ()
singleGame = do
    theWord <- randomWord
    let lengthWord = length theWord
        hangman = Hangman theWord [] []
    putStrLn ("Guess a letter or a string with " ++ show lengthWord ++ " letters\n")
    game hangman

{-  multiGame
    starts the multi gamemode by getting the word to guess from user input and then
    calls game with that word. If user entered wrong word they can type a new word by not continuing
    SIDE EFFECTS: prints Strings to the terminal
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

{-  game hangman
    Main body function of the hangman game. Checks if the user input is first a valid guess and then a correct guess. 
    Depending on the outcomes of the checks the game will call on different functions.
    PRE: hangman can't be None 
    SIDE EFFECTS: prints the current game state to the terminal
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
                    tree (guessesLeft) -- prints the hangmantree
                    putStrLn (underscore ++ "\n") -- prints the current state of the guessing
                    putStrLn ("Wrong guesses: " ++ guessed) -- prints the incorrect guessed letters
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
    RETURNS: an IO String        
    SIDE EFFECTS: prints a String to terminal if not valid input
-}
getGuess :: Hangman -> IO String
getGuess hangman = do
    newG <- getLine
    if validInput newG hangman
        then return newG
        else do putStrLn "Bad guess, try again"
                getGuess hangman 

{-  validInput xs hangman
    checks if the String s is a valid input
    PRE: hangman can't be None 
    RETURNS: a Bool 
    EXAMPLES: validInput ""      h2 == False
              validInput "abcd"  h2 == False
              validInput "abcde" h2 == True 
-}
validInput :: String -> Hangman -> Bool
validInput "" hangman = False
validInput [x] hangman = True
validInput xs (Hangman theWord _ _) = length xs == length theWord
 
{-  underscores i
    creates a String of i numbers of '_' with spaces between
    RETURNS: a String
    VARIANT: i
    EXAMPLES: underscores 5 == "_ _ _ _ _"  
-}
underscores :: Int -> String
underscores 1 = "_"
underscores x = '_' : ' ' : underscores (x-1)


{-  insertLetterinUnderscore xs (index,char)
    inserts a tuple (index,char) into the underscores xs
    RETURNS: String
    VARIANT: index
    EXAMPLE: foldl (\l x -> insertLetterinUnderscore l x ) "_ _ _" [(0,'h'),(1,'e'),(2,'j')] == "h e j"
-}
insertLetterinUnderscore :: [Char] -> (Int, Char) -> [Char]
insertLetterinUnderscore [x] (0,char)           = [char]
insertLetterinUnderscore (x:y:xs) (0, char)     = char : ' ' : xs
insertLetterinUnderscore (x:y:xs) (index, char) = x : y : insertLetterinUnderscore xs (index-1, char)

{-  printWord xs
    prints a String with a space between each Char
    RETURNS: a String
    VARIANT: length xs
    EXAMPLES: printWord "hello" == "h e l l o" 
-}
printWord :: String -> String
printWord ""     = ""
printWord [x]    = [x]
printWord (x:xs) = x : ' ' : printWord xs 

{-  correctGuess xs
    creates a list of the value in the tuple in xs
    RETURNS: a String
    EXAMPLES: correctGuess [(0,'h'),(1,'i')] == "hi"
-}
correctGuess :: [(Int, Char)] -> String
correctGuess = map snd

{-  win hangman
    prints the hangman tree and that the user have won
    PRE: hangman can't be None 
    SIDE EFFECTS: prints Strings to the terminal
-}
win :: Hangman -> IO ()
win (Hangman word correct guessed) = do
    tree (numbOfGuesses - length guessed)
    putStrLn $ printWord word
    putStrLn "\nCongratulations! You won!\n"
    endgame

{-  lose hangman
    prints the hangman tree and that the user have lost
    PRE: hangman can't be None 
    SIDE EFFECTS: prints Strings to the terminal
-}
lose :: Hangman -> IO ()
lose (Hangman word _ guessed) = do
    putStrLn $ "You lost!\n"
    tree 0
    putStrLn $ "The actual word was: \n" ++ printWord word
    endgame

{-  endgame
    let the user to replay or shutdown the game
    SIDE EFFECTS: prints a String to the terminal   
-}
endgame :: IO ()
endgame = do
    putStrLn "Do you want play again? (y/n): "
    usrInp <- getLine
    when (usrInp == "yes" || usrInp == "y") main

{-  validGuess hangman guess
    checks if the guess is correct.
    PRE: hangman can't be None         
    RETURNS: a Bool
    EXAMPLES: validGuess hangman@(Hangman w _ _) `a` = `a` elem w == True
-}
validGuess :: Hangman -> Input -> Bool
validGuess hangman@(Hangman w _ _) [c] =  c `elem` w && (not $ alreadyGuessed hangman [c]) -- kollar om din gissning är i ordet

{-  alreadyGuessed hangman input
    checks if the input have already been guessed. 
    PRE: hangman can't be None        
    RETURNS: a Bool 
    EXAMPLES: alreadyGuessed (Hangman _ k g) [c] = c `elem` correctGuess k == True 
-}
alreadyGuessed :: Hangman -> Input -> Bool
alreadyGuessed (Hangman _ k g) [c] = c `elem` g || c `elem` correctGuess k -- kollar om din gissning redan har gissats

{-  insertWrongGuess hangman input
    inserts the wrong guess in the guess list. 
    PRE: hangman can't be None
    RETURNS: a Hangman
    EXAMPLES: insertWrongGuess h2 "a" == Hangman "hello" [(0,'h'),(4,'o')] "ga"       
-}
insertWrongGuess :: Hangman -> Input -> Hangman
insertWrongGuess (Hangman w k g) [c] = Hangman w k (g ++ [c])

{-  insertCorrectGuess hangman input
    inserts the input into hangman with the correct index as key and input as the value
    PRE: hangman can't be None
    RETURNS: a Hangman
    EXAMPLES: insertCorrectGuess h2 "e" == Hangman "hello" [(0,'h'),(1,'e'),(4,'o')] "g"
-}
insertCorrectGuess :: Hangman -> Input -> Hangman
insertCorrectGuess (Hangman w k g) [c] = Hangman w (insert) g
                                       where insert = foldl (\l x -> insertInput [] l (x,c)) k (getIndex w c 0)

{-
    h1 and h2 are used to providing simplier examples
-}
h1 :: Hangman
h1 = Hangman "test" [(1,'e')] "l"
h2 :: Hangman
h2 = Hangman "hello" [(0,'h'),(4,'o')] "g"

{-  insertInput ys xs (i,c)
    inserts (i,c) in xs according to the index i
    RETURNS: a CorrectGuessed
    EXAMPLES: insertInput [] [(0,'h'),(2,'j')] (1,'e') == [(0,'h'),(1,'e'),(2,'j')]   
-}
insertInput :: CorrectGuessed -> CorrectGuessed -> (Int, Char) -> CorrectGuessed
insertInput ys [] (i,c) = foldr (\x l -> x : l) [(i,c)] ys
insertInput ys ((xi,xv):xs) (i,c)
    | i < xi    = foldr (\x l -> x : l) ((i,c) : ((xi,xv):xs)) ys
    | otherwise = insertInput (ys ++ [(xi,xv)]) xs (i,c)

{-  getIndex xs c acc
    get the index/es of char c in string xs
    RETURNS: a list of Integers
    EXAMPLES: getIndex "hello" 'l' 0 == [2,3]
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

{-  drawStick hangman
    returns the hangman drawing depending on how many guesses the player have left
    PRE: hangman can't be None
    RETURNS: a list of Picture
    EXAMPLES: drawStick h2 == [Translate (-20.0) (-100.0) (Color (RGBA 0.0 1.0 0.0 1.0) (ThickCircle 50.0 100.0)),Translate (-20.0) (-150.0) (Color (RGBA 1.0 1.0 1.0 1.0) (Polygon [(-100.0,-50.0),(-100.0,50.0),(100.0,50.0),(100.0,-50.0)]))]  
-}
drawStick :: Hangman -> [Picture]
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

drawing1 :: [Picture]
drawing1 = [translate (-20) (-100) $ color green $ circleSolid 100, translate (-20) (-150) $ color white $ rectangleSolid 200 100]

drawing2 :: [Picture]
drawing2 = drawing1 ++ [translate (-20) (100) $ color black $ rectangleSolid 10 200 ]

drawing3 :: [Picture]
drawing3 = drawing2 ++ [translate (15) (200) $ color black $ rectangleSolid 75 10 ]

drawing4 :: [Picture]
drawing4 = drawing3 ++ [translate (50) (180) $ color black $ rectangleSolid 2 30, translate (50) (140) $ color black $ circle 20]

drawing5 :: [Picture]
drawing5 = drawing4 ++ [translate (50) (97) $ color black $ rectangleSolid 1 50, translate (110) (46) $ color black $ line [(-30, -30), (-60, 30)], translate (-10) (46) $ color black $ line [(30, -30), (60, 30)]]

drawing6 :: [Picture]
drawing6 = drawing5 ++ [translate (110) (110) $ color black $ line [(-30, -30), (-60, 0)], translate (-10) (110) $ color black $ line [(30, -30), (60, 0)]]


-- Terminal Hangman drawing

{-
    tree i prints the i tree in the terminal
-}
tree :: Int -> IO ()
tree 0 = do
    putStrLn "   ______ \n\
             \  |      |\n\
             \  |      O\n\
             \  |     -|- \n\
             \__|__   J L \n\
             \|   |"

tree 1 = do
    putStrLn "   ______ \n\
             \  |      |\n\
             \  |      O\n\
             \  |     -|- \n\
             \__|__     \n\
             \|   |"

tree 2 = do
    putStrLn "   ______ \n\
             \  |      |\n\
             \  |      O\n\
             \  |       \n\
             \__|__     \n\
             \|   |"

tree 3 = do
    putStrLn "   ______ \n\
             \  |       \n\
             \  |       \n\
             \  |       \n\
             \__|__     \n\
             \|   |"

tree 4 = do 
    putStrLn "  |  \n\
             \  |  \n\
             \  |  \n\
             \__|__\n\
             \|   |"

tree 5 = do 
    putStrLn "____\n\
             \|  |"

tree i = do
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