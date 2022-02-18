import System.IO 
import System.Random
import Control.Monad


type Guess = String
type Word = String


filepath = "wordlist.txt"

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
              \   |___|                                                 "
    putStrLn "Written by Erik Odhner, Edvard Axelman and Viktor Wallstén"

main :: IO ()
main = do
    splash
    menu

theword = randomWord 

menu :: IO ()
menu = do
    putStrLn "--------------------"
    putStrLn "1. Singleplayer"
    putStrLn "2. Multiplayer"
    putStrLn "3. Quit game"
    putStrLn "--------------------"
    option <- getLine 
    menuChoice option

menuChoice :: IO String -> IO ()
menuChoice option = do
    case option of 
        "1" -> singleGame
        "2" -> multiGame
        "3" -> return ()
         -- _  -> menu TODO!!!!!!!!



randomWord :: IO String
randomWord = do  
        handle <- openFile filepath ReadMode -- Import wordlist
        contents <- hGetContents handle      -- Parse content in wordlist
        let list = lines contents            -- Creats a list of every word
            upper = length list              -- Creates a upper bound with the help of length of list
        ranInt <- randomRIO (1,upper-1)      -- Returns a random IO Int
        hClose handle                        -- closes handle
        return $ list !! ranInt              -- Returns a random word from the list with the help of the random number we get.
                               




test = do
    rword <- randomWord
    putStrLn ("Edvard is a: " ++ rword)


--singleGame :: IO ()
singleGame = do
    rword <- randomWord
    getGuess


guessCount = undefined 
    --if guess is wrong. Add 1 to acc.
    --There is a guess limit. So when the acc reaches a certain Int. Call lose function. 

multiGame = undefined

win :: IO ()
win = do 
    putStrLn "Congratulations! You won! faggot"
    endgame 

lose :: IO ()
lose = do
    rword <- randomWord
    putStrLn ("Out of guesses! The correct word was: " ++ rword)
    endgame


endgame :: IO ()
endgame = do
    putStrLn "Do you want play again? (y/n): "
    usrInp <- getLine 
    if usrInp == "yes" || usrInp == "y"
        then main
        else return ()

            
--När array är tom når vi basfallet och då har användaren gissat rätt.
--Plocka ur ett

--Main function
    --Monad.

--Check gamestate
    --Win/Lose
    --New game, calls on the main function again. 
    --Quit game, terminates the main function.

--Guess


getGuess = do
    putStrLn "What is your next guess?"
    inputGuess <- getLine
    case inputGuess of
        -- om tom gissning
        [] -> getGuess
        x  -> return x
        -- om det är en string
        --(x : tail) -> return x



    -- validChar -- TODO Check that input is a letter and also either only 1 char or the whole string


    --User wordinput.
    --Connected to guess count.
    --Fixed amount of guesses.

    --txt-file. (or parse the file into an array)
    --Plot out length of word in GUI. 

--WordCheck
    --Keeps track if the word is complete, calls on win status if the word is complete. 

--Guess count
    --Keeps track of how many guesses.
    --Wrong or right guess.
    --Max amount of guesses, if guess limit exceed: calls on lose status. 

--Data types
    --Letter, a list of Chars.
