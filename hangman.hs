import System.IO 
import System.Random
import Control.Monad


type Guess = String
type Word = String

main :: IO ()
main = do
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
              
--Main function
    --Monad.

--Check gamestate
    --Win/Lose
    --New game, calls on the main function again. 
    --Quit game, terminates the main function.

--Guess
    --User wordinput.
    --Connected to guess count.
    --Fixed amount of guesses.

filepath = "wordlist.txt"

randomWord = do  
        handle <- openFile filepath ReadMode -- Import wordlist
        contents <- hGetContents handle      -- Parse content in wordlist
        let list = lines contents            -- Creats a list of every word
            upper = length list              -- Creates a upper bound with the help of length of list
        ranInt <- randomRIO (1,upper-1)      -- Returns a random IO Int
        return $ list !! ranInt              -- Returns a random word from the list with the help of the random number we get.
        hClose handle                        -- closes handle


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



