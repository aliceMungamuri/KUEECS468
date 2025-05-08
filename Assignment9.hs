{- EECS 468 Assignment 9 - Nim Game
= game of Nim 
player input 
   outputs: board display, prompts,  winner message.
=   other sources: ChatGPT, StackOverflow
   author: Alice Mungamuri
   creation date: May 7, 2025
-}

type Board = [Int] -- define Board 
initial :: Board -- declare the board
initial = [5,4,3,2,1] -- 5 rows 

putBoard :: Board -> IO () -- display
putBoard board = do -- start a block
  putStrLn "" -- print a blank line 
  mapM_ putRow (zip [1..] board) -- zip row numbers with board values 
  where putRow (n, stars) = putStrLn (show n ++ ": " ++ replicate stars '*') -- this justt prints row num and then star

isGameOver :: Board -> Bool -- t/f is it over
isGameOver = all (== 0) -- true of 0 stars everywhere

validMove :: Board -> Int -> Int -> Bool -- move is valid?
validMove board row stars = 
  row >= 1 && row <= length board && 
  stars >= 1 && stars <= board !! (row - 1) -- limits on stoats - must be pos 

makeMove :: Board -> Int -> Int -> Board -- new board after a move
makeMove board row stars = -- update selected row 
  take (row - 1) board ++ -- keep rows before the chosen row
  [board !! (row - 1) - stars] ++ -- subtract stars
  drop row board -- keep rows =after the changed row

play :: Board -> Int -> IO () -- recursive  loop:
play board player = do
  putBoard board -- board
  if isGameOver board -- check if all rows are empty
    then putStrLn ("\nPlayer " ++ show (3 - player) ++ " wins!") -- display winner (opponent of current player)
    else do -- continue the game
      putStrLn ("\nPlayer " ++ show player) --  current player
      putStr "Enter a row number: " --   row
      rowStr <- getLine -- read row input
      putStr "Stars to remove: " --number of stars
      starsStr <- getLine -- read  input
      let row = read rowStr :: Int -- input to int
          stars = read starsStr :: Int 
      if validMove board row stars 
        then play (makeMove board row stars) (3 - player) -
        else do -- invalid move: show error
          putStrLn "\nERROR: Invalid move" --  error  lowkey stackoverflowed this and Ik its not necessary
          play board player -- new turn same pkayer

nim :: IO () -- start game
nim = play initial 1 --  befin player 1 yay
