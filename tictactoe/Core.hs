module Core
    ( tictactoe
    ) where


import System.Console.Haskeline
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import State


readChar :: String -> StateT RoundS IO Char
readChar p = do
  t <- gets turn
  mayc <- runInputT defaultSettings $ getInputChar (show t ++ ": " ++ p)
  case mayc of 
    Nothing -> fail "EOF"
    Just 'q' -> fail "Quit"
    Just c -> return c


render :: StateT RoundS IO ()
render = do
  s <- get
  lift $ putStrLn (show s)


win :: StateT RoundS IO () 
win = do
  b <- gets board
  case boardWinner b of 
    Nothing -> return ()
    Just w -> do
      render
      modify (applyWinner w)
      _ <- readChar " Winner, press any key to continue" 
      return ()


continue :: StateT RoundS IO Bool
continue = do
  s <- gets score
  if (total s) == (matches s) 
    then return False
    else return True


move :: Char -> StateT RoundS IO () 
move c = do
  s <- get
  if canMove c s
    then modify (applyMove c) >> win
    else return ()


loop :: Bool -> StateT RoundS IO ()
loop False = render >> return ()
loop _ = render >> readChar "? " >>= move >> continue >>= loop


{-

X: 0
O: 0

   |   |      1 | 2 | 3
---+---+---  ---+---+---
   |   |      4 | 5 | 6 
---+---+---  ---+---+---
   |   |      7 | 8 | 9

X, Press one of (1..9):
-}
tictactoe :: Int -> IO ()
tictactoe t = do
  _ <- runStateT (loop True) $ r
  return ()
  where 
    r = RoundS (pure Empty) 0 X (Score 0 0 0 t)
