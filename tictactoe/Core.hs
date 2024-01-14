module Core
    ( tictactoe
    ) where


import System.Console.Haskeline
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import State


readChar :: StateT RoundS IO (Maybe Char)
readChar = do
  s <- get
  c <- runInputT defaultSettings $ getInputChar (show (turn s) ++ ": ? ")
  return c


render :: StateT RoundS IO ()
render = do
  s <- get
  lift $ putStrLn (show s)


move :: Maybe Char -> StateT RoundS IO Bool 
move Nothing = return False 
move (Just c) = modify (applyMove c) >> return True


loop :: Bool -> StateT RoundS IO ()
loop False = return ()
loop _ = render >> readChar >>= move >>= loop


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
