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
  lift $ r s
  where 
    r s = putStr ("\x1b[" ++ show rows ++ "A\x1b[0J") >> putStrLn (show s)


win :: StateT RoundS IO () 
win = do
  b <- gets board
  case boardWinner b of 
    Nothing -> return ()
    Just w -> do
      render
      modify (applyWinner w)
      _ <- readChar "The winner, press any key to continue" 
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


rows :: Int
rows = 12


tictactoe :: Int -> IO ()
tictactoe t = do
  putStrLn $ replicate rows '\n'
  _ <- runStateT (loop True) $ r
  return ()
  where 
    r = RoundS (pure Empty) 0 X (Score 0 0 0 t)
