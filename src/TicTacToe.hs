module TicTacToe
    ( tictactoe
    ) where


data Cell
  = X
  | O
  | Empty


instance Show Cell where
  show X = " X "
  show O = " O "
  show Empty = "   "


data Board a = Board 
  { nw :: a
  , n :: a
  , ne :: a
  , w :: a
  , c :: a
  , e :: a
  , sw :: a
  , s :: a
  , se :: a
  }



instance Functor Board where
  fmap f (Board x1 x2 x3 x4 x5 x6 x7 x8 x9) = Board (f x1) (f x2) (f x3)
                                                    (f x4) (f x5) (f x6)
                                                    (f x7) (f x8) (f x9)


instance Applicative Board where
  pure x = Board x x x x x x x x x

  (Board f1 f2 f3 f4 f5 f6 f7 f8 f9) <*> (Board x1 x2 x3 x4 x5 x6 x7 x8 x9) =
    Board (f1 x1) (f2 x2) (f3 x3)
          (f4 x4) (f5 x5) (f6 x6)
          (f7 x7) (f8 x8) (f9 x9)

{-
   |   |  
---+---+---
   |   |  
---+---+---
   |   |  
-}
instance Show a => Show (Board a) where
  show (Board x1 x2 x3 x4 x5 x6 x7 x8 x9) = 
    (show x1) ++ "|" ++ (show x2) ++ "|" ++ (show x3) ++ "\n---+---+---\n" ++
    (show x4) ++ "|" ++ (show x5) ++ "|" ++ (show x6) ++ "\n---+---+---\n" ++
    (show x7) ++ "|" ++ (show x8) ++ "|" ++ (show x9)


-- TODO: generalize
new :: Board Cell 
new = pure Empty


tictactoe :: IO ()
tictactoe = putStrLn $ show b
  where b = pure Empty :: Board Cell
