module TicTacToe
    ( tictactoe
    ) where



data Player
  = X
  | O
  deriving Show


data Cell
  = Set Player
  | Empty


data Board a = Board 
  { c1 :: a
  , c2 :: a
  , c3 :: a
  , c4 :: a
  , c5 :: a
  , c6 :: a
  , c7 :: a
  , c8 :: a
  , c9 :: a
  }


data Score = Score 
  { x :: Int
  , o :: Int
  }


data Round = Round
  { board :: Board Cell
  , turn  :: Player
  , score :: Score
  }


instance Show Cell where
  show Empty = " "
  show (Set p) = show p


instance Functor Board where
  fmap f (Board x1 x2 x3 x4 x5 x6 x7 x8 x9) = 
    Board (f x1) (f x2) (f x3)
          (f x4) (f x5) (f x6)
          (f x7) (f x8) (f x9)


instance Applicative Board where
  pure x = Board x x x x x x x x x

  (Board f1 f2 f3 f4 f5 f6 f7 f8 f9) <*> (Board x1 x2 x3 x4 x5 x6 x7 x8 x9) =
    Board (f1 x1) (f2 x2) (f3 x3)
          (f4 x4) (f5 x5) (f6 x6)
          (f7 x7) (f8 x8) (f9 x9)


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
instance Show a => Show (Board a) where
  show (Board x1 x2 x3 x4 x5 x6 x7 x8 x9) = 
    (sh x1) ++ "|" ++ (sh x2) ++ "|" ++ (sh x3) ++ "\n---+---+---\n" ++
    (sh x4) ++ "|" ++ (sh x5) ++ "|" ++ (sh x6) ++ "\n---+---+---\n" ++
    (sh x7) ++ "|" ++ (sh x8) ++ "|" ++ (sh x9)
    where sh x = " " ++ show x ++ " "


instance Show Score where
  show (Score x o) = 
    "X: " ++ (show x) ++ "\n" ++
    "O: " ++ (show o) ++ "\n"


instance Show Round where
  show (Round b X s) = (show s) ++ "\n" ++ show b


tictactoe :: IO ()
tictactoe = do
  putStrLn $ show r 
  where 
    b = pure (Set X) :: Board Cell
    r = Round b X (Score 0 0)
