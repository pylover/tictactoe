module State 
  ( Player(..)
  , Cell(..)
  , Board(..)
  , Score(..)
  , RoundS(..)
  , applyMove
  ) where


data Player
  = X
  | O
  deriving Show


data Cell
  = Set Player
  | Empty


data Board a = Board 
  { c1 :: a, c2 :: a, c3 :: a
  , c4 :: a, c5 :: a, c6 :: a
  , c7 :: a, c8 :: a, c9 :: a
  }


data Score = Score
  { xWins :: Int
  , oWins :: Int
  , matches :: Int
  , total :: Int
  }


data RoundS = RoundS
  { board :: Board Cell
  , moves :: Int
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


instance Show Score where
  show (Score x o m t) = "X: " ++ (show x) ++ "\n" ++
                         "O: " ++ (show o) ++ "\n" ++
                         "total: " ++ (show m) ++ "/" ++ show (t) ++ "\n" 


instance Show a => Show (Board a) where
  show (Board x1 x2 x3 x4 x5 x6 x7 x8 x9) = 
    (sh x1) ++ "|" ++ (sh x2) ++ "|" ++ (sh x3) ++ "\n---+---+---\n" ++
    (sh x4) ++ "|" ++ (sh x5) ++ "|" ++ (sh x6) ++ "\n---+---+---\n" ++
    (sh x7) ++ "|" ++ (sh x8) ++ "|" ++ (sh x9) ++ "\n"
    where sh x = " " ++ show x ++ " "


instance Show RoundS where
  show (RoundS b m _ s) = show s ++ "\n" ++ 
                          show b ++ "\n" ++ 
                          "Moves: " ++ show m


toggle :: Player -> Player 
toggle X = O
toggle _ = X


boardUpdate :: Player -> Char -> Board Cell -> Board Cell
boardUpdate p '1' (Board _ x2 x3 x4 x5 x6 x7 x8 x9) = 
  Board (Set p) x2 x3 x4 x5 x6 x7 x8 x9
boardUpdate p '2' (Board x1 _ x3 x4 x5 x6 x7 x8 x9) = 
  Board x1 (Set p) x3 x4 x5 x6 x7 x8 x9
boardUpdate _ _ b = b


applyMove :: Char -> RoundS -> RoundS
applyMove x (RoundS b m t s) = RoundS (boardUpdate t x b) (m+1) (toggle t) s
