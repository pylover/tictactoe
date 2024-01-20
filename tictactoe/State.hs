module State 
  ( Player(..)
  , Cell(..)
  , Board(..)
  , Score(..)
  , RoundS(..)
  , applyMove
  , applyWinner
  , boardWinner
  , boardIsFull
  , cellIsEmpty
  ) where


data Player
  = X
  | O
  deriving (Show, Eq)


data Cell
  = Set Player
  | Empty
  deriving Eq


data Board a = Board 
  { cell1 :: a, cell2 :: a, cell3 :: a
  , cell4 :: a, cell5 :: a, cell6 :: a
  , cell7 :: a, cell8 :: a, cell9 :: a
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
    row x1 x2 x3 ++ "\n" ++ hsep ++ 
    row x4 x5 x6 ++ "\n" ++ hsep ++
    row x7 x8 x9 ++ "\n" 
    where 
      sh x = " " ++ show x ++ " "
      row x y z = (sh x) ++ rsep ++ (sh y) ++ rsep ++ (sh z)
      rsep = "|"
      hsep = "---+---+---\n"


instance Show RoundS where
  show (RoundS b m _ s) = show s ++ "\n" ++ 
                          show b ++ "\n" ++ 
                          "Moves: " ++ show m


toggle :: Player -> Player 
toggle X = O
toggle _ = X


cellIsEmpty :: Char -> Board Cell -> Bool
cellIsEmpty '1' (Board Empty _ _ _ _ _ _ _ _) = True
cellIsEmpty '2' (Board _ Empty _ _ _ _ _ _ _) = True
cellIsEmpty '3' (Board _ _ Empty _ _ _ _ _ _) = True
cellIsEmpty '4' (Board _ _ _ Empty _ _ _ _ _) = True
cellIsEmpty '5' (Board _ _ _ _ Empty _ _ _ _) = True
cellIsEmpty '6' (Board _ _ _ _ _ Empty _ _ _) = True
cellIsEmpty '7' (Board _ _ _ _ _ _ Empty _ _) = True
cellIsEmpty '8' (Board _ _ _ _ _ _ _ Empty _) = True
cellIsEmpty '9' (Board _ _ _ _ _ _ _ _ Empty) = True
cellIsEmpty _ _ = False


boardIsFull :: Board Cell -> Bool
boardIsFull (Board Empty _ _ _ _ _ _ _ _) = False
boardIsFull (Board _ Empty _ _ _ _ _ _ _) = False
boardIsFull (Board _ _ Empty _ _ _ _ _ _) = False
boardIsFull (Board _ _ _ Empty _ _ _ _ _) = False
boardIsFull (Board _ _ _ _ Empty _ _ _ _) = False
boardIsFull (Board _ _ _ _ _ Empty _ _ _) = False
boardIsFull (Board _ _ _ _ _ _ Empty _ _) = False
boardIsFull (Board _ _ _ _ _ _ _ Empty _) = False
boardIsFull (Board _ _ _ _ _ _ _ _ Empty) = False
boardIsFull _ = True


-- TODO: use applicative functor
boardUpdate :: Player -> Char -> Board Cell -> Board Cell
boardUpdate p '1' (Board _ x2 x3 x4 x5 x6 x7 x8 x9) = 
  Board (Set p) x2 x3 x4 x5 x6 x7 x8 x9
boardUpdate p '2' (Board x1 _ x3 x4 x5 x6 x7 x8 x9) = 
  Board x1 (Set p) x3 x4 x5 x6 x7 x8 x9
boardUpdate p '3' (Board x1 x2 _ x4 x5 x6 x7 x8 x9) = 
  Board x1 x2 (Set p) x4 x5 x6 x7 x8 x9
boardUpdate p '4' (Board x1 x2 x3 _ x5 x6 x7 x8 x9) = 
  Board x1 x2 x3 (Set p) x5 x6 x7 x8 x9
boardUpdate p '5' (Board x1 x2 x3 x4 _ x6 x7 x8 x9) = 
  Board x1 x2 x3 x4 (Set p) x6 x7 x8 x9
boardUpdate p '6' (Board x1 x2 x3 x4 x5 _ x7 x8 x9) = 
  Board x1 x2 x3 x4 x5 (Set p) x7 x8 x9
boardUpdate p '7' (Board x1 x2 x3 x4 x5 x6 _ x8 x9) = 
  Board x1 x2 x3 x4 x5 x6 (Set p) x8 x9
boardUpdate p '8' (Board x1 x2 x3 x4 x5 x6 x7 _ x9) = 
  Board x1 x2 x3 x4 x5 x6 x7 (Set p) x9
boardUpdate p '9' (Board x1 x2 x3 x4 x5 x6 x7 x8 _) = 
  Board x1 x2 x3 x4 x5 x6 x7 x8 (Set p)
boardUpdate _ _ b = b


applyMove :: Char -> RoundS -> RoundS
applyMove x (RoundS b m t s) = RoundS (boardUpdate t x b) (m+1) (toggle t) s


applyWinnerScore :: Player -> Score -> Score
applyWinnerScore X (Score x o m t) = Score (x+1) o (m+1) t
applyWinnerScore O (Score x o m t) = Score x (o+1) (m+1) t


applyWinner :: Player -> RoundS -> RoundS
applyWinner p (RoundS _ m _ s) = 
  RoundS (pure Empty) m p (applyWinnerScore p s)


boardWinner :: Board Cell -> Maybe Player
-- X
boardWinner (Board  (Set X) (Set X) (Set X)  _ _ _  _ _ _) = Just X
boardWinner (Board  _ _ _  (Set X) (Set X) (Set X)  _ _ _) = Just X
boardWinner (Board  _ _ _  _ _ _  (Set X) (Set X) (Set X)) = Just X
boardWinner (Board  (Set X) _ _  (Set X) _ _  (Set X) _ _) = Just X
boardWinner (Board  _ (Set X) _  _ (Set X) _  _ (Set X) _) = Just X
boardWinner (Board  _ _ (Set X)  _ _ (Set X)  _ _ (Set X)) = Just X
boardWinner (Board  (Set X) _ _  _ (Set X) _  _ _ (Set X)) = Just X
boardWinner (Board  _ _ (Set X)  _ (Set X) _  (Set X) _ _) = Just X
-- O
boardWinner (Board  (Set O) (Set O) (Set O)  _ _ _  _ _ _) = Just O
boardWinner (Board  _ _ _  (Set O) (Set O) (Set O)  _ _ _) = Just O
boardWinner (Board  _ _ _  _ _ _  (Set O) (Set O) (Set O)) = Just O
boardWinner (Board  (Set O) _ _  (Set O) _ _  (Set O) _ _) = Just O
boardWinner (Board  _ (Set O) _  _ (Set O) _  _ (Set O) _) = Just O
boardWinner (Board  _ _ (Set O)  _ _ (Set O)  _ _ (Set O)) = Just O
boardWinner (Board  (Set O) _ _  _ (Set O) _  _ _ (Set O)) = Just O
boardWinner (Board  _ _ (Set O)  _ (Set O) _  (Set O) _ _) = Just O
-- No winner
boardWinner _ = Nothing
