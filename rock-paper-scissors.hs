-- Rock, Paper, Scissors

-- Custom Data Definition
data Move = Rock | Paper | Scissors
    deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)


-- Creation of the function using guards : Function might be imporved later
outcome :: Move -> Move -> Outcome
outcome fm sm | fm == Rock && sm == Scissors = Lose
              | fm == Scissors && sm == Paper = Win
              | otherwise = Tie
