module Dec2 where

import Data.List(unfoldr, sortOn)
import Data.Ord(Down(..))

run :: String -> IO ()
run input =
  do
    putStrLn "Task 1:"
    print $ process parseRound1 $ lines input
    putStrLn "Task 2:"
    print $ process parseRound2 $ lines input

process :: (String -> Round) -> [String] -> Int
process parseFun lines = sum $ (score . parseFun) <$> lines

data Move = Rock | Paper | Scissors
  deriving Show

data Round = Round {
  oppMove :: Move,
  myMove  :: Move }
  deriving Show

parseRound1 :: String -> Round
parseRound1 s =
  let oppMove = parseOppMove (s !! 0)
      myMove  = parseMyMove (s !! 2)
  in Round oppMove myMove

parseOppMove :: Char -> Move
parseOppMove 'A' = Rock
parseOppMove 'B' = Paper
parseOppMove 'C' = Scissors

parseMyMove :: Char -> Move
parseMyMove 'X' = Rock
parseMyMove 'Y' = Paper
parseMyMove 'Z' = Scissors

score :: Round -> Int
score round = (scoreOutcome round) + (scoreMove $ myMove round)
  where
    win  = 6
    draw = 3
    loss = 0

    scoreOutcome (Round Rock Rock)     = draw
    scoreOutcome (Round Rock Paper)    = win
    scoreOutcome (Round Rock Scissors) = loss

    scoreOutcome (Round Paper Rock)     = loss
    scoreOutcome (Round Paper Paper)    = draw
    scoreOutcome (Round Paper Scissors) = win

    scoreOutcome (Round Scissors Rock)     = win
    scoreOutcome (Round Scissors Paper)    = loss
    scoreOutcome (Round Scissors Scissors) = draw

    scoreMove Rock     = 1
    scoreMove Paper    = 2
    scoreMove Scissors = 3

parseRound2 :: String -> Round
parseRound2 s =
  let oppMove = parseOppMove (s !! 0)
      myMove  = react (s !! 2) oppMove
  in Round oppMove myMove

react :: Char -> Move -> Move
react 'X' = loseAgainst
react 'Y' = drawAgainst
react 'Z' = winAgainst

loseAgainst :: Move -> Move
loseAgainst Rock     = Scissors
loseAgainst Paper    = Rock
loseAgainst Scissors = Paper

drawAgainst :: Move -> Move
drawAgainst = id

winAgainst :: Move -> Move
winAgainst Rock     = Paper
winAgainst Paper    = Scissors
winAgainst Scissors = Rock
