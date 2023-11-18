
import Domino

import Data.List (minimumBy)
import Data.Ord (comparing)

type Algo = Domino -> DominoPile -> IO DominoChain

argmin xs = snd $ minimumBy (comparing fst) (zip xs [0..])

optimalAlgo :: Algo
optimalAlgo startDomino pile = return (fst $ (findOptimalChain startDomino pile))

greedyAlgo :: Algo
greedyAlgo startDomino pile = return (greedyTraversal startDomino (dominosToGraph pile))

randomAlgo :: Algo
randomAlgo startDomino pile = randomTraversal startDomino (dominosToGraph pile)

randomNAlgo :: Int -> Algo
randomNAlgo n startDomino pile = sampleRandom n startDomino pile

getFinalScore :: Algo -> Domino -> DominoPile -> IO Int
getFinalScore algo startDomino dominoes = do
        chain <- algo startDomino dominoes
        let startingScore = getStartingScore dominoes
            pointsRemaining = startingScore - (getScore chain)
         in return pointsRemaining

simGame :: Int -> IO Int
simGame nSamples = do
  let startDomino = (0, 0)
      dominoes = getAllDominoes 12
      algos = [optimalAlgo, optimalAlgo, (randomNAlgo nSamples), (randomNAlgo nSamples)]
      fns = map (\x -> getFinalScore x startDomino) algos

  splits <- shuffleAndSplitDominoes dominoes startDomino 4
  scores <- mapM (\x -> (fst x) (snd x)) (zip fns splits)

  let winner = argmin scores
  return winner


main :: IO ()
main = do
    let nGames = 100
    values <- sequence $ [simGame 20 | _ <- [1..nGames]]
    let numWins = [length $ filter (\x -> x == i) values | i <- [0..3]]
    putStrLn (show numWins)
