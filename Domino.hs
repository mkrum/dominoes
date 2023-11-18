
module Domino where

import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.Array (bounds, (!), (//))
import Data.Set (Set, fromList, delete, toList)
import qualified Data.Set (filter, size)
import Data.Tree (foldTree, Tree(Node))

import Data.Graph (graphFromEdges, Graph, Edge, buildG, edges, outdegree)

import Data.List.Split (chunksOf)

type Domino = (Int, Int)
type DominoChain = [Domino]
type DominoPile = Set Domino

isConnectableDomino :: Domino -> Domino -> Bool
isConnectableDomino (a, b) (c, d) = b == c || b == d

isConnectable :: DominoChain -> Domino -> Bool
isConnectable [] domino = True
isConnectable chain domino = isConnectableDomino (last chain) domino

addToChain :: DominoChain -> Domino -> DominoChain
addToChain domino1 domino2@(a, b) 
  | snd (last domino1) == a = domino1 ++ [domino2]
  | snd (last domino1) == b = domino1 ++ [(b, a)]
  | otherwise = [(-1, -1)]

-- From the library but not exposed for some reason
undirected :: Graph -> Graph
undirected g  = buildG (bounds g) (edges g ++ reverseENoRepeat g)

-- Flip the edges, but don't include edges that connect a node to itself
reverseENoRepeat  :: Graph -> [Edge]
reverseENoRepeat g   = [ (w, v) | (v, w) <- edges g , v /=  w]

dominosToGraph :: DominoPile -> Graph
dominosToGraph dominos = 
        let dominosList = toList dominos :: [(Int, Int)]
            dominosVerteces = nub $ (map fst dominosList) ++ (map snd dominosList)
            getEdges = \y -> map snd (Prelude.filter (\x -> fst x == y) dominosList) 
            (graph, _, _) = graphFromEdges [(x, x, getEdges x) | x <- [0..12]]
        in undirected graph

removeDomino :: Graph -> Domino -> Graph
removeDomino g domino = 
        let (sideOne, sideTwo) = domino
            newEdgesOne = [e | e <- g!sideOne, e /= sideTwo] 
            newEdgesTwo = [e | e <- g!sideTwo, e /= sideOne] 
        in g // [(sideOne, newEdgesOne), (sideTwo, newEdgesTwo)]

dominoTraversal :: (Int, Int) -> Graph -> Tree (Int, Int)
dominoTraversal currentDomino@(_, follow) g = 
         Data.Tree.Node currentDomino [dominoTraversal (follow, p) (removeDomino g (follow, p)) | p <- g!follow]

findOptimalChain :: Domino -> DominoPile -> (DominoChain, Int)
findOptimalChain startDomino pile = foldTree foldFn dominoChains
    where dominoGraph = dominosToGraph pile
          dominoChains = dominoTraversal startDomino dominoGraph

          foldFn :: Domino -> [(DominoChain, Int)] -> (DominoChain, Int)
          foldFn x@(x1, x2) xs = 
                  let mySum = x1 + x2
                      xsWithNothing = ([], 0):xs
                      (bestChain, bestSum) = maximumBy (\x y -> compare (snd x) (snd y)) xsWithNothing
                  in (x:bestChain, mySum + bestSum)

getAllDominoes :: Int -> DominoPile
getAllDominoes maxValue = fromList [(x, y) | x <- [0..maxValue], y <- [0..maxValue], x <= y] 

getAllDominoesExcept :: Domino -> Int -> DominoPile
getAllDominoesExcept startDomino maxValue = fromList [(x, y) | x <- [0..maxValue], y <- [0..maxValue], x <= y, (x, y) /= startDomino] 

sampleDominoes :: Domino -> Int -> IO DominoPile
sampleDominoes startDomino sampleSize = do
        rng <- newStdGen
        let allDominoes = toList $ getAllDominoesExcept startDomino 12
            n = length allDominoes
            sample = shuffle' allDominoes (length allDominoes) rng
            dominoList = (take sampleSize sample)
            pile = fromList dominoList :: DominoPile
        return pile

getScore :: DominoChain -> Int
getScore pile = sum $ map (\x -> (fst x) + (snd x)) pile

getStartingScore :: DominoPile -> Int
getStartingScore pile = let dominoes = toList pile :: DominoChain
                         in getScore dominoes

sampleOptimal :: Int -> Int -> IO Int
sampleOptimal maxValue numDominoes = do
        rng <- newStdGen
        let startDomino = (0, 0)
            allDominoes = toList $ getAllDominoesExcept startDomino 12
            n = length allDominoes
            sample = shuffle' allDominoes (length allDominoes) rng
            dominoList = (take numDominoes sample)
            pile = fromList dominoList :: DominoPile
            (solution, score) = findOptimalChain startDomino pile
        return (length solution)

randomTraversal :: (Int, Int) -> Graph -> IO [(Int, Int)]
randomTraversal currentDomino@(_, follow) g = 
            if (length (g!follow)) > 0
               then do
                    rng <- newStdGen
                    let potential = g!follow
                        sample = shuffle' potential (length potential) rng
                        next = ((take 1 sample) !! 0) :: Int
                    continue <- randomTraversal (follow, next) (removeDomino g (follow, next))
                    let chain = currentDomino: continue
                    return chain
               else do
                    return [currentDomino]

greedyTraversal :: (Int, Int) -> Graph -> [(Int, Int)]
greedyTraversal currentDomino@(_, follow) g = 
            if (length (g!follow)) > 0
               then 
                    let potential = g!follow
                        next = maximum potential
                        continue = greedyTraversal (follow, next) (removeDomino g (follow, next))
                        chain = currentDomino: continue
                     in chain
               else [currentDomino]
                    
sampleRandom :: Int -> Domino -> DominoPile -> IO DominoChain
sampleRandom numSamples startDomino pile = do
        rng <- newStdGen
        let startDomino = (0, 0)
            dominoGraph = dominosToGraph pile
        solution <- sequence $ [randomTraversal startDomino dominoGraph | _ <- [1..numSamples]]
        let longestChain = maximumBy (comparing getScore) solution
        return longestChain

degrees :: Graph -> [Int]
degrees graph = 
        let maxValue = (snd . bounds) graph
         in [length (graph!i) | i <- [0..maxValue]]

hasPerfectChain :: Graph -> Bool
hasPerfectChain graph = ((totalOddDegrees == 0) || (totalOddDegrees == 2))
        where isOdd x = (x `mod` 2) ==  1
              graphDegrees = degrees graph
              oddDegreess = filter isOdd graphDegrees
              totalOddDegrees = length oddDegreess

shuffleAndSplitDominoes :: DominoPile -> Domino -> Int -> IO [DominoPile]
shuffleAndSplitDominoes pile startDomino numPlayers = do
        rng <- newStdGen
        let totalPile = toList pile :: [Domino]
            totalDominoes = length totalPile

            shuffledPile = shuffle' totalPile totalDominoes rng

            dominoesPerPlayer = totalDominoes `div` numPlayers
            splits = (chunksOf dominoesPerPlayer shuffledPile) ++ [[]] 
            playerPiles = take numPlayers splits
            extra = (drop numPlayers splits) !! 0
            addExtra = map (\x -> (fst x) ++ [(snd x)]) (zip (take (length extra) playerPiles) extra)
            allSplits = addExtra ++ (drop (length extra) playerPiles)
            allSplitsWithoutStart = map (filter (\x -> x /= startDomino)) allSplits
            allPiles = map (\x -> fromList x :: DominoPile) allSplits

            shuffledPiles = shuffle' allPiles numPlayers  rng

         in return shuffledPiles
