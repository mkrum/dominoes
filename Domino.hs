
module Domino where

import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

import Data.List (maximumBy, nub)
import Data.Array (bounds, (!), (//))
import Data.Set (Set, fromList, delete, toList)
import qualified Data.Set (filter, size)
import Data.Tree (foldTree, Tree(Node))

import Data.Graph (graphFromEdges, Graph, Edge, buildG, edges)

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

getAllDominoes :: Domino -> Int -> DominoPile
getAllDominoes startDomino maxValue = fromList [(x, y) | x <- [0..maxValue], y <- [0..maxValue], x <= y, (x, y) /= startDomino] 

sampleOptimal :: Int -> Int -> IO Int
sampleOptimal maxValue numDominoes = do
        rng <- newStdGen
        let startDomino = (0, 0)
            allDominoes = toList $ getAllDominoes startDomino 12
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
                    
sampleRandom :: Int -> Int -> Int -> IO Int
sampleRandom numSamples maxValue numDominoes = do
        rng <- newStdGen
        let startDomino = (0, 0)
            allDominoes = toList $ getAllDominoes startDomino 12
            n = length allDominoes
            sample = shuffle' allDominoes (length allDominoes) rng
            dominoList = (take numDominoes sample)
            pile = fromList dominoList :: DominoPile
            dominoGraph = dominosToGraph pile
        solution <- sequence $ [randomTraversal startDomino dominoGraph | _ <- [1..numSamples]]
        let longestChain = maximum $ map length solution
        return longestChain

