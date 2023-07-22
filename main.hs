
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.Array (bounds, (!), (//))
import Data.Set (Set, fromList, delete, filter, map, toList, size)
import Data.Tree (foldTree, drawTree, Tree(Node))

import Data.Graph (graphFromEdges, Graph, Edge, buildG, edges, dfs, dff)

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

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

getPossibleChains :: DominoChain -> DominoPile -> [(DominoChain, DominoPile)]
getPossibleChains chain dominos = 
        let
            possible = Data.Set.filter (isConnectable chain) dominos
            total = if 
                       (Data.Set.size possible) > 0 
                    then 
                        Prelude.map (\d -> getPossibleChains (addToChain chain d) (delete d dominos)) (toList possible) 
                    else 
                        [[(chain, dominos)]]
        in flatten total

-- From the library but not exposed for some reason
undirected :: Graph -> Graph
undirected g  = buildG (bounds g) (edges g ++ reverseE g)

reverseE  :: Graph -> [Edge]
reverseE g   = [ (w, v) | (v, w) <- edges g , v /=  w]

dominosToGraph :: DominoPile -> Graph
dominosToGraph dominos = 
        let dominosList = toList dominos :: [(Int, Int)]
            dominosVerteces = nub $ (Prelude.map fst dominosList) ++ (Prelude.map snd dominosList)
            getEdges = \y -> Prelude.map snd (Prelude.filter (\x -> fst x == y) dominosList) 
            (graph, _, _) = graphFromEdges [(x, x, getEdges x) | x <- [0..12]]
        in undirected graph

removeDomino :: Graph -> Domino -> Graph
removeDomino g domino = 
        let (sideOne, sideTwo) = domino
            newEdgesOne = [e | e <- g!sideOne, e /= sideTwo] 
            newEdgesTwo = [e | e <- g!sideTwo, e /= sideOne] 
        in g // [(sideOne, newEdgesOne), (sideTwo, newEdgesTwo)]

dominoTraversal :: (Int, Int) -> Graph -> Tree (Int, Int)
dominoTraversal currentDomino g = 
        let (_, follow) = currentDomino
         in Data.Tree.Node currentDomino [dominoTraversal (follow, p) (removeDomino g (follow, p)) | p <- g!follow]

foldFn :: Domino -> [(DominoChain, Int)] -> (DominoChain, Int)
foldFn x [] = 
        let mySum = (fst x) + (snd x)
         in ([x], mySum)

foldFn x xs = 
        let mySum = (fst x) + (snd x)
            (bestChain, bestSum) = maximumBy (\x y -> compare (snd x) (snd y)) xs
        in (x:bestChain, mySum + bestSum)

main :: IO ()
main = do
  let allDominoes = [(x, y) | x <- [0..12], y <- [0..12], x <= y] 
  rng <- newStdGen
  let sample = shuffle' allDominoes (length allDominoes) rng

  let dominoList = (take 32 sample)
      pile = fromList dominoList :: DominoPile
      graph = dominosToGraph pile
      --chains = getPossibleChains [(0, 0)] pile
      chainsG = dominoTraversal (0, 0) graph

  putStrLn $ "Total Dominoes: " ++ show (dominoList)
  --putStrLn $ "Total Chains: " ++ show (length chains)

  --putStrLn $ show $ foldTree (\x xs -> if null xs then (fst x) + (snd x) else (fst x) + (snd x) + (maximum xs)) chainsG
  putStrLn $ show $ foldTree foldFn chainsG
  --putStrLn $ show $ graph
  --putStr $ drawTree $ fmap show $ chainsG


