
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
reverseE g   = [ (w, v) | (v, w) <- edges g ]

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

--dominoTraversal :: Int -> Graph -> Tree
dominoTraversal currentIndex g = 
        let potential = g!currentIndex
         in if (length potential) == 0
               -- Leaf
               then []
               else [Data.Tree.Node (currentIndex, p) (dominoTraversal p (removeDomino g (currentIndex, p))) | p <- potential]

main :: IO ()
main = do
  let allDominoes = [(x, y) | x <- [0..12], y <- [0..12], x <= y] 
  rng <- newStdGen
  let sample = shuffle' allDominoes (length allDominoes) rng

  let dominoList = (take 16 sample)
      pile = fromList  dominoList :: DominoPile
      graph = dominosToGraph pile
      chains = getPossibleChains [(1, 1)] pile

  putStrLn $ "Total Dominoes: " ++ show (dominoList)
  --putStrLn $ "Total Chains: " ++ show (length chains)
  putStrLn $ show $ graph
  putStrLn $ show $ length $ dominoTraversal 0 graph 
  putStr $ drawTree $ fmap show $ (dominoTraversal 0 graph) !! 0

