
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Set (Set, fromList, delete, filter, map, toList, size)

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
            total = if (Data.Set.size possible) > 0 then Prelude.map (\d -> getPossibleChains (addToChain chain d) (delete d dominos)) (toList possible) else [[(chain, dominos)]]
        in flatten total

main :: IO ()
main = do
  let allDominoes = [(x, y) | x <- [0..12], y <- [0..12], x <= y] 
  rng <- newStdGen
  let sample = shuffle' allDominoes (length allDominoes) rng

  let pile = fromList (take 16 sample) :: DominoPile
      chains = getPossibleChains [(1, 1)] pile

  putStrLn $ "Total Dominoes: " ++ show (length allDominoes)
  putStrLn $ "Optimal chain: " ++ show (length chains)
