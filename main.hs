

import Domino

main :: IO ()
main = do
  let startDomino = (0, 0)
  let allDominoes = getAllDominoes startDomino 12
  scoreOne <- sequence $ replicate 10 $ (sampleOptimal 12 16)
  mapM_ (putStrLn . show) scoreOne
