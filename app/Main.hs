module Main where
import System.Random

type Node  = ([Float], Float)
type Layer = [Node]

validateNN :: [Layer] -> Bool
validateNN (l1:l2:ls) = let n = map (length.fst) l2 in if allSame n then length l1 == head n && validateNN (l2:ls) else False
                      where allSame (x1:x2:xs) = if x1 == x2 then allSame (x2:xs) else False
                            allSame _ = True
validateNN _ = True

createNN :: StdGen -> [Int] -> ([Layer], StdGen)
createNN = createNN' 1
         where createNN' :: Int -> StdGen -> [Int] -> ([Layer], StdGen)
               createNN' p g (x:xs) = let (l, g') = foldr (\_ (ns, cg) -> let (n, ng) = randNode cg p in (n:ns, ng)) ([], g) [1..x]
                                      in (\(ls, g'') -> (l:ls, g'')) $ createNN' x g' xs
               createNN' _ g _ = ([], g)
               randNode :: StdGen -> Int -> (Node, StdGen)
               randNode g p = let (ws, g') = randomSeq g p
                                  (b, g'') = random g'
                              in ((ws, b), g'')

randomSeq :: Random a => StdGen -> Int -> ([a], StdGen)
randomSeq g 0 = ([], g)
randomSeq g n = let (r, g') = random g in (\(a, b) -> (r:a, b)) $ randomSeq g' (n-1)

main :: IO ()
main = print $ createNN (mkStdGen 4) [1, 2, 3]
