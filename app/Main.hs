module Main where
import System.Random

type Node  = ([Float], Float)
type Layer = [Node]
type NN    = [Layer]

validateNN :: NN -> Bool
validateNN (l1:l2:ls) = let n = map (length.fst) l2 in if allSame n then length l1 == head n && validateNN (l2:ls) else False
                      where allSame (x1:x2:xs) = if x1 == x2 then allSame (x2:xs) else False
                            allSame _ = True
validateNN _ = True

randomSeq :: Random a => StdGen -> Int -> ([a], StdGen)
randomSeq g 0 = ([], g)
randomSeq g n = let (r, g') = random g in (\(a, b) -> (r:a, b)) $ randomSeq g' (n-1)

createNN :: StdGen -> [Int] -> (NN, StdGen)
createNN = createNN' 1
         where createNN' :: Int -> StdGen -> [Int] -> ([Layer], StdGen)
               createNN' p g (x:xs) = let (l, g') = foldr (\_ (ns, cg) -> let (n, ng) = randNode cg p in (n:ns, ng)) ([], g) [1..x]
                                      in (\(ls, g'') -> (l:ls, g'')) $ createNN' x g' xs
               createNN' _ g _ = ([], g)
               randNode :: StdGen -> Int -> (Node, StdGen)
               randNode g p = let (ws, g') = randomSeq g p
                                  (b, g'') = random g'
                              in ((ws, b), g'')

feedForwardL :: [Float] -> Layer -> [Float]
feedForwardL fs l = foldr (\(ws, b) a -> (b + (sum $ zipWith (*) fs ws)) : a) [] l

feedForward :: [Float] -> NN -> [Float]
feedForward fs [l] = feedForwardL fs l
feedForward fs (l:ls) = feedForward (feedForwardL fs l) ls

main :: IO ()
main = print $ createNN (mkStdGen 4) [1, 2, 3]
