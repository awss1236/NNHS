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

sigmoid :: Float -> Float
sigmoid x = 1/(1+exp(-x))
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
feedForwardL fs l = foldr (\(ws, b) a -> sigmoid (b + (sum $ zipWith (*) fs ws)) : a) [] l

feedForward :: [Float] -> NN -> [Float]
feedForward fs [l] = feedForwardL fs l
feedForward fs (l:ls) = feedForward (feedForwardL fs l) ls

backProp :: ([Float], [Float]) -> Float -> NN -> NN
backProp (xs, ts) lr nn = reverse $ backProp' (reverse $ feedForward' xs nn) (let (o:_) = reverse $ feedForward' xs nn in zipWith (-) o ts) nn
                        where feedForward' :: [Float] -> NN -> [[Float]]
                              feedForward' fs [l] = [feedForwardL fs l]
                              feedForward' fs (l:ls) = let os = feedForwardL fs l in os:feedForward' os ls
                              backProp' :: [[Float]] -> [Float] -> NN -> NN
                              backProp' = undefined

main :: IO ()
main = print $ createNN (mkStdGen 4) [1, 2, 3]
