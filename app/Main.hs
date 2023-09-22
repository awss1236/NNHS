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
                              in ((ws, 2*b-1), g'')

feedForwardL :: [Float] -> Layer -> [Float]
feedForwardL fs l = foldr (\(ws, b) a -> sigmoid (b + (sum $ zipWith (*) fs ws)) : a) [] l

feedForward :: [Float] -> NN -> [Float]
feedForward fs [l] = feedForwardL fs l
feedForward fs (l:ls) = feedForward (feedForwardL fs l) ls

backProp :: ([Float], [Float]) -> Float -> NN -> NN
backProp (xs, ts) lr nn = reverse $ backProp' (tail $ reverse $ feedForward' xs nn ++ [xs]) (let (o:_) = reverse $ feedForward' xs nn in zipWith (\a t -> (a-t)*a*(1-a)) o ts) (reverse nn)
                        where feedForward' :: [Float] -> NN -> [[Float]]
                              feedForward' fs [l] = [feedForwardL fs l]
                              feedForward' fs (l:ls) = let os = feedForwardL fs l in os:feedForward' os ls
                              backProp' :: [[Float]] -> [Float] -> NN -> NN
                              backProp' (oi:ros) dels (j:i:ls) = map (\((ws, b), d) -> (map (\(w, a) -> w - lr*d*a) $ zip ws oi, b - lr*d)) (zip j dels) : backProp' ros (foldr (\((ws, _), dl) acc -> zipWith (+) acc (zipWith (\w a -> w*dl*a*(1-a)) ws oi)) (replicate (length i) 0) (zip j dels)) (i:ls)
                              backProp' [oi] dels [j] = [map (\((ws, b), d) -> (map (\(w, a) -> w - lr*d*a) $ zip ws oi, b - lr*d)) (zip j dels)]

main :: IO ()
main = print $ createNN (mkStdGen 4) [1, 2, 3]
