genera :: Int -> Int -> [Int]
genera n l = [10*n + d | d <- [0..9], (10*n + d) `mod` l == 0]

siguiente :: [Int] -> [Int]
siguiente ns = [10*n + d | n <- ns, d <- [0..9], all (\x -> (10*n + d) `mod` x == 0) [2..length (show (10*n + d))]]

listapolidivisibles :: [Int]
listapolidivisibles = concat (take 3 (iterate siguiente [1,2,3,4,5,6,7,8,9]))

main :: IO ()
main = do
    putStrLn "Polidivisibles:"
    print listapolidivisibles
