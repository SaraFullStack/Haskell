-- (a) Una función genera que, dado un número n y una longitud l, genere una lista con los números en [10*n..10*n+9] que son  divisibles por l.
genera :: Int -> Int -> [Int] -- Recibe dos argumentos y devuelve una lista te enteros en este intervalo: [10*n, 10*n+9]
genera n l =
  [x | x <- [10 * n .. 10 * n + 9], x `mod` l == 0]

-- l es la longitud
--  [10 * n .. 10 * n + 9] del numero 10-19 si n es 1.
-- x `mod` l == 0 divisibles por l con resto 0.
-- x | es igual a el resultado de lo primero x <- [10 * n .. 10 * n + 9] y lo segundo x `mod` l == 0

-- (b) Una función siguiente que, dada una lista con los números polidivisibles de n cifras, genere la lista de números polidivisibles de (n+1) cifras. Utilice para ello la función genera.
siguiente :: [Int] -> [Int] -- Recive una lista de enteros polidivisibles de n y genera una lista de polidivisibles de n+1
siguiente ns =
  [n * 10 + d | n <- ns, d <- [0 .. 9], (n * 10 + d) `elem` genera n (length (show (n * 10 + d)))]

-- n <- ns los numeros de ns son n
-- n * 10 + d -> genera los numeros de n+1, crear listas numeros enteros de varias cifras
-- genera n (length (show (n * 10 + d + 1))) devuelvo los polidivisibles
-- elem -> valida que los elementos esten en la lista de (n * 10 + d) y genera n (length (show (n * 10 + d + 1)))

-- (c) Una función listapolidivislbes que de devuelva la lista de todos los números polidivisibles. Utilice para ello,la función siguiente.
-- take devuelve el número de cifras que queremos que calcule los polidivisibles, por que itera con esa cantidad de números
-- concat une varias listas en una sola.
listapolidivisibles :: [Int] -- Devuelve una lista de enteros
listapolidivisibles = concat (iterate siguiente [1, 2, 3, 4, 5, 6, 7, 8, 9])

main :: IO () -- Inicio de ejecución
main = do
  putStrLn "Polidivisibles:" -- Imprime sin comillas
  print listapolidivisibles -- Imprime resultado de la función
