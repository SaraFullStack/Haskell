-- (a) Una función genera que, dado un número n y una longitud l, genere una lista con los números en [10*n..10*n+9] que son  divisibles por l.
-- La funcion genera recibe un numero y una longitud, de esta forma genera los numeros polidivisibles en el intervalo [10*n..10*n+9]
-- Si la longitud es 1, devuelve el numero dado que un numero de longitud 1 es polidivisible
-- Si la longitud es 2 o mas, genera todos los numeros en el intervalo mencionado y comprueba si son divisibles entre la longitud, solo agrega al array los que si son polidivisibles
genera :: Int -> Int -> [Int]
genera n l
  | l == 1 = [n]
  | otherwise = [x | x <- [10 * n .. 10 * n + 9], x `mod` l == 0]

-- [ x | x <-[ 10*n..10*n+9 ], x  `mod` l == 0 ] Genera todos los numeros de la longitud a comprobar a partir de los numeros polidivisibles recibidos

-- (b) Una función siguiente que, dada una lista con los números polidivisibles de n cifras, genere la lista de números polidivisibles de (n+1) cifras. Utilice para ello la función genera.
-- La funcion siguiente recibe un array de numeros polidivisibles y calcula los numeros polidivisibles de la longitud siguiente a la maxima que contiene
-- Si el array es vacio, genera los numeros del 0 al 9 de longitud 1
-- Si no es vacio genera los numeros polidivisibles de la longitud siguiente a la maxima que contiene, solo envia los numeros que contiene en el array
siguiente :: [Int] -> [Int]
siguiente [] = [x | n <- [0 .. 9], x <- genera n 1]
siguiente ns = [x | n <- ns, x <- genera n (length (show (maximum ns)) + 1), x `notElem` ns]

-- [ x | n <- [0..9], x <- genera n 1 ] Genera los numeros polidivisibles en el intervalo [0..9] de longitud 1
-- [ x | n <- ns, x <- genera n (length (show ( maximum ns )) + 1), x `notElem` ns ] Genera los numeros polidivisibles de la longitud mas alta que haya en el array mas 1, y como numero envia todos los numeros que tenemos para generar los nuevos numeros polidivisibles

-- (c) Una función listapolidivislbes que de devuelva la lista de todos los números polidivisibles. Utilice para ello,la función siguiente.
-- Llama recursivamente a siguiente con un array vacio, y se va concatenando el resultado, obteniendo asi todos los numeros polidivisibles
listapolidivisibles :: [Int]
listapolidivisibles = concat (iterate siguiente [])

main :: IO ()
main = do
  print listapolidivisibles
