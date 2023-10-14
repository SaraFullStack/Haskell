module StockControl where

data Stock = ROOTNODE [Stock] | INNERNODE Char [Stock] | INFONODE Int
  deriving (Show, Read, Eq)

-------------------------
-- FUNCIÓN CREATESTOCK --
-------------------------

-- FUNCIÓN QUE DEVUELVE UN STOCK VACÍO --
createStock :: Stock -- Declaración de la función createStock, que devuelve un Stock.
createStock = ROOTNODE [] -- Implementación de la función createStock. Crea un nodo raíz ROOTNODE sin hijos.

---------------------------
-- FUNCIÓN RETRIEVESTOCK --
---------------------------

-- FUNCIÓN QUE DEVUELVE EL NÚMERO DE UNIDADES DE UN PRODUCTO EN EL STOCK --
-- SI NO ESTÁ, DEBERÁ DEVOLVER -1                                        --

retrieveStock :: Stock -> String -> Int -- Declaración de la función retrieveStock, que toma un Stock y una cadena String y devuelve un entero Int.
retrieveStock (ROOTNODE cs) xs = --Implementación de retrieveStock. Toma un Stock y un nombre de producto y devuelve la cantidad de unidades de ese producto en el Stock
  if null xs
    then -1
    else retrieveStock' cs xs --Si el nombre del producto no está vacío, llama a la función auxiliar retrieveStock' con los hijos del nodo raíz y el nombre del producto.
  where
    retrieveStock' :: [Stock] -> String -> Int --Declaración de la función auxiliar retrieveStock'
    retrieveStock' [] _ = -1 --Si se ha agotado la lista de Stock para buscar y no se ha encontrado el producto, devuelve -1.
    retrieveStock' (INNERNODE c cs : cs') xs@(x : xs')
      | c == x = retrieveStock' cs xs' -- Si el carácter en el INNERNODE coincide con el primer carácter del nombre del producto, continúa la búsqueda en los hijos.
      | otherwise = retrieveStock' cs' xs -- Si no coincide, continúa la búsqueda en el resto de la lista de Stock.
    retrieveStock' (INFONODE n : _) [] = n --Si se ha agotado la lista de caracteres para buscar y se ha encontrado un nodo de información INFONODE, devuelve el entero asociado con ese nodo.
    retrieveStock' (INFONODE _ : cs') xs = retrieveStock' cs' xs -- Si todavía quedan caracteres en el nombre del producto pero se ha encontrado un nodo de información, continúa la búsqueda en el resto de la lista de Stock.
    retrieveStock' _ _ = -1 -- Condición de error: ninguna de las condiciones anteriores se cumple.

-------------------------
-- FUNCIÓN UPDATESTOCK --
-------------------------

-- FUNCIÓN QUE MODIFICA EL VALOR ASOCIADO A UN PRODUCTO EN EL STOCK --
-- SÓLO PUEDE ALMACENAR NÚMEROS MAYORES O IGUALES A 0               --

updateStock :: Stock -> String -> Int -> Stock --Declaración de la función updateStock, que toma un Stock, una cadena String y un entero Int y devuelve un Stock.
updateStock (ROOTNODE cs) p u --Implementación de updateStock. Toma un Stock, un nombre de producto y una cantidad de unidades y actualiza el valor asociado al producto en el Stock.
  | null p = ROOTNODE cs
  | otherwise = ROOTNODE (updateStock' cs p u)
  where
    updateStock' :: [Stock] -> String -> Int -> [Stock] --Declaración de la función auxiliar updateStock' 
    updateStock' [] p u
      | null p = [] -- Si la lista de Stock está vacía y todavía quedan caracteres en el nombre del producto, crea un nuevo INNERNODE para continuar la actualización.
      | otherwise = [INNERNODE (head p) (if null (tail p) then [INFONODE u] else updateStock' [] (tail p) u)]
    updateStock' (INNERNODE c cs' : cs) p u 
      | null p = INNERNODE c cs' : cs -- Si el primer carácter del nombre del producto coincide con el carácter en el INNERNODE, actualiza el Stock en los hijos del INNERNODE.
      | c == head p =
          if null (tail p)
            then case cs' of
              [] -> INNERNODE c [INFONODE u] : cs -- Si no quedan más caracteres en el nombre del producto, actualiza el INNERNODE actual con el valor de la unidad y agrega el nuevo INNERNODE al Stock.
              (INFONODE n : cs'') -> INNERNODE c (INFONODE u : cs'') : cs -- Si se encuentra un nodo de información en el INNERNODE actual, actualiza el valor del nodo con la unidad y continúa en el resto de la lista de Stock.
              _ -> INNERNODE c (updateStock' cs' (tail p) u) : cs -- Si no se cumple ninguna de las condiciones anteriores, continúa la actualización en el resto de la lista de Stock.
            else INNERNODE c (updateStock' cs' (tail p) u) : cs -- Si todavía quedan caracteres en el nombre del producto, continúa la actualización en los hijos del INNERNODE.
      | c > head p = INNERNODE (head p) (if null (tail p) then [INFONODE u] else updateStock' [] (tail p) u) : INNERNODE c cs' : cs -- Si el carácter en el INNERNODE es mayor que el primer carácter del nombre del producto, crea un nuevo INNERNODE con el primer carácter del nombre del producto y continúa la actualización en el resto de la lista de Stock.
      | otherwise = INNERNODE c cs' : updateStock' cs p u -- Si el carácter en el INNERNODE es menor que el primer carácter del nombre del producto, continúa la actualización en el resto de la lista de Stock.

-----------------------
-- FUNCIÓN LISTSTOCK --
-----------------------

-- FUNCIÓN QUE DEVUELVE UNA LISTA PARES PRODUCTO-EXISTENCIA --
-- DEL CATÁLOGO QUE COMIENZAN POR LA CADENA PREFIJO p       --
listStock :: Stock -> String -> [(String, Int)]
listStock s prefix = listStock' s prefix ""
  where
    listStock' :: Stock -> String -> String -> [(String, Int)] -- Declaración de la función auxiliar listStock'
    listStock' (ROOTNODE cs) prefix acc
      | null cs = [] -- Si no hay hijos en el nodo raíz, devuelve una lista vacía.
      | otherwise = concatMap (\c -> listStock' c prefix acc) cs -- Realiza la búsqueda en cada uno de los hijos del nodo raíz.
    listStock' (INNERNODE c cs) prefix acc =
      concatMap (\c' -> listStock' c' prefix (acc ++ [c])) cs -- Realiza la búsqueda en cada uno de los hijos del INNERNODE, añadiendo el carácter del INNERNODE a la cadena acumulada.
    listStock' (INFONODE n) prefix acc
      | prefix `isPrefixOf` acc = [(acc, n)] -- Si el prefijo coincide con la cadena acumulada, agrega el par (cadena acumulada, valor del nodo) a la lista de resultados.
      | otherwise = [] -- Si no coincide, devuelve una lista vacía.

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True -- Si la primera cadena está vacía, devuelve True.
isPrefixOf _ [] = False -- Si la segunda cadena está vacía, devuelve False.
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys -- Comprueba si el primer carácter de la primera cadena es igual al primer carácter de la segunda cadena y si el resto de la primera cadena es un prefijo del resto de la segunda cadena.

-- FUNCIÓN GENÉRICA DE BACKTRACKING --
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt eS c n
  | eS n = [n]
  | otherwise = concat (map (bt eS c) (c n))
