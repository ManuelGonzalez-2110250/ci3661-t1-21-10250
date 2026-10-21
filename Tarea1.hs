-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Manuel González
--  Carnet: 21-10250l
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  De momento se ha usado para consultar sobre por que había un bucle infinito 
--  en la función esPalindormo (no podía modificar xs en xs)
--  Tambien sobre que significaba la pista de A3
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo [] = True
esPalindromo [x] = True
esPalindromo (x:xs) =
 if (isAlphaNum (x)) 
 then 
  let y = last(xs)
      n = length(xs)
      --eliminar el último
      xl = take (n - 1) xs
  in 
    if(isAlphaNum(y))
     then if(toLower(x) == toLower(y))
      then
       --se procesa sin el primero ni el último
       esPalindromo xl
      else
       False
     else
     --se añade el primero de nuevo
      let xi = x:xl
      --se procesa sin el último
      in esPalindromo xi
   
  
   else
  --se procesa sin el primero
  esPalindromo xs


-- A2
-- Se recorre una sola vez, primero se intentan agarrar 1 o 2 elementos (primero
-- y/o último) y asumiendo que solo se tome 1 (peor caso) al final son n iteraciones
-- por lo que al final, se recorre una sola vez 
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor [x] = Just (x,x)
mayorYMenor (x:xs)
 | x > mayor = Just (x,menor)
 | x < menor = Just (mayor,x)
 | otherwise = Just (mayor,menor)
 where Just (mayor,menor) = mayorYMenor xs

-- En el mismo recorrdio verifica si es mayor o menor, y cambia la tupla si lo es. 
-- esto se debe a que nunca se hace otra llamada recursiva para que maneje los menores\mayores
-- por separado.
leerHora :: String -> Either String (Int, Int)
leerHora [] = Left "formato inválido"  
leerHora [a, b, ':', c, d] = 
 let h = digitToInt(a)*10 + digitToInt(b)
     m = digitToInt(c)*10 + digitToInt(d)
 in if(h > 23)
     then Left "hora fuera de rango"
    else if (m > 59)
     then Left "minutos fuera de rango"
    else Right (h,m)
leerHora _ = Left "formato inválido" 
     

duracion :: String -> String -> Either String Int
duracion _ [] = Left "formato inválido"
duracion [] _ = Left "formato inválido"
duracion x y  = 
 let x1 = leerHora x
     y1 = leerHora y
 in case (x1,y1) of
  (Right (a,b), Right (c,d)) -> 
   let minX = a*60 + b
       minY = c*60 + d
   in if(minY > minX)
       then Right (minY - minX)
      else Right ((minY + 1440) - minX)
  (Left x2, Right (c,d)) -> Left x2
  (Right (a,b), Left y2) -> Left y2
  (Left x2, Left y2)-> Left x2
 

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral _ [] = Nothing
promedioSobreUmbral x xs = 
  let acum = foldl (\acc y -> if(x < y) then acc + y else acc) 0.0 xs
      nMax = foldl (\acc y -> if(x < y) then acc + 1 else acc) 0.0 xs
      tot = (acum / nMax)
      
  in if (nMax == 0.0)
   then Nothing
   else
    Just tot

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
