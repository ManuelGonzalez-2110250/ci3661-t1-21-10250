-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Manuel González
--  Carnet: 21-10250l
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  De momento se ha usado para consultar sobre por que había un bucle infinito 
--  en la función esPalindormo (no podía modificar xs en xs)
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
      xl = take (n - 1) xs
  in 
    if(isAlphaNum(y))
     then if(toLower(x) == toLower(y))
      then 
       esPalindromo xl
      else
       False
     else
      let xi = x:xl
      in esPalindromo xi
   
  
   else
  esPalindromo xs


-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
--
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
leerHora = undefined

duracion :: String -> String -> Either String Int
duracion = undefined

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

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
