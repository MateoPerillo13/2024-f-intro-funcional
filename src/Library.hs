module Library where
import PdePreludat

-- Enunciado Parte 1

totalEmpleados :: Number -> String -> Number
totalEmpleados cantidadSucursales nombreEmpresa = cantidadEmpleadosPorSucursal nombreEmpresa * cantidadSucursales

terminaConLetraMenor :: String -> Bool
terminaConLetraMenor palabra = head palabra > last palabra

letrasIntermedias :: String -> Number
letrasIntermedias palabra = (length palabra) - 2

esCapicua :: String -> Bool
esCapicua palabra
    | (head palabra == last palabra) && (length palabra > 2) = esCapicua(tail (init palabra))
    | (head palabra == last palabra) = True
    | otherwise = False


nombreEsPar :: String -> Bool
nombreEsPar nombre = rem (length nombre) 2 == 0

esMultiploDe :: String -> Number -> Bool 
esMultiploDe palabra multiplo = rem (length palabra) multiplo == 0



cantidadEmpleadosPorSucursal :: String -> Number
cantidadEmpleadosPorSucursal "acme" = 10 --Pattern matching

cantidadEmpleadosPorSucursal nombreEmpresa --Guardas
    | terminaConLetraMenor nombreEmpresa = letrasIntermedias nombreEmpresa
    | esCapicua nombreEmpresa && nombreEsPar nombreEmpresa = letrasIntermedias nombreEmpresa * 2
    | esMultiploDe nombreEmpresa 3 || esMultiploDe nombreEmpresa 7 = 3
    | otherwise = 0

--Tarea: Implementar funciones, y la condicion que falta (esMultiploDe)









-- Enunciado Parte 2 - "esBisiesto"

esBisiesto :: Number -> Bool
esBisiesto anio = ((rem anio 400) == 0 || (((rem anio 4) == 0) && (not ((rem anio 100) == 0))))