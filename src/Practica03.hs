module Practica03 where
import Data.List (nub, elem)
import Data.Foldable (all)

-- Definiciones de Tipos
data Prop = 
    Var String | Cons Bool | Not Prop | And Prop Prop | Or Prop Prop | 
    Impl Prop Prop | Syss Prop Prop 
    deriving (Eq)

instance Show Prop where
    show (Cons True)  = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p)      = p
    show (Not p)      = "¬" ++ show p 
    show (Or p q)     = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q)    = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q)   = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q)   = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

type Estado = [String]

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Funciones Auxiliares
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [(x:ys) | ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs

-- Función auxiliar para el Ejercicio 1: elimina duplicados
sinDuplicados :: Eq a => [a] -> [a]
sinDuplicados xs = [x | (x,i) <- zip xs [0..], x `notElem` take i xs]

-- Ejercicio 1: variables
variables :: Prop -> [String]
variables (Var x)     = [x]
variables (Cons _)    = [] 
variables (Not p)     = variables p
variables (And p q)   = sinDuplicados (variables p ++ variables q)
variables (Or p q)    = sinDuplicados (variables p ++ variables q)
variables (Impl p q)  = sinDuplicados (variables p ++ variables q)
variables (Syss p q)  = sinDuplicados (variables p ++ variables q)


-- Ejercicio 2: interpretacion
interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons b) estado  = b
interpretacion (Var s) estado   = s `elem` estado 
interpretacion (Not f) estado   = not (interpretacion f estado)
interpretacion (And f1 f2) estado = (interpretacion f1 estado) && (interpretacion f2 estado)
interpretacion (Or f1 f2) estado  = (interpretacion f1 estado) || (interpretacion f2 estado)
interpretacion (Impl f1 f2) estado = not (interpretacion f1 estado) || (interpretacion f2 estado)
interpretacion (Syss f1 f2) estado = (interpretacion f1 estado) == (interpretacion f2 estado)

-- Ejercicio 3: estadosPosibles
estadosPosibles :: Prop -> [Estado]
estadosPosibles f = conjuntoPotencia (variables f)

-- Ejercicio 4: modelos
modelos :: Prop -> [Estado]
modelos f = filter (\i -> interpretacion f i) (estadosPosibles f)

-- Ejercicio 6: tautologia
tautologia :: Prop -> Bool
tautologia f = length (modelos f) == length (estadosPosibles f)

-- Ejercicio 5: sonEquivalentes
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes f1 f2 = tautologia (Syss f1 f2)

-- Ejercicio 7: consecuenciaLogica
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica premisas conclusion = all (\i -> interpretacion conclusion i) modelosPremisas
  where
    -- Obtiene todas las variables relevantes
    todasLasVars = nub (concatMap variables premisas ++ variables conclusion)
    -- Genera todos los estados posibles
    todosLosEstados = conjuntoPotencia todasLasVars
    -- Filtra los estados que hacen verdaderas TODAS las premisas
    modelosPremisas = filter (\i -> all (\phi -> interpretacion phi i) premisas) todosLosEstados