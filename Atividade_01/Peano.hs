{-|
Module      : Peano
Description : Implementação da Aritmética de Peano
Copyright   : (c) 2025
License     : MIT
Maintainer  : 
Stability   : experimental

Esta biblioteca implementa o sistema formal da aritmética de Peano,
incluindo a definição dos números naturais e as operações fundamentais.
-}

module Main where

import Data.List (intercalate)

-- | Tipo de dados representando os números naturais de Peano
-- Zero representa o número 0
-- Succ n representa o sucessor de n (n + 1)
data Nat = Zero | Succ Nat deriving (Eq)

-- | Instância Show para visualização dos números de Peano
instance Show Nat where
  show Zero = "Zero"
  show (Succ n) = "Succ (" ++ show n ++ ")"

-- | Converte um número de Peano para Int
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

-- | Converte um Int para número de Peano
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n
  | n > 0 = Succ (intToNat (n - 1))
  | otherwise = error "Número negativo não existe em N"

-- | Representação mais legível de um número de Peano
showNat :: Nat -> String
showNat n = "Nat(" ++ show (natToInt n) ++ ") = " ++ show n

-- | Representação compacta
showNatCompact :: Nat -> String
showNatCompact n = "S^" ++ show (natToInt n) ++ "(0)"

--------------------------------------------------------------------------------
-- OPERAÇÕES ARITMÉTICAS
--------------------------------------------------------------------------------

-- | Adição de números de Peano
-- Definição recursiva baseada nos axiomas de Peano:
-- a + 0 = a                    (A1)
-- a + S(b) = S(a + b)          (A2)
add :: Nat -> Nat -> Nat
add a Zero = a                    -- (A1)
add a (Succ b) = Succ (add a b)   -- (A2)

-- | Subtração de números de Peano
-- Definição recursiva:
-- a - 0 = a
-- S(a) - S(b) = a - b
-- 0 - S(b) = 0 (truncada, pois não temos negativos)
sub :: Nat -> Nat -> Nat
sub a Zero = a
sub Zero _ = Zero  -- subtração truncada
sub (Succ a) (Succ b) = sub a b

-- | Multiplicação de números de Peano
-- Definição recursiva baseada nos axiomas:
-- a * 0 = 0                    (M1)
-- a * S(b) = a + (a * b)       (M2)
mult :: Nat -> Nat -> Nat
mult _ Zero = Zero                      -- (M1)
mult a (Succ b) = add a (mult a b)      -- (M2)

-- | Máximo entre dois números naturais
-- max(a, 0) = a
-- max(0, b) = b
-- max(S(a), S(b)) = S(max(a, b))
maxNat :: Nat -> Nat -> Nat
maxNat a Zero = a
maxNat Zero b = b
maxNat (Succ a) (Succ b) = Succ (maxNat a b)

-- | Comparação: a <= b?
leq :: Nat -> Nat -> Bool
leq Zero _ = True
leq (Succ _) Zero = False
leq (Succ a) (Succ b) = leq a b

--------------------------------------------------------------------------------
-- FUNÇÕES DE DEMONSTRAÇÃO (mostram o comportamento passo a passo)
--------------------------------------------------------------------------------

-- | Demonstra a adição passo a passo
demonstrateAdd :: Int -> Int -> IO ()
demonstrateAdd x y = do
  let a = intToNat x
  let b = intToNat y
  putStrLn $ "\n=== Demonstração: " ++ show x ++ " + " ++ show y ++ " ==="
  putStrLn $ "a = " ++ showNat a
  putStrLn $ "b = " ++ showNat b
  putStrLn "\nPasso a passo da adição:"
  showAddSteps a b 0
  let result = add a b
  putStrLn $ "\nResultado final: " ++ showNat result
  putStrLn $ "Verificação: " ++ show (natToInt result) ++ " = " ++ show (x + y) ++ " ✓"

-- | Mostra os passos da adição
showAddSteps :: Nat -> Nat -> Int -> IO ()
showAddSteps a Zero step = do
  putStrLn $ "Passo " ++ show step ++ ": " ++ showNatCompact a ++ " + Zero = " ++ 
             showNatCompact a ++ " (por A1)"
showAddSteps a (Succ b) step = do
  putStrLn $ "Passo " ++ show step ++ ": " ++ showNatCompact a ++ " + " ++ 
             showNatCompact (Succ b) ++ " = Succ(" ++ showNatCompact a ++ " + " ++ 
             showNatCompact b ++ ") (por A2)"
  showAddSteps a b (step + 1)

-- | Demonstra a multiplicação passo a passo
demonstrateMult :: Int -> Int -> IO ()
demonstrateMult x y = do
  let a = intToNat x
  let b = intToNat y
  putStrLn $ "\n=== Demonstração: " ++ show x ++ " × " ++ show y ++ " ==="
  putStrLn $ "a = " ++ showNat a
  putStrLn $ "b = " ++ showNat b
  putStrLn "\nPasso a passo da multiplicação:"
  showMultSteps a b 0
  let result = mult a b
  putStrLn $ "\nResultado final: " ++ showNat result
  putStrLn $ "Verificação: " ++ show (natToInt result) ++ " = " ++ show (x * y) ++ " ✓"

-- | Mostra os passos da multiplicação
showMultSteps :: Nat -> Nat -> Int -> IO ()
showMultSteps a Zero step = do
  putStrLn $ "Passo " ++ show step ++ ": " ++ showNatCompact a ++ " × Zero = Zero (por M1)"
showMultSteps a (Succ b) step = do
  let prev = mult a b
  putStrLn $ "Passo " ++ show step ++ ": " ++ showNatCompact a ++ " × " ++ 
             showNatCompact (Succ b) ++ " = " ++ showNatCompact a ++ " + (" ++ 
             showNatCompact a ++ " × " ++ showNatCompact b ++ ") (por M2)"
  putStrLn $ "         = " ++ showNatCompact a ++ " + " ++ showNatCompact prev
  showMultSteps a b (step + 1)

-- | Demonstra a subtração
demonstrateSub :: Int -> Int -> IO ()
demonstrateSub x y = do
  let a = intToNat x
  let b = intToNat y
  putStrLn $ "\n=== Demonstração: " ++ show x ++ " - " ++ show y ++ " ==="
  putStrLn $ "a = " ++ showNat a
  putStrLn $ "b = " ++ showNat b
  let result = sub a b
  putStrLn $ "\nResultado: " ++ showNat result
  putStrLn $ "Verificação: " ++ show (natToInt result) ++ " = " ++ 
             show (max 0 (x - y)) ++ " ✓"

-- | Demonstra o máximo
demonstrateMax :: Int -> Int -> IO ()
demonstrateMax x y = do
  let a = intToNat x
  let b = intToNat y
  putStrLn $ "\n=== Demonstração: max(" ++ show x ++ ", " ++ show y ++ ") ==="
  putStrLn $ "a = " ++ showNat a
  putStrLn $ "b = " ++ showNat b
  let result = maxNat a b
  putStrLn $ "\nResultado: " ++ showNat result
  putStrLn $ "Verificação: " ++ show (natToInt result) ++ " = " ++ 
             show (max x y) ++ " ✓"

--------------------------------------------------------------------------------
-- REPL INTERATIVO
--------------------------------------------------------------------------------

-- | Menu principal
menu :: IO ()
menu = do
  putStrLn "\n╔════════════════════════════════════════════════════════╗"
  putStrLn "║     Biblioteca de Aritmética de Peano em Haskell      ║"
  putStrLn "╚════════════════════════════════════════════════════════╝"
  putStrLn "\nOperações disponíveis:"
  putStrLn "1. Adição (a + b)"
  putStrLn "2. Subtração (a - b)"
  putStrLn "3. Multiplicação (a × b)"
  putStrLn "4. Máximo (max a b)"
  putStrLn "5. Todos os exemplos"
  putStrLn "0. Sair"
  putStrLn "\nEscolha uma opção: "

-- | REPL principal
repl :: IO ()
repl = do
  menu
  option <- getLine
  case option of
    "1" -> do
      putStrLn "Digite o primeiro número:"
      x <- readLn :: IO Int
      putStrLn "Digite o segundo número:"
      y <- readLn :: IO Int
      demonstrateAdd x y
      repl
    "2" -> do
      putStrLn "Digite o primeiro número:"
      x <- readLn :: IO Int
      putStrLn "Digite o segundo número:"
      y <- readLn :: IO Int
      demonstrateSub x y
      repl
    "3" -> do
      putStrLn "Digite o primeiro número:"
      x <- readLn :: IO Int
      putStrLn "Digite o segundo número:"
      y <- readLn :: IO Int
      demonstrateMult x y
      repl
    "4" -> do
      putStrLn "Digite o primeiro número:"
      x <- readLn :: IO Int
      putStrLn "Digite o segundo número:"
      y <- readLn :: IO Int
      demonstrateMax x y
      repl
    "5" -> do
      runAllExamples
      repl
    "0" -> putStrLn "\nAté logo!"
    _ -> do
      putStrLn "\nOpção inválida!"
      repl

-- | Executa todos os exemplos
runAllExamples :: IO ()
runAllExamples = do
  demonstrateAdd 2 3
  demonstrateMult 2 3
  demonstrateSub 5 2
  demonstrateMax 3 5
  
  putStrLn "\n=== Exemplos Adicionais ==="
  
  let zero = Zero
  let one = Succ Zero
  let two = Succ one
  let three = Succ two
  let four = Succ three
  let five = Succ four
  
  putStrLn $ "\nZero = " ++ show zero
  putStrLn $ "Um = " ++ show one
  putStrLn $ "Dois = " ++ show two
  putStrLn $ "Três = " ++ show three
  
  putStrLn $ "\nDois + Três = " ++ show (add two three)
  putStrLn $ "Três × Dois = " ++ show (mult three two)
  putStrLn $ "Cinco - Dois = " ++ show (sub five two)
  putStrLn $ "max(Dois, Quatro) = " ++ show (maxNat two four)
  
  -- Demonstrando propriedades
  putStrLn "\n=== Verificação de Propriedades ==="
  putStrLn $ "Comutatividade: 2 + 3 = " ++ show (add two three) ++ 
             ", 3 + 2 = " ++ show (add three two) ++ 
             " → " ++ show (add two three == add three two)
  putStrLn $ "Associatividade: (2 + 3) + 1 = " ++ 
             show (add (add two three) one) ++ 
             ", 2 + (3 + 1) = " ++ show (add two (add three one)) ++ 
             " → " ++ show (add (add two three) one == add two (add three one))
  putStrLn $ "Elemento neutro: 3 + 0 = " ++ show (add three zero) ++ 
             ", 0 + 3 = " ++ show (add zero three) ++ 
             " → " ++ show (add three zero == three && add zero three == three)

-- | Main function para executar o REPL
main :: IO ()
main = repl
