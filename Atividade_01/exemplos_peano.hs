-- Exemplos de Uso da Biblioteca Peano
-- Para executar: nix-shell --run "ghci Peano.hs"
-- Depois carregue este arquivo com: :load exemplos_peano.hs

import Main

-- Definindo alguns números para reutilização
zero = Zero
um = Succ Zero
dois = Succ (Succ Zero)
tres = Succ (Succ (Succ Zero))
quatro = intToNat 4
cinco = intToNat 5
seis = intToNat 6
sete = intToNat 7
oito = intToNat 8
nove = intToNat 9
dez = intToNat 10

-- Exemplo 1: Operações Básicas
exemplo1 :: IO ()
exemplo1 = do
  putStrLn "\n=== Exemplo 1: Operações Básicas ==="
  putStrLn $ "2 + 3 = " ++ show (natToInt (add dois tres))
  putStrLn $ "5 - 2 = " ++ show (natToInt (sub cinco dois))
  putStrLn $ "3 × 4 = " ++ show (natToInt (mult tres quatro))
  putStrLn $ "max(7, 3) = " ++ show (natToInt (maxNat sete tres))

-- Exemplo 2: Propriedades da Adição
exemplo2 :: IO ()
exemplo2 = do
  putStrLn "\n=== Exemplo 2: Propriedades da Adição ==="
  
  -- Comutatividade
  let a_mais_b = add dois tres
  let b_mais_a = add tres dois
  putStrLn $ "Comutatividade: 2 + 3 = " ++ show (natToInt a_mais_b) ++ 
             ", 3 + 2 = " ++ show (natToInt b_mais_a)
  putStrLn $ "  São iguais? " ++ show (a_mais_b == b_mais_a)
  
  -- Associatividade
  let abc1 = add (add dois tres) quatro  -- (2 + 3) + 4
  let abc2 = add dois (add tres quatro)  -- 2 + (3 + 4)
  putStrLn $ "\nAssociatividade: (2 + 3) + 4 = " ++ show (natToInt abc1) ++
             ", 2 + (3 + 4) = " ++ show (natToInt abc2)
  putStrLn $ "  São iguais? " ++ show (abc1 == abc2)
  
  -- Elemento neutro
  putStrLn $ "\nElemento neutro (0):"
  putStrLn $ "  5 + 0 = " ++ show (natToInt (add cinco zero))
  putStrLn $ "  0 + 5 = " ++ show (natToInt (add zero cinco))

-- Exemplo 3: Propriedades da Multiplicação
exemplo3 :: IO ()
exemplo3 = do
  putStrLn "\n=== Exemplo 3: Propriedades da Multiplicação ==="
  
  -- Comutatividade
  let a_vezes_b = mult dois tres
  let b_vezes_a = mult tres dois
  putStrLn $ "Comutatividade: 2 × 3 = " ++ show (natToInt a_vezes_b) ++
             ", 3 × 2 = " ++ show (natToInt b_vezes_a)
  putStrLn $ "  São iguais? " ++ show (a_vezes_b == b_vezes_a)
  
  -- Distributividade: a × (b + c) = (a × b) + (a × c)
  let lado_esq = mult dois (add tres quatro)      -- 2 × (3 + 4)
  let lado_dir = add (mult dois tres) (mult dois quatro)  -- (2 × 3) + (2 × 4)
  putStrLn $ "\nDistributividade: 2 × (3 + 4) = " ++ show (natToInt lado_esq) ++
             ", (2 × 3) + (2 × 4) = " ++ show (natToInt lado_dir)
  putStrLn $ "  São iguais? " ++ show (lado_esq == lado_dir)
  
  -- Elemento neutro
  putStrLn $ "\nElemento neutro (1):"
  putStrLn $ "  5 × 1 = " ++ show (natToInt (mult cinco um))
  putStrLn $ "  1 × 5 = " ++ show (natToInt (mult um cinco))
  
  -- Elemento absorvente
  putStrLn $ "\nElemento absorvente (0):"
  putStrLn $ "  5 × 0 = " ++ show (natToInt (mult cinco zero))
  putStrLn $ "  0 × 5 = " ++ show (natToInt (mult zero cinco))

-- Exemplo 4: Testando Máximo
exemplo4 :: IO ()
exemplo4 = do
  putStrLn "\n=== Exemplo 4: Função Máximo ==="
  putStrLn $ "max(3, 5) = " ++ show (natToInt (maxNat tres cinco))
  putStrLn $ "max(7, 2) = " ++ show (natToInt (maxNat sete dois))
  putStrLn $ "max(4, 4) = " ++ show (natToInt (maxNat quatro quatro))
  putStrLn $ "max(0, 8) = " ++ show (natToInt (maxNat zero oito))

-- Exemplo 5: Soma dos primeiros n números
somaNaturais :: Nat -> Nat
somaNaturais Zero = Zero
somaNaturais (Succ n) = add (Succ n) (somaNaturais n)

exemplo5 :: IO ()
exemplo5 = do
  putStrLn "\n=== Exemplo 5: Soma dos Primeiros n Números ==="
  putStrLn $ "1 + 2 + 3 + 4 + 5 = " ++ show (natToInt (somaNaturais cinco))
  putStrLn $ "Fórmula: n(n+1)/2 = 5×6/2 = " ++ show (5 * 6 `div` 2)
  
  putStrLn $ "\n1 + 2 + ... + 10 = " ++ show (natToInt (somaNaturais dez))
  putStrLn $ "Fórmula: n(n+1)/2 = 10×11/2 = " ++ show (10 * 11 `div` 2)

-- Exemplo 6: Fatorial usando Peano
fatorial :: Nat -> Nat
fatorial Zero = Succ Zero  -- 0! = 1
fatorial (Succ n) = mult (Succ n) (fatorial n)

exemplo6 :: IO ()
exemplo6 = do
  putStrLn "\n=== Exemplo 6: Fatorial usando Números de Peano ==="
  putStrLn $ "0! = " ++ show (natToInt (fatorial zero))
  putStrLn $ "1! = " ++ show (natToInt (fatorial um))
  putStrLn $ "2! = " ++ show (natToInt (fatorial dois))
  putStrLn $ "3! = " ++ show (natToInt (fatorial tres))
  putStrLn $ "4! = " ++ show (natToInt (fatorial quatro))
  putStrLn $ "5! = " ++ show (natToInt (fatorial cinco))

-- Exemplo 7: Potenciação
potencia :: Nat -> Nat -> Nat
potencia _ Zero = Succ Zero           -- a^0 = 1
potencia a (Succ n) = mult a (potencia a n)  -- a^(n+1) = a × a^n

exemplo7 :: IO ()
exemplo7 = do
  putStrLn "\n=== Exemplo 7: Potenciação ==="
  putStrLn $ "2^0 = " ++ show (natToInt (potencia dois zero))
  putStrLn $ "2^1 = " ++ show (natToInt (potencia dois um))
  putStrLn $ "2^2 = " ++ show (natToInt (potencia dois dois))
  putStrLn $ "2^3 = " ++ show (natToInt (potencia dois tres))
  putStrLn $ "3^3 = " ++ show (natToInt (potencia tres tres))

-- Executar todos os exemplos
todosExemplos :: IO ()
todosExemplos = do
  putStrLn "\n╔════════════════════════════════════════════════════╗"
  putStrLn "║      Exemplos da Biblioteca Peano em Haskell      ║"
  putStrLn "╚════════════════════════════════════════════════════╝"
  exemplo1
  exemplo2
  exemplo3
  exemplo4
  exemplo5
  exemplo6
  exemplo7
  putStrLn "\n✓ Todos os exemplos executados com sucesso!"

-- Para usar no GHCi:
-- :load exemplos_peano.hs
-- todosExemplos
-- exemplo1
-- exemplo2
-- etc.
