module Main where

-- Função auxiliar para imprimir listas
printList :: Show a => [a] -> IO ()
printList l = putStrLn $ show l

--------------------------------------------------------------------------------
-- Questão 1(a): Arranjos sem repetição
-- Gerar arranjos de tamanho m usando elementos de X
--------------------------------------------------------------------------------

-- Verifica se um elemento está na lista
contem :: Eq a => a -> [a] -> Bool
contem _ [] = False
contem x (y:ys) = x == y || contem x ys

-- Gera arranjos sem repetição de tamanho m
-- X: conjunto de elementos disponíveis
-- m: tamanho do arranjo desejado
arranjosSemRepeticao :: Eq a => [a] -> Int -> [[a]]
arranjosSemRepeticao _ 0 = [[]]
arranjosSemRepeticao [] _ = []
arranjosSemRepeticao elements m = 
    [ x:xs | x <- elements, 
             xs <- arranjosSemRepeticao [e | e <- elements, e /= x] (m-1) ]

-- Wrapper para a questão (a)
questaoA :: Show a => Eq a => [a] -> Int -> IO ()
questaoA x m = do
    putStrLn $ "\n--- Questão (a): Arranjos sem repetição de tamanho " ++ show m ++ " ---"
    let result = arranjosSemRepeticao x m
    mapM_ print result
    putStrLn $ "Total: " ++ show (length result)

--------------------------------------------------------------------------------
-- Questão 1(b): Arranjos com repetição
-- Tamanho m, usando NO MÍNIMO n elementos distintos de X
--------------------------------------------------------------------------------

-- Conta elementos distintos numa lista
contaDistintos :: Eq a => [a] -> Int
contaDistintos [] = 0
contaDistintos (x:xs)
    | contem x xs = contaDistintos xs
    | otherwise   = 1 + contaDistintos xs

-- Gera todos os arranjos com repetição de tamanho m
arranjosComRepeticao :: [a] -> Int -> [[a]]
arranjosComRepeticao _ 0 = [[]]
arranjosComRepeticao elements m =
    [ x:xs | x <- elements, xs <- arranjosComRepeticao elements (m-1) ]

-- Filtra arranjos que usam pelo menos n elementos distintos
questaoB :: Show a => Eq a => [a] -> Int -> Int -> IO ()
questaoB x m n = do
    putStrLn $ "\n--- Questão (b): Arranjos com repetição (tam " ++ show m ++ ", min " ++ show n ++ " distintos) ---"
    let todos = arranjosComRepeticao x m
    let validos = [ arr | arr <- todos, contaDistintos arr >= n ]
    mapM_ print validos
    putStrLn $ "Total: " ++ show (length validos)

--------------------------------------------------------------------------------
-- Questão 1(c): Maior arranjo com soma exata p
-- X, Y conjuntos. Max m de X, Max n de Y. Soma = p.
--------------------------------------------------------------------------------

-- Soma de uma lista
soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

-- Separa elementos de X e Y num arranjo misto para contagem
-- Retorna (qtdX, qtdY)
contaOrigem :: Eq a => [a] -> [a] -> [a] -> (Int, Int)
contaOrigem _ _ [] = (0, 0)
contaOrigem x y (h:t) = 
    let (cx, cy) = contaOrigem x y t
    in if contem h x then (cx + 1, cy) 
       else if contem h y then (cx, cy + 1)
       else (cx, cy) -- não deveria acontecer se o arranjo vem de X U Y

-- Gera subconjuntos (combinações) de uma lista
subconjuntos :: [a] -> [[a]]
subconjuntos [] = [[]]
subconjuntos (x:xs) = [ x:ys | ys <- subconjuntos xs ] ++ subconjuntos xs

-- Implementação da questão C
questaoC :: [Int] -> [Int] -> Int -> Int -> Int -> IO ()
questaoC x y m n p = do
    putStrLn $ "\n--- Questão (c): Maior subconjunto com soma " ++ show p ++ " ---"
    let uniao = x ++ y
    -- Gera todos os subconjuntos possíveis (powerset)
    let subs = subconjuntos uniao
    
    -- Filtra pela soma
    let comSomaP = [ s | s <- subs, soma s == p ]
    
    -- Filtra pelas restrições de quantidade (max m de X, max n de Y)
    let validos = [ s | s <- comSomaP, 
                        let (qx, qy) = contaOrigem x y s,
                        qx <= m && qy <= n ]

    -- Encontra o de maior tamanho
    let maior = encontraMaior validos
    
    case maior of
        Nothing -> putStrLn "Nenhum arranjo encontrado."
        Just arr -> do
            putStrLn $ "Arranjo encontrado: " ++ show arr
            putStrLn $ "Tamanho: " ++ show (length arr)
            putStrLn $ "Soma: " ++ show (soma arr)

encontraMaior :: [[a]] -> Maybe [a]
encontraMaior [] = Nothing
encontraMaior (x:xs) = 
    case encontraMaior xs of
        Nothing -> Just x
        Just y -> if length x >= length y then Just x else Just y

--------------------------------------------------------------------------------
-- Questão 1(d): Maior subarranjo crescente (contíguo)
--------------------------------------------------------------------------------

-- Função auxiliar que percorre a lista mantendo o subarranjo atual e o melhor visto
buscaCrescente :: Ord a => [a] -> [a] -> [a] -> [a]
buscaCrescente [] atual melhor = 
    if length atual > length melhor then atual else melhor
buscaCrescente (x:xs) [] melhor = buscaCrescente xs [x] melhor
buscaCrescente (x:xs) (last:rest) melhor =
    if x >= last
    then buscaCrescente xs (last:rest ++ [x]) melhor
    else 
        let novoMelhor = if length (last:rest) > length melhor then (last:rest) else melhor
        in buscaCrescente xs [x] novoMelhor

maiorSubCrescente :: Ord a => [a] -> [a]
maiorSubCrescente [] = []
maiorSubCrescente lista = go lista [] []
  where
    go [] atual melhor = if length atual > length melhor then atual else melhor
    go (x:xs) [] melhor = go xs [x] melhor
    go (x:xs) atual@(last:_) melhor
        | x >= last = go xs (x:atual) melhor
        | otherwise = 
            let lenAtual = length atual
                lenMelhor = length melhor
                novoMelhor = if lenAtual > lenMelhor then atual else melhor
            in go xs [x] novoMelhor

    reverseList [] = []
    reverseList (x:xs) = reverseList xs ++ [x]

questaoD :: [Int] -> IO ()
questaoD arr = do
    putStrLn $ "\n--- Questão (d): Maior subarranjo crescente ---"
    putStrLn $ "Entrada: " ++ show arr
    let resultado = encontraSubCrescente arr [] []
    putStrLn $ "Saída: " ++ show resultado

encontraSubCrescente :: [Int] -> [Int] -> [Int] -> [Int]
encontraSubCrescente [] atual melhor = 
    if length atual > length melhor then atual else melhor
encontraSubCrescente (x:xs) [] melhor = encontraSubCrescente xs [x] melhor
encontraSubCrescente (x:xs) atual melhor =
    let ultimo = last atual
    in if x >= ultimo
       then encontraSubCrescente xs (atual ++ [x]) melhor
       else 
           let novoMelhor = if length atual > length melhor then atual else melhor
           in encontraSubCrescente xs [x] novoMelhor

-- Função last manual
meuLast :: [a] -> a
meuLast [x] = x
meuLast (_:xs) = meuLast xs

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== Soluções da Atividade 02 ==="
    
    -- Teste (a)
    -- Conjunto X = {1, 2, 3}, m = 2
    questaoA [1, 2, 3] 2
    
    -- Teste (b)
    -- Conjunto X = {1, 2}, m = 3, n = 2 (pelo menos 2 distintos)
    questaoB [1, 2] 3 2
    
    -- Teste (c)
    -- X={1,2,3}, Y={4,5}, m=2, n=1, p=6
    -- Possíveis: {1,5} (1 de X, 1 de Y, soma 6), {2,4} (1 de X, 1 de Y, soma 6)
    -- {1,2,3} soma 6 (3 de X -> inválido se m=2)
    questaoC [1, 2, 3] [4, 5] 2 1 6
    
    -- Teste (d)
    let exemplo = [10, 1, -2, 27, 33, 35, -5, -4, 0, 0, 1, -17, 25, 30, 4]
    questaoD exemplo
