#set document(
  title: "Atividade de Reforço 02 - DIM0152",
  author: "Eduardo Teixeira de Moura Silva",
  date: datetime(year: 2025, month: 11, day: 24),
)

#set page(
  paper: "a4",
  margin: (x: 2.5cm, y: 2.5cm),
  numbering: "1",
)

#set text(
  font: "New Computer Modern",
  size: 11pt,
  lang: "pt",
)

#set par(justify: true)

#set heading(numbering: "1.")

// Title page
#align(center)[
  #text(size: 14pt, weight: "bold")[
    Universidade Federal do Rio Grande do Norte
  ]
  
  #v(0.5cm)
  
  #text(size: 12pt)[
    DIM0152 - Matemática para Computação I
  ]
  
  #v(0.3cm)
  
  #text(size: 11pt)[
    Prof.: Valdigleis S. Costa
  ]
  
  #v(1.5cm)
  
  #text(size: 16pt, weight: "bold")[
    Atividade de Reforço 02
  ]
  
  #v(0.5cm)
  
  #text(size: 12pt)[
    Semestre 2025.2
  ]
  
  #v(2cm)
  
  #text(size: 11pt)[
    *Aluno:* Eduardo Teixeira de Moura Silva
  ]
  
  #v(0.5cm)
  
  #text(size: 11pt)[
    *Matrícula:* 20200047345
  ]
]

#pagebreak()

= Introdução

Esta atividade consiste na implementação de algoritmos para manipulação de arranjos e conjuntos, utilizando apenas recursos básicos de uma linguagem de programação.

A linguagem escolhida foi *Haskell*, devido à sua expressividade natural para lidar com listas e recursão, que são fundamentais para problemas combinatórios.

O código fonte completo encontra-se no arquivo `Arranjos.hs` neste mesmo diretório.

= Soluções Implementadas

== Questão 1(a): Arranjos sem Repetição

*Problema:* Gerar todos os arranjos sem repetição de tamanho $m$ utilizando elementos de um conjunto $X$.

*Implementação:*
Utilizamos uma abordagem recursiva. Para gerar um arranjo de tamanho $m$:
1. Selecionamos um elemento $x$ de $X$.
2. Recursivamente geramos arranjos de tamanho $m-1$ a partir dos elementos restantes ($X - {x}$).
3. Combinamos $x$ com cada sub-arranjo gerado.

```haskell
arranjosSemRepeticao :: Eq a => [a] -> Int -> [[a]]
arranjosSemRepeticao _ 0 = [[]]
arranjosSemRepeticao elements m = 
    [ x:xs | x <- elements, 
             xs <- arranjosSemRepeticao [e | e <- elements, e /= x] (m-1) ]
```

== Questão 1(b): Arranjos com Repetição

*Problema:* Gerar arranjos com repetição de tamanho $m$ que utilizam no mínimo $n$ elementos distintos do conjunto $X$.

*Implementação:*
1. Geramos todos os arranjos com repetição de tamanho $m$ (produto cartesiano).
2. Filtramos apenas aqueles que possuem quantidade de elementos distintos $>= n$.

```haskell
questaoB :: Show a => Eq a => [a] -> Int -> Int -> IO ()
questaoB x m n = do
    let todos = arranjosComRepeticao x m
    let validos = [ arr | arr <- todos, contaDistintos arr >= n ]
    mapM_ print validos
```

== Questão 1(c): Maior Arranjo com Soma Fixa

*Problema:* Encontrar o maior arranjo formado por $X union Y$ tal que a soma seja $p$, com no máximo $m$ elementos de $X$ e $n$ elementos de $Y$.

*Implementação:*
1. Geramos todos os subconjuntos de $X union Y$.
2. Filtramos os que têm soma igual a $p$.
3. Verificamos as restrições de quantidade para cada origem.
4. Selecionamos o de maior comprimento.

== Questão 1(d): Maior Subarranjo Crescente

*Problema:* Dado um arranjo $A$, encontrar o maior subarranjo contíguo crescente.

*Implementação:*
Percorremos a lista mantendo o subarranjo crescente atual. Se o próximo elemento for maior ou igual ao anterior, estendemos o subarranjo. Caso contrário, iniciamos um novo. Mantemos sempre o maior encontrado até o momento.

```haskell
encontraSubCrescente :: [Int] -> [Int] -> [Int] -> [Int]
encontraSubCrescente (x:xs) atual melhor =
    let ultimo = last atual
    in if x >= ultimo
       then encontraSubCrescente xs (atual ++ [x]) melhor
       else 
           let novoMelhor = if length atual > length melhor then atual else melhor
           in encontraSubCrescente xs [x] novoMelhor
```

= Como Executar

Para executar o código e ver os resultados dos testes para cada questão:

1. Certifique-se de estar no ambiente Nix:
   ```bash
   nix-shell
   ```

2. Compile o arquivo Haskell:
   ```bash
   cd Atividade_02
   ghc Arranjos.hs -o arranjos
   ```

3. Execute o programa:
   ```bash
   ./arranjos
   ```

O programa executará casos de teste para cada uma das 4 questões e imprimirá os resultados no terminal.
