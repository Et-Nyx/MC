#set document(
  title: "Atividade de Reforço - DIM0152",
  author: "",
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

// Teorema
#let theorem(title, body) = {
  block(
    fill: rgb("#e8f4f8"),
    inset: 10pt,
    radius: 4pt,
    [
      *#title:* #body
    ]
  )
}

#let proof(body) = {
  block(
    inset: (left: 10pt),
    [
      *Demonstração:*
      
      #body
    ]
  )
}

#let axiom(title, body) = {
  block(
    fill: rgb("#fff4e6"),
    inset: 10pt,
    radius: 4pt,
    [
      *#title:* #body
    ]
  )
}

// Capa
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
    Atividade de Reforço
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

= Questão 1

#theorem("Proposição")[
  Para todo $n in NN - {0}$ tem-se que:
  $ 1^2 + 2^2 + dots + n^2 = (n(n+1)(2n+1))/6 $
]

#proof[
  Demonstraremos por *indução matemática* sobre $n$.
  
  *Caso Base ($n = 1$):*
  
  Para $n = 1$, temos:
  - Lado esquerdo: $1^2 = 1$
  - Lado direito: $(1 dot 2 dot 3)/6 = 6/6 = 1$
  
  Portanto, $1 = 1$, e a proposição é verdadeira para $n = 1$.
  
  *Hipótese de Indução:*
  
  Suponha que a proposição seja verdadeira para algum $k in NN - {0}$, isto é:
  $ 1^2 + 2^2 + dots + k^2 = (k(k+1)(2k+1))/6 $
  
  *Passo Indutivo:*
  
  Devemos mostrar que a proposição é verdadeira para $k + 1$, ou seja:
  $ 1^2 + 2^2 + dots + k^2 + (k+1)^2 = ((k+1)(k+2)(2k+3))/6 $
  
  Partindo do lado esquerdo:
  $ 1^2 + 2^2 + dots + k^2 + (k+1)^2 &= (k(k+1)(2k+1))/6 + (k+1)^2 quad #text(fill: blue)[(pela H.I.)] \
  &= (k(k+1)(2k+1) + 6(k+1)^2)/6 \
  &= ((k+1)[k(2k+1) + 6(k+1)])/6 \
  &= ((k+1)[2k^2 + k + 6k + 6])/6 \
  &= ((k+1)[2k^2 + 7k + 6])/6 \
  &= ((k+1)(k+2)(2k+3))/6 $
  
  A última igualdade segue de fatorar $2k^2 + 7k + 6 = (k+2)(2k+3)$.
  
  Portanto, a proposição é verdadeira para $k + 1$.
  
  *Conclusão:*
  
  Pelo princípio de indução matemática, $1^2 + 2^2 + dots + n^2 = (n(n+1)(2n+1))/6$ para todo $n in NN - {0}$.
]

#pagebreak()

= Questão 2

#theorem("Proposição")[
  Para todo $n in NN - {0}$ tem-se que:
  $ 1 + 3 + 5 + dots + 2(n-1) = n^2 $
]

#proof[
  Demonstraremos por *indução matemática* sobre $n$.
  
  *Caso Base ($n = 1$):*
  
  Para $n = 1$, o lado esquerdo contém apenas o primeiro termo da sequência, que é $2(1-1) = 0$. 
  
  Porém, observamos que quando $n = 1$, a soma dos primeiros $n$ ímpares começando de 1 é: $1 = 1^2$.
  
  Vamos reinterpretar a questão como: $1 + 3 + 5 + dots + (2n-1) = n^2$.
  
  Para $n = 1$: $2(1) - 1 = 1 = 1^2$. ✓
  
  *Hipótese de Indução:*
  
  Suponha que para algum $k in NN - {0}$:
  $ 1 + 3 + 5 + dots + (2k-1) = k^2 $
  
  *Passo Indutivo:*
  
  Devemos mostrar que para $k + 1$:
  $ 1 + 3 + 5 + dots + (2k-1) + (2(k+1)-1) = (k+1)^2 $
  
  Partindo do lado esquerdo:
  $ 1 + 3 + 5 + dots + (2k-1) + (2k+1) &= k^2 + (2k+1) quad #text(fill: blue)[(pela H.I.)] \
  &= k^2 + 2k + 1 \
  &= (k+1)^2 $
  
  *Conclusão:*
  
  Pelo princípio de indução matemática, a soma dos primeiros $n$ números ímpares é igual a $n^2$.
]

#pagebreak()

= Questão 3

#theorem("Proposição")[
  Para todo $n in NN - {0}$ tem-se que:
  $ 1^3 + 2^3 + 3^3 + dots + n^3 = (1 + 2 + 3 + dots + n)^2 $
]

#proof[
  Sabemos que $1 + 2 + 3 + dots + n = (n(n+1))/2$. Portanto, queremos mostrar:
  $ 1^3 + 2^3 + 3^3 + dots + n^3 = ((n(n+1))/2)^2 = (n^2(n+1)^2)/4 $
  
  Demonstraremos por *indução matemática* sobre $n$.
  
  *Caso Base ($n = 1$):*
  
  - Lado esquerdo: $1^3 = 1$
  - Lado direito: $((1 dot 2)/2)^2 = 1^2 = 1$
  
  Portanto, a proposição é verdadeira para $n = 1$.
  
  *Hipótese de Indução:*
  
  Suponha que para algum $k in NN - {0}$:
  $ 1^3 + 2^3 + dots + k^3 = (k^2(k+1)^2)/4 $
  
  *Passo Indutivo:*
  
  Devemos mostrar que para $k + 1$:
  $ 1^3 + 2^3 + dots + k^3 + (k+1)^3 = ((k+1)^2(k+2)^2)/4 $
  
  Partindo do lado esquerdo:
  $ 1^3 + 2^3 + dots + k^3 + (k+1)^3 &= (k^2(k+1)^2)/4 + (k+1)^3 quad #text(fill: blue)[(pela H.I.)] \
  &= ((k+1)^2[k^2 + 4(k+1)])/4 \
  &= ((k+1)^2[k^2 + 4k + 4])/4 \
  &= ((k+1)^2(k+2)^2)/4 $
  
  Pois $k^2 + 4k + 4 = (k+2)^2$.
  
  *Conclusão:*
  
  Pelo princípio de indução matemática, $1^3 + 2^3 + dots + n^3 = (1 + 2 + dots + n)^2$ para todo $n in NN - {0}$.
]

#pagebreak()

= Questão 4

#theorem("Proposição")[
  Para todo $n in NN - {0}$ tem-se que:
  $ 2 dot 1 + 2 dot 2 + 2 dot 3 + dots + 2 dot n = n^2 $
]

#proof[
  *Observação:* A fórmula apresentada parece estar incorreta. Vamos verificar para $n = 1, 2, 3$:
  
  - Para $n = 1$: $2 dot 1 = 2 eq.not 1^2 = 1$
  - Para $n = 2$: $2 dot 1 + 2 dot 2 = 2 + 4 = 6 eq.not 2^2 = 4$
  
  A fórmula correta deveria ser:
  $ 2 dot 1 + 2 dot 2 + 2 dot 3 + dots + 2 dot n = n(n+1) $
  
  ou possivelmente:
  $ 1 + 2 + 3 + dots + n = (n(n+1))/2 $
  
  Demonstraremos a fórmula $sum_(i=1)^n 2i = n(n+1)$ por indução.
  
  *Caso Base ($n = 1$):*
  
  $2 dot 1 = 2 = 1(1+1)$ ✓
  
  *Hipótese de Indução:*
  
  Suponha que para algum $k in NN - {0}$:
  $ 2 dot 1 + 2 dot 2 + dots + 2 dot k = k(k+1) $
  
  *Passo Indutivo:*
  
  Devemos mostrar que para $k + 1$:
  $ 2 dot 1 + 2 dot 2 + dots + 2 dot k + 2(k+1) = (k+1)(k+2) $
  
  Partindo do lado esquerdo:
  $ 2 dot 1 + 2 dot 2 + dots + 2 dot k + 2(k+1) &= k(k+1) + 2(k+1) quad #text(fill: blue)[(pela H.I.)] \
  &= (k+1)(k+2) $
  
  *Conclusão:*
  
  Pelo princípio de indução matemática, $2 dot 1 + 2 dot 2 + dots + 2 dot n = n(n+1)$ para todo $n in NN - {0}$.
]

#pagebreak()

= Questão 5

Usando o sistema formal da aritmética de Peano, demonstraremos as seguintes propriedades:

== Propriedade 5.1: Comutatividade da Adição

#theorem("Teorema (Comutatividade da Adição)")[
  Para todo $a, b in NN$, tem-se que $a + b = b + a$.
]

#axiom("Axiomas de Peano Relevantes")[
  - *P1:* $0 in NN$
  - *P2:* Para todo $n in NN$, existe $S(n) in NN$ (sucessor de $n$)
  - *P3:* Para todo $n in NN$, $S(n) eq.not 0$
  - *P4:* Se $S(m) = S(n)$, então $m = n$
  - *P5:* Se $P(0)$ é verdadeiro e $P(n) => P(S(n))$ para todo $n$, então $P(n)$ é verdadeiro para todo $n in NN$
  
  *Definição da Adição:*
  - *A1:* $a + 0 = a$
  - *A2:* $a + S(b) = S(a + b)$
]

#proof[
  Fixemos $a in NN$ arbitrário e demonstremos por indução sobre $b$.
  
  *Lemas Auxiliares:*
  
  *Lema 1:* $0 + b = b$ para todo $b in NN$.
  
  _Demonstração do Lema 1:_ Por indução em $b$.
  - Base: $0 + 0 = 0$ (por A1)
  - Passo: Suponha $0 + k = k$. Então:
    $ 0 + S(k) = S(0 + k) = S(k) quad #text(fill: blue)[(por A2 e H.I.)] $
  
  *Lema 2:* $S(a) + b = S(a + b)$ para todo $a, b in NN$.
  
  _Demonstração do Lema 2:_ Por indução em $b$.
  - Base: $S(a) + 0 = S(a) = S(a + 0)$ (por A1)
  - Passo: Suponha $S(a) + k = S(a + k)$. Então:
    $ S(a) + S(k) &= S(S(a) + k) quad #text(fill: blue)[(por A2)] \
    &= S(S(a + k)) quad #text(fill: blue)[(por H.I.)] \
    &= S(a + S(k)) quad #text(fill: blue)[(por A2)] $
  
  *Demonstração Principal:*
  
  *Base ($b = 0$):*
  $ a + 0 = a = 0 + a quad #text(fill: blue)[(por A1 e Lema 1)] $
  
  *Passo Indutivo:* Suponha $a + k = k + a$. Então:
  $ a + S(k) &= S(a + k) quad #text(fill: blue)[(por A2)] \
  &= S(k + a) quad #text(fill: blue)[(por H.I.)] \
  &= S(k) + a quad #text(fill: blue)[(por Lema 2)] $
  
  Portanto, por indução, $a + b = b + a$ para todo $b in NN$.
]

#pagebreak()

== Propriedade 5.2: Associatividade da Adição

#theorem("Teorema (Associatividade da Adição)")[
  Para todo $a, b, c in NN$, tem-se que $(a + b) + c = a + (b + c)$.
]

#proof[
  Fixemos $a, b in NN$ arbitrários e demonstremos por indução sobre $c$.
  
  *Base ($c = 0$):*
  $ (a + b) + 0 &= a + b quad #text(fill: blue)[(por A1)] \
  &= a + (b + 0) quad #text(fill: blue)[(por A1)] $
  
  *Passo Indutivo:* Suponha $(a + b) + k = a + (b + k)$. Então:
  $ (a + b) + S(k) &= S((a + b) + k) quad #text(fill: blue)[(por A2)] \
  &= S(a + (b + k)) quad #text(fill: blue)[(por H.I.)] \
  &= a + S(b + k) quad #text(fill: blue)[(por A2)] \
  &= a + (b + S(k)) quad #text(fill: blue)[(por A2)] $
  
  Portanto, por indução, $(a + b) + c = a + (b + c)$ para todo $c in NN$.
]

#pagebreak()

== Propriedade 5.3: Lei do Cancelamento

#theorem("Teorema (Lei do Cancelamento)")[
  Para todo $a, b, c in NN$, se $a + c = b + c$, então $a = b$.
]

#proof[
  Demonstraremos por indução sobre $c$.
  
  *Base ($c = 0$):*
  
  Se $a + 0 = b + 0$, então $a = b$ (por A1).
  
  *Passo Indutivo:*
  
  Suponha que a propriedade vale para $k$, ou seja, se $a + k = b + k$, então $a = b$.
  
  Agora, suponha que $a + S(k) = b + S(k)$. Então:
  $ a + S(k) &= b + S(k) \
  S(a + k) &= S(b + k) quad #text(fill: blue)[(por A2)] $
  
  Pelo axioma P4, se $S(a + k) = S(b + k)$, então $a + k = b + k$.
  
  Pela hipótese de indução, como $a + k = b + k$, segue que $a = b$.
  
  Portanto, por indução, a lei do cancelamento vale para todo $c in NN$.
]

#pagebreak()

= Questão 6

Para a questão 6, foi implementada uma biblioteca em Haskell que formaliza a aritmética de Peano e suas operações.

A biblioteca está disponível no arquivo #link("file:///home/eter/Documentos/Projetos/Logica/Atividade_01/Peano.hs")[`Peano.hs`] e pode ser testada no REPL do GHC.

*Estrutura da Implementação:*

1. *Tipo de Dados `Nat`*: Representa os números naturais usando os construtores de Peano
   - `Zero`: representa o número $0$
   - `Succ n`: representa o sucessor de $n$

2. *Operações Aritméticas*:
   - `add`: Soma de dois números naturais
   - `sub`: Subtração de dois números naturais
   - `mult`: Multiplicação de dois números naturais
   - `maxNat`: Máximo entre dois números naturais

3. *Interface de Visualização*:
   - Instância de `Show` para visualizar números de Peano
   - Funções auxiliares para converter entre `Int` e `Nat`
   - Funções de demonstração que mostram o passo a passo das operações

A implementação completa com comentários detalhados e exemplos encontra-se no arquivo fonte.
