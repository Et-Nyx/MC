# Respostas da Atividade de Reforço - DIM0152

Este diretório contém as respostas formalizadas para a Atividade de Reforço da disciplina **Matemática para Computação I**.

## 📁 Estrutura do Projeto

```
Atividade_01/
├── MC1.pdf              # Enunciado da atividade
├── respostas.typ        # Respostas em Typst (código fonte)
├── respostas.pdf        # Respostas compiladas (PDF)
├── Peano.hs             # Biblioteca Haskell (Questão 6)
├── peano_test           # Executável compilado
└── README.md            # Este arquivo
```

## 🛠️ Ambiente de Desenvolvimento

O projeto utiliza **Nix** para gerenciar dependências. Um arquivo `shell.nix` está disponível no diretório raiz do projeto (`/home/eter/Documentos/Projetos/Logica/`).

### Ferramentas Incluídas

- **Typst**: Geração de documentos matemáticos formatados
- **GHC + Cabal**: Compilador e ferramentas Haskell
- **Poppler Utils**: Ferramentas para manipulação de PDF

### Como Usar o Ambiente

Entre no ambiente Nix a partir do diretório raiz ou de qualquer subdiretório:

```bash
cd /home/eter/Documentos/Projetos/Logica
nix-shell
```

## 📝 Respostas Implementadas

### Questões 1-4: Provas por Indução Matemática

Demonstrações formais para:

1. Soma dos quadrados: $1^2 + 2^2 + \cdots + n^2 = \frac{n(n+1)(2n+1)}{6}$
2. Soma de ímpares: $1 + 3 + 5 + \cdots + (2n-1) = n^2$
3. Soma dos cubos: $1^3 + 2^3 + \cdots + n^3 = (1 + 2 + \cdots + n)^2$
4. Soma de pares: $2 \cdot 1 + 2 \cdot 2 + \cdots + 2 \cdot n = n(n+1)$

### Questão 5: Sistema Formal de Peano

Demonstrações formais das propriedades fundamentais da adição:

- **Comutatividade**: $a + b = b + a$
- **Associatividade**: $(a + b) + c = a + (b + c)$
- **Lei do Cancelamento**: Se $a + c = b + c$, então $a = b$

### Questão 6: Biblioteca Haskell

Implementação completa da aritmética de Peano em Haskell com:

- Tipo de dados `Nat` representando números naturais
- Operações: soma, subtração, multiplicação, máximo
- Interface interativa (REPL)
- Demonstrações passo a passo de cada operação

## 🔨 Compilação

### Compilar o Documento Typst

```bash
nix-shell --run "typst compile respostas.typ respostas.pdf"
```

Ou dentro do `nix-shell`:

```bash
typst compile respostas.typ respostas.pdf
```

### Compilar a Biblioteca Haskell

```bash
nix-shell --run "ghc Peano.hs -o peano_test"
```

Ou dentro do `nix-shell`:

```bash
ghc Peano.hs -o peano_test
```

## 🚀 Executando a Biblioteca Haskell

### Modo Interativo (REPL)

Execute o programa compilado:

```bash
./peano_test
```

Você verá um menu interativo onde pode escolher operações e testar a biblioteca.

### Usando GHCi

Você também pode carregar a biblioteca no REPL do Haskell:

```bash
nix-shell --run "ghci Peano.hs"
```

Exemplos de comandos no GHCi:

```haskell
-- Criar números
let dois = Succ (Succ Zero)
let tres = Succ (Succ (Succ Zero))

-- Operações básicas
add dois tres
mult dois tres
maxNat dois tres

-- Demonstrações com passos
demonstrateAdd 2 3
demonstrateMult 3 4
demonstrateMax 5 7

-- Executar todos os exemplos
runAllExamples
```

### Conversões Úteis

```haskell
-- Converter de Int para Nat
let cinco = intToNat 5

-- Converter de Nat para Int
natToInt cinco

-- Visualizar um número
showNat cinco
showNatCompact cinco
```

## 📚 Estrutura da Solução

### Documento Typst (respostas.typ)

O documento utiliza uma estrutura formal com:

- **Ambientes de teorema**: Proposições e axiomas destacados
- **Ambientes de prova**: Demonstrações formais com QED
- **Formatação matemática**: Notação LaTeX para equações
- **Organização hierárquica**: Seções e subseções numeradas

### Biblioteca Haskell (Peano.hs)

A implementação segue fielmente os axiomas de Peano:

```haskell
-- Definição dos números naturais
data Nat = Zero | Succ Nat

-- Axiomas da adição
add a Zero     = a           -- A1: a + 0 = a
add a (Succ b) = Succ (add a b)  -- A2: a + S(b) = S(a + b)

-- Axiomas da multiplicação
mult a Zero     = Zero          -- M1: a × 0 = 0
mult a (Succ b) = add a (mult a b)  -- M2: a × S(b) = a + (a × b)
```

## ✅ Verificação

Após compilar, você pode verificar:

1. **Documento PDF**: Abra `respostas.pdf` para visualizar as provas formais
2. **Biblioteca Haskell**: Execute `./peano_test` e escolha a opção 5 para ver todos os exemplos

## 📖 Referências

- **Axiomas de Peano**: Definição formal dos números naturais
- **Indução Matemática**: Método de demonstração para proposições sobre naturais
- **Haskell**: Linguagem funcional pura, ideal para expressões matemáticas
