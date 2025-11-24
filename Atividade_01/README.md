# Respostas da Atividade de Reforço - DIM0152

Este diretório contém as respostas formalizadas para a Atividade de Reforço da disciplina Matemática para Computação I.

## Estrutura do Projeto

```
Atividade_01/
├── MC1.pdf              # Enunciado da atividade
├── respostas.typ        # Respostas em Typst (código fonte)
├── respostas.pdf        # Respostas compiladas (PDF)
├── Peano.hs             # Biblioteca Haskell (Questão 6)
└── README.md            # Este arquivo
```

## Ambiente de Desenvolvimento

O projeto utiliza Nix para gerenciar dependências. Um arquivo `shell.nix` está disponível no diretório raiz do projeto.

### Ferramentas Incluídas

- **Typst**: Geração de documentos matemáticos formatados
- **GHC + Cabal**: Compilador e ferramentas Haskell
- **Poppler Utils**: Ferramentas para manipulação de PDF

### Como Usar o Ambiente

Entre no ambiente Nix a partir do diretório raiz:

```bash
nix-shell
```

## Como Usar a Biblioteca Peano

A biblioteca pode ser usada de duas formas diferentes:

### Opção 1: Executável Interativo 

Esta opção cria um programa com menu interativo.

**Passo 1:** Compile o executável
```bash
ghc Peano.hs -o peano
```

**Passo 2:** Execute o programa
```bash
./peano
```

### Opção 2: GHCi (REPL do Haskell)

Esta opção dá mais flexibilidade para experimentar com as funções.

**Passo 1:** Abra o GHCi com a biblioteca carregada
```bash
ghci Peano.hs
```

**Passo 2:** Experimente as funções

```haskell
-- Criar números de Peano
let dois = Succ (Succ Zero)
let tres = Succ (Succ (Succ Zero))

-- Ou converter de Int
let cinco = intToNat 5

-- Operações básicas
add dois tres           -- retorna: Succ (Succ (Succ (Succ (Succ Zero))))
natToInt (add dois tres)  -- retorna: 5
mult dois tres          -- multiplicação
maxNat dois tres        -- máximo

-- Demonstrações passo a passo
demonstrateAdd 2 3
demonstrateMult 3 4
demonstrateMax 5 7

-- Ver todos os exemplos
runAllExamples
```

**Passo 3:** Para sair do GHCi

Digite qualquer um destes comandos:
- `:quit`
- `:q`
- Ou pressione `Ctrl+D`

## Compilar o Documento Typst

```bash
typst compile respostas.typ respostas.pdf
```

## Autor

- **Aluno**: Eduardo Teixeira de Moura Silva
- **Matrícula**: 20200047345
- **Disciplina**: DIM0152 - Matemática para Computação I
- **Semestre**: 2025.2