# Respostas da Atividade de Reforço 02 - DIM0152

Este diretório contém as respostas e implementações para a segunda Atividade de Reforço da disciplina **Matemática para Computação I**, focada em **Arranjos e Combinatória**.

## Estrutura do Projeto

```
Atividade_02/
├── MC2.pdf              # Enunciado da atividade
├── respostas.typ        # Respostas explicativas em Typst
├── respostas.pdf        # Documento compilado (PDF)
├── Arranjos.hs          # Implementação dos algoritmos em Haskell
├── arranjos             # Executável compilado
└── README.md            # Este arquivo
```

## Soluções Implementadas

Todas as soluções foram implementadas em **Haskell** (`Arranjos.hs`) utilizando apenas recursos básicos da linguagem (sem bibliotecas externas de combinatória), conforme solicitado.

### Questão 1(a): Arranjos sem Repetição
Algoritmo recursivo que gera todos os arranjos de tamanho $m$ utilizando elementos de um conjunto $X$, sem repetir elementos no mesmo arranjo.

### Questão 1(b): Arranjos com Repetição
Geração de arranjos de tamanho $m$ que utilizam no mínimo $n$ elementos distintos do conjunto $X$.

### Questão 1(c): Otimização Combinatória
Algoritmo que encontra o maior arranjo formado pela união de dois conjuntos $X$ e $Y$ tal que a soma dos elementos seja exatamente $p$, respeitando limites de quantidade para elementos de $X$ e $Y$.

### Questão 1(d): Maior Subarranjo Crescente
Algoritmo que processa uma lista de números e identifica a maior sequência contígua de elementos em ordem crescente (ou não-decrescente).

## Como Executar

Certifique-se de estar no ambiente Nix (execute `nix-shell` na raiz do projeto).

### 1. Compilar e Executar o Código Haskell

```bash
# Entrar no diretório
cd Atividade_02

# Compilar
ghc Arranjos.hs -o arranjos

# Executar
./arranjos
```

O programa executará automaticamente casos de teste para todas as 4 questões e exibirá os resultados no terminal.

### 2. Compilar a Documentação

```bash
typst compile respostas.typ respostas.pdf
```

## Autor

- **Aluno**: Eduardo Teixeira de Moura Silva
- **Matrícula**: 20200047345
- **Disciplina**: DIM0152 - Matemática para Computação I
- **Semestre**: 2025.2
