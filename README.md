# Matemática para Computação I

Repositório contendo as atividades e implementações da disciplina **DIM0152 - Matemática para Computação I**.

## Conteúdo

### Atividade 01 - Aritmética de Peano e Indução Matemática

Respostas formalizadas abordando:
- Provas por indução matemática
- Sistema formal da aritmética de Peano
- Implementação de biblioteca Haskell para números naturais

[Ver detalhes →](Atividade_01/README.md)

### Atividade 02 - Arranjos e Combinatória

Implementação de algoritmos para:
- Geração de arranjos com e sem repetição
- Problemas de otimização combinatória (soma de subconjuntos)
- Maior subarranjo crescente

[Ver código →](Atividade_02/Arranjos.hs) | [Ver respostas →](Atividade_02/respostas.pdf)

## Ambiente de Desenvolvimento

O projeto utiliza **Nix** para gerenciar dependências de forma reproduzível.

### Pré-requisitos

- Nix package manager instalado

### Configuração do Ambiente

```bash
# Clone o repositório
git clone https://github.com/Et-Nyx/MC.git
cd MC

# Entre no ambiente Nix
nix-shell

# Agora você tem acesso a:
# - Typst (compilador de documentos)
# - GHC e Cabal (Haskell)
# - Ferramentas de manipulação de PDF
```

## Estrutura do Projeto

```
MC/
├── Atividade_01/          # Primeira atividade
│   ├── respostas.typ      # Respostas em Typst
│   ├── Peano.hs           # Biblioteca Haskell
│   └── README.md          # Documentação específica
├── Atividade_02/          # Segunda atividade
│   ├── respostas.typ      # Respostas em Typst
│   └── Arranjos.hs        # Implementação Haskell
│   └── README.md          # Documentação específica
├── shell.nix              # Configuração do ambiente Nix
├── .gitignore
└── README.md              # Este arquivo
```

## Como Usar

### Compilar Documentos Typst

```bash
nix-shell
cd Atividade_01
typst compile respostas.typ respostas.pdf
```

### Executar Código Haskell

```bash
nix-shell
cd Atividade_01

# Compilar e executar
ghc Peano.hs -o peano_test
./peano_test

# Ou usar o REPL interativo
ghci Peano.hs
```

### Executar Código da Atividade 02

```bash
nix-shell
cd Atividade_02

# Compilar e executar
ghc Arranjos.hs -o arranjos
./arranjos
```

## Dsciplina

- **Código**: DIM0152
- **Nome**: Matemática para Computação I
- **Professor**: Valdigleis S. Costa
- **Semestre**: 2025.2
- **Instituição**: Universidade Federal do Rio Grande do Norte (UFRN)

## Licença

Este projeto é destinado a fins educacionais.

## 👤 Autor

[@Et-Nyx](https://github.com/Et-Nyx)
