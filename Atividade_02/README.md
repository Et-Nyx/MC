# Respostas da Atividade de Reforço 02 - DIM0152

Este diretório contém as respostas e implementações para a segunda Atividade de Reforço da disciplina Matemática para Computação I, focada em Arranjos e Combinatória.

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

## Como Executar

**Passo 1:** Compilar o código

```bash
ghc Arranjos.hs -o arranjos
```

**Passo 2:** Executar o programa

```bash
./arranjos
```

O programa executará automaticamente casos de teste para todas as 4 questões e exibirá os resultados no terminal.

## Compilar o Documento Typst

```bash
typst compile respostas.typ respostas.pdf
```

## Autor

- **Aluno**: Eduardo Teixeira de Moura Silva
- **Matrícula**: 20200047345
- **Disciplina**: DIM0152 - Matemática para Computação I
- **Semestre**: 2025.2
