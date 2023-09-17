# Mapas

Um módulo educacional para programação funcional.

## Descrição

O módulo foi criado para suporte aos alunos da disciplina de Programação
Funcional na Universidade Federal de Sergipe (UFS).

O módulo define tipos como `Cidade`, `Estradas` e `Mapa`, assim como:

- funções de IO para os mapas, representados por arquivos com a extensão `.mapa`
- funções de desenho de cidades, estradas e mapas, usando `CodeWorld` como API
  de desenho

## Instalação

Por causa da dependência `codeworld-api`, é recomendado instalar via `cabal` (ou
`stack`, se preferir)

### `cabal`

A ferramenta `cabal` é o sistema de pacotes padrão de Haskell. É muito fácil se
perder ao usar ela, mas siga os passos que você vai se dar bem =)

1. Siga a [documentação](https://cabal.readthedocs.io/en/stable/) e o
instale via GHCup. Recomendo instalar a versão `3.10.1.0` ou maior.
2. Crie uma pasta/diretório. Nesse caso eu chamei ela de `projeto-mapas`
3. Vá para `projeto-mapas` e execute `cabal init -m -n`
4. O arquivo `projeto-mapas.cabal` será criado. Edite-o para ficar assim:
```cabal
cabal-version:   3.0
name:            teste
version:         0.1.0.0
license:         NONE
build-type:      Simple

common warnings
    ghc-options: -Wall

executable teste
    import:           warnings
    main-is:          Main.hs
    build-depends:
        base ^>=4.16.4.0,
        text ^>=1.2.5.0,
        codeworld-api ^>=0.8.1,
        mapas-ufs ^>= 0.1.0.0
    hs-source-dirs:   app
    default-language: Haskell2010
```
5. Edite o seu código que estará em `app/Main.hs`. Não se esqueça de importar as
   bibliotecas necessárias.
6. Teste com `cabal repl` ou `cabal run`. Não use somente o `ghci`, pois não
   terá acesso aos módulos.

## Documentação da API pública

**API pública** é o conjunto de elementos que um módulo exporta: tipos,
funções, constantes, etc. Como a nossa é minúscula, ela é capaz de ser descrita no
README. **Lembre de incluir `import Mapa` no começo do seu código`.

### `Mapa`, `Cidade`, `Estradas` e outros tipos

- Esses tipos modelam um grafo, uma das estruturas mais
  utilizadas na computação, que representa cidades nomeadas (`Nome`) distribuídas no plano cartesiano
  (`Localizacao`), as quais possuem estradas para outras cidades (`Estradas`). A
  lista de cidades é um `Mapa`.
- Obs: Pode ocorrer dos tipos que você definiu anteriormente entrarem em
  conflito com os tipos exportados pelo módulo. Recomendo apagar seus tipos e
  usar os exportados pela API.
```hs
meuPais :: Cidade
meuPais = ("Aracaju", (8, 10), ["Maceió", "Recife", "Salvador"])

cidadeFrevo :: Cidade
cidadeFrevo = ("Recife", (8, 15), ["Aracaju"])

orlaFraca :: Cidade
orlaFraca = ("Maceió", (7, 12.5), ["Aracaju"])

nordesteParcial :: Mapa
nordesteParcial = [meuPais, cidadeFrevo, orlaFraca]

rota :: Estradas
rota = [cidadeFrevo, meuPais, orlaFraca]
```

### `carregarMapa :: FilePath -> IO Mapa`

- Carrega um `Mapa` de um arquivo `.mapa`
- Obs: **Importante usar o operador bind `<-` visto nas aulas de IO**
```hs
marioWorld :: Mapa
marioWorld <- carregarMapa "teste.mapa"
-- se nao usassemos operador bind, tipo resultante seria IO Mapa
```

### `salvarMapa :: FilePath -> Mapa -> IO ()`

- Salva um `Mapa` num dado arquivo.
```hs
novoMarioWorld = (adicionarCidade "Wario" -100.3 0.0 . removerCidade "Luigi") marioWorld
salvarMapa "teste.mapa" novoMarioWorld
```

---

### `desenharCidade :: Cidade -> Picture`

- Desenha uma cidade (no caso, uma simples bola) em sua `Localizacao` no plano
  cartesiano.

### `desenharEstrada :: Cidade -> Cidade -> Picture`

- Desenha uma linha reta entre duas cidades.

### `desenharMapa :: Mapa -> Picture`

- Desenha todas cidades e estradas do dado mapa

```hs
import CodeWorld
import Mapa

main :: IO ()
main = drawingOf $ desenharMapa marioWorld
  where
    marioWorld <- carregarMapa "teste.mapa"
```
