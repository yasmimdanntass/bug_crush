# 🪲 BUG CRUSH 🪲
## O JOGO DO PROGRAMADOR!

Caro desenvolvedor de software, está cansado de encontrar bugs profissionalmente e agora quer se dedicar a capturá-los por diversão? Este jogo é para você!

Aqui, você vai ter a experiência de 'Candy Crush' e vários outros jogos de combinação - mas dessa vez, no terminal!

---

### Regras e Metas
* Utilize 3 comandos  para deslocar os insetos em um tabuleiro **8x8**;
* Digite a **linha**, a **coluna**, e **w a s d** para os movimentos;
* Ex.: ``1 2 s`` (desloca a peça da linha 1 e coluna 2 para baixo);

Seu objetivo é atingir **500 pontos** com o mínimo de jogadas possível, num máximo de 15! Para isso, combine as peças em pelo menos trios e planeje combos para aumentar seus multiplicadores.

Boa sorte!

---

### 🛠 Pré-requisitos

Para executar este jogo, você precisará ter o ambiente Haskell instalado em sua máquina:
* **GHC** (Glasgow Haskell Compiler)
* **Cabal** (Gerenciador de pacotes do Haskell)

---

### Como rodar?

Você tem que instalar a biblioteca `random` (necessária para gerar os elementos do jogo).

1. Instale a dependência via Cabal:
```bash
cabal build
```

2. Logo em seguida, rode o comando abaixo dentro do diretório raiz do jogo:
```bash
cabal run bug-crush
```

E está pronto o sorvetinho! 
---

### Desenvolvedoras

* Ana Paula Soares Tôrres Cassimiro
* Ariany da Silva de Macena
* Maria Eduarda Ramos Lucena Maia
* Yasmim Dantas da Costa Souza
