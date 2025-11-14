
# Sistema de Inventário em Haskell

## Informações do Projeto

**Instituição:** Pontifícia Universidade Católica do Paraná (PUCPR)  
**Disciplina:** Programação Lógica  
**Professor:** Frank Alcantara 
**Grupo:** 8  

### Integrantes (em ordem alfabética):
- **Bernardo Muller** - [@zzMuller](https://github.com/zzMuller)
- **João Pedro Cochek** - [@Cochek2903](https://github.com/Cochek2903)
- **Luís Felipe Tozzi** - [@LuisTozzi1](https://github.com/LuisTozzi1)
- **Luiz Fernando Brasão** - [@lf-bf](https://github.com/lf-bf)

## Como Compilar e Executar

### Online GDB
Para executar o programa no ambiente online, acesse:  
**[GDB Online - Haskell Compiler](https://onlinegdb.com/q8dP3i7km)**

### Instruções de Compilação:
1. Acesse o GDB Online no link acima
2. Clique em "Run" para compilar e executar o programa

## Comandos Disponíveis

O sistema oferece os seguintes comandos interativos:

- **adicionar** - Adicionar novo item ao inventário
- **remover** - Remover quantidade de um item existente
- **atualizar** - Atualizar quantidade de um item existente
- **listar** - Listar todos os itens no inventário
- **reportar** - Consultar logs de auditoria e relatórios
- **sair** - Encerrar o sistema

## Estrutura do Projeto

```
grupo-8/
├── main.hs                      # Arquivo principal - ponto de entrada do programa
├── Types.hs                     # Definições de tipos e estruturas de dados
├── Logic_v2.hs                  # Lógica principal do sistema (operações CRUD)
├── Reports.hs                   # Módulo de relatórios e auditoria
├── README.MD                    # Documentação do projeto
├── Cenario_1_resultados/        # Diretório com resultados do cenário 1
├── Cenario_2:3_resultados/      # Diretório com resultados dos cenários 2 e 3
├── Inventario.dat              # Arquivo de persistência do inventário (gerado automaticamente)
└── Auditoria.log               # Arquivo de log de auditoria (gerado automaticamente)
```

### Descrição dos Arquivos:

- **`main.hs`** - Contém a função main e o loop principal de interação com o usuário. Coordena todas as operações do sistema.
- **`Types.hs`** - Define os tipos de dados utilizados no sistema (Item, LogEntry, Status, Acao, etc.) e suas instâncias.
- **`Logic_v2.hs`** - Implementa a lógica de negócio do sistema, incluindo operações de adicionar, remover, atualizar e listar itens.
- **`Reports.hs`** - Responsável pela geração de relatórios, consulta de logs e funcionalidades de auditoria.
- **`Inventario.dat`** - Arquivo binário que armazena o estado atual do inventário de forma persistente.
- **`Auditoria.log`** - Arquivo de texto que registra todas as operações realizadas no sistema com timestamp e status.

## Exemplos de Uso

# Cenário 1:
```
Sistema de Inventário

Comandos disponíveis:
  adicionar  - Adicionar novo item
  remover    - Remover quantidade de um item
  atualizar  - Atualizar quantidade de um item
  listar     - Listar o inventario
  reportar   - Consultar logs
  sair       - Encerrar o sistema
> adicionar

== Adicionar Item ==
ID do item: 1
Nome: flechas
Quantidade inicial: 30
Categoria: armamento
Operação realizada com sucesso.

Comandos disponíveis:
  adicionar  - Adicionar novo item
  remover    - Remover quantidade de um item
  atualizar  - Atualizar quantidade de um item
  listar     - Listar o inventario
  reportar   - Consultar logs
  sair       - Encerrar o sistema
> adicionar

== Adicionar Item ==
ID do item: 2
Nome: batata
Quantidade inicial: 5
Categoria: comida
Operação realizada com sucesso.

Comandos disponíveis:
  adicionar  - Adicionar novo item
  remover    - Remover quantidade de um item
  atualizar  - Atualizar quantidade de um item
  listar     - Listar o inventario
  reportar   - Consultar logs
  sair       - Encerrar o sistema
> adicionar

== Adicionar Item ==
ID do item: 3
Nome: madeira
Quantidade inicial: 20
Categoria: material
Operação realizada com sucesso.

Comandos disponíveis:
  adicionar  - Adicionar novo item
  remover    - Remover quantidade de um item
  atualizar  - Atualizar quantidade de um item
  listar     - Listar o inventario
  reportar   - Consultar logs
  sair       - Encerrar o sistema
> sair
Encerrando o sistema.


...Program finished with exit code 0
Press ENTER to exit console.
```

## Auditoria.log

```
LogEntry {timestamp = 2025-11-14 19:11:50.748492241 UTC, acao = Adicionar, detalhes = "ADICIONADO 1 (flechas), quantidade =30", status = Sucesso}
LogEntry {timestamp = 2025-11-14 19:12:12.211584897 UTC, acao = Adicionar, detalhes = "ADICIONADO 2 (batata), quantidade =5", status = Sucesso}
LogEntry {timestamp = 2025-11-14 19:12:38.764930864 UTC, acao = Adicionar, detalhes = "ADICIONADO 3 (madeira), quantidade =20", status = Sucesso}
```

## Inventario.dat

```
fromList [("1",Item {itemID = "1", nome = "flechas", quantidade = 30, categoria = "armamento"}),("2",Item {itemID = "2", nome = "batata", quantidade = 5, categoria = "comida"}),("3",Item {itemID = "3", nome = "madeira", quantidade = 20, categoria = "material"})]
```

## Sistema reiniciado (demonstrando persistência)

```
[1 of 5] Compiling Types            ( Types.hs, Types.o )
[2 of 5] Compiling Reports          ( Reports.hs, Reports.o )
[3 of 5] Compiling LogicV2          ( Logic_v2.hs, Logic_v2.o )
[4 of 5] Compiling Main             ( main.hs, main.o )
[5 of 5] Linking a.out
Sistema de Inventário

Comandos disponíveis:
  adicionar  - Adicionar novo item
  remover    - Remover quantidade de um item
  atualizar  - Atualizar quantidade de um item
  listar     - Listar o inventario
  reportar   - Consultar logs
  sair       - Encerrar o sistema
> listar

== Itens no Inventário ==
ID: 1 | Nome: flechas | Quantidade: 30 | Categoria: armamento
ID: 2 | Nome: batata | Quantidade: 5 | Categoria: comida
ID: 3 | Nome: madeira | Quantidade: 20 | Categoria: material

```

# Cenário 2

```
[1 of 5] Compiling Types            ( Types.hs, Types.o )
[2 of 5] Compiling Reports          ( Reports.hs, Reports.o )
[3 of 5] Compiling LogicV2          ( Logic_v2.hs, Logic_v2.o )
[4 of 5] Compiling Main             ( main.hs, main.o )
[5 of 5] Linking a.out
Sistema de Inventário

Comandos disponíveis:
  adicionar  - Adicionar novo item
  remover    - Remover quantidade de um item
  atualizar  - Atualizar quantidade de um item
  listar     - Listar o inventario
  reportar   - Consultar logs
  sair       - Encerrar o sistema
> adicionar

== Adicionar Item ==
ID do item: 1
Nome: flechas
Quantidade inicial: 10
Categoria: armamento
Operação realizada com sucesso.

Comandos disponíveis:
  adicionar  - Adicionar novo item
  remover    - Remover quantidade de um item
  atualizar  - Atualizar quantidade de um item
  listar     - Listar o inventario
  reportar   - Consultar logs
  sair       - Encerrar o sistema
> remover

== Remover Quantidade ==
ID do item: 1
Quantidade a remover: 15
Erro: Estoque insuficiente para a remoção.

Comandos disponíveis:
  adicionar  - Adicionar novo item
  remover    - Remover quantidade de um item
  atualizar  - Atualizar quantidade de um item
  listar     - Listar o inventario
  reportar   - Consultar logs
  sair       - Encerrar o sistema
> listar

== Itens no Inventário ==
ID: 1 | Nome: flechas | Quantidade: 10 | Categoria: armamento
```

## Inventario.dat

```
fromList [("1",Item {itemID = "1", nome = "flechas", quantidade = 10, categoria = "armamento"})]
```

## Auditoria.log

```
LogEntry {timestamp = 2025-11-14 19:28:46.989557345 UTC, acao = Adicionar, detalhes = "ADICIONADO 1 (flechas), quantidade =10", status = Sucesso}
LogEntry {timestamp = 2025-11-14 19:29:06.378739269 UTC, acao = Remover, detalhes = "Estoque insuficiente para a remo\231\227o.", status = Falha "Estoque insuficiente para a remo\231\227o."}
```

# Cenário 3

```
Comandos disponíveis:
  adicionar  - Adicionar novo item
  remover    - Remover quantidade de um item
  atualizar  - Atualizar quantidade de um item
  listar     - Listar o inventario
  reportar   - Consultar logs
  sair       - Encerrar o sistema
> reportar

== Relatórios de Auditoria ==

-- Logs de Erro --
LogEntry {timestamp = 2025-11-14 19:29:06.378739269 UTC, acao = Remover, detalhes = "Estoque insuficiente para a remo\231\227o.", status = Falha "Estoque insuficiente para a remo\231\227o."}

Digite um ID para ver o histórico do item (ou deixe vazio para pular): 1

Histórico para o item '1':
LogEntry {timestamp = 2025-11-14 19:28:46.989557345 UTC, acao = Adicionar, detalhes = "ADICIONADO 1 (flechas), quantidade =10", status = Sucesso}

Item mais movimentado (aproximado): remoçao.

Comandos disponíveis:
  adicionar  - Adicionar novo item
  remover    - Remover quantidade de um item
  atualizar  - Atualizar quantidade de um item
  listar     - Listar o inventario
  reportar   - Consultar logs
  sair       - Encerrar o sistema
> sair
Encerrando o sistema.


...Program finished with exit code 0
Press ENTER to exit console.
```