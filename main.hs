module Main where

import System.IO (hFlush, stdout)
import Control.Exception (catch, IOException)
import Data.Time (getCurrentTime)
import qualified Data.Map as Map
import Data.Either (either)


import Types
import LogicV2
import Reports


arquivoInventario :: FilePath
arquivoInventario = "Inventario.dat"

arquivoLog :: FilePath
arquivoLog = "Auditoria.log"


lerArquivo :: Read arquivo => FilePath -> arquivo -> IO arquivo
lerArquivo caminho padrao = do
  conteudo <- readFile caminho `catch` (\(_ :: IOException) -> return "")
  if null conteudo
    then return padrao
    else case reads conteudo of
           [(valor, "")] -> return valor
           _             -> return padrao

salvarInventario :: Inventario -> IO ()
salvarInventario inventario =
  writeFile arquivoInventario (show inventario)

registrarLog :: LogEntry -> IO ()
registrarLog entrada =
  appendFile arquivoLog (show entrada ++ "\n")

main :: IO ()
main = do
  putStrLn "Sistema de Inventário"
  invInicial   <- lerArquivo arquivoInventario Map.empty
  logsIniciais <- lerArquivo arquivoLog []
  cicloDeComandos invInicial logsIniciais

cicloDeComandos :: Inventario -> [LogEntry] -> IO ()
cicloDeComandos inventario logs = do
  putStrLn ""
  putStrLn "Comandos disponíveis:"
  putStrLn "  adicionar  - Adicionar novo item"
  putStrLn "  remover    - Remover quantidade de um item"
  putStrLn "  atualizar  - Atualizar quantidade de um item"
  putStrLn "  listar     - Listar o inventario"
  putStrLn "  reportar   - Consultar logs"
  putStrLn "  sair       - Encerrar o sistema"
  putStr   "> "
  hFlush stdout
  comando <- getLine
  case comando of
    "adicionar"    -> comandoAdicionar inventario logs
    "remover" -> comandoRemover inventario logs
    "atualizar" -> comandoAtualizar inventario logs
    "listar" -> comandoListar inventario logs
    "reportar" -> comandoRelatorio inventario logs
    "sair"   -> putStrLn "Encerrando o sistema."
    _        -> do
      putStrLn "Comando inválido."
      cicloDeComandos inventario logs

-- Funções utéis 

solicitarEntrada :: String -> IO String
solicitarEntrada texto = do
  putStr (texto ++ ": ")
  hFlush stdout
  getLine

lerInteiro :: String -> Maybe Int
lerInteiro string = case reads string of [(valor, "")] -> Just valor; _ -> Nothing

-- processa o resultado de cada operacao
processarOperacao :: AcaoLog -> Inventario -> [LogEntry] -> Either String ResultadoOperacao -> IO ()
processarOperacao acaoLog inventario logs resultado = do
    tempo <- getCurrentTime
    
    let tratarErro erro = do
          let entradaLog = criarLogFalha tempo acaoLog erro
          registrarLog entradaLog
          putStrLn ("Erro: " ++ erro)
          cicloDeComandos inventario (entradaLog:logs)

    let tratarSucesso (novoInv, entradaLog) = do
          salvarInventario novoInv
          registrarLog entradaLog
          putStrLn "Operação realizada com sucesso."
          cicloDeComandos novoInv (entradaLog:logs)
    
    either tratarErro tratarSucesso resultado
    
    
-- funcao genérica que encapsula a lógica do processamento com quantidade
processarComandoComQuantidade
  :: AcaoLog
  -> Inventario
  -> [LogEntry]
  -> String  
  -> (Int -> IO (Either String ResultadoOperacao)) 
  -> IO ()
processarComandoComQuantidade acaoLog inventario logs quantidadeString acaoDeSucesso =
  case lerInteiro quantidadeString of
    Nothing -> do
      tempo <- getCurrentTime
      let mensagem = "Quantidade inválida fornecida."
          logEnt = criarLogFalha tempo acaoLog mensagem
      registrarLog logEnt
      putStrLn mensagem
      cicloDeComandos inventario (logEnt:logs)
    Just quantidade -> do
      resultado <- acaoDeSucesso quantidade
      processarOperacao acaoLog inventario logs resultado
      
      

-- Comandos de interaçao com o user

comandoAdicionar :: Inventario -> [LogEntry] -> IO ()
comandoAdicionar inventario logs = do
  putStrLn "\n== Adicionar Item =="
  identificador <- solicitarEntrada "ID do item"
  nome          <- solicitarEntrada "Nome"
  quantidadeString <- solicitarEntrada "Quantidade inicial"
  categoria     <- solicitarEntrada "Categoria"
  
  processarComandoComQuantidade Adicionar inventario logs quantidadeString $ \quantidade -> do
    tempo <- getCurrentTime
    return (adicionarItem tempo identificador nome quantidade categoria inventario)

comandoRemover :: Inventario -> [LogEntry] -> IO ()
comandoRemover inventario logs = do
  putStrLn "\n== Remover Quantidade =="
  identificador <- solicitarEntrada "ID do item"
  quantidadeString <- solicitarEntrada "Quantidade a remover"

  processarComandoComQuantidade Remover inventario logs quantidadeString $ \quantidade -> do
    tempo <- getCurrentTime
    return (removerItem tempo identificador quantidade inventario)

comandoAtualizar :: Inventario -> [LogEntry] -> IO ()
comandoAtualizar inventario logs = do
  putStrLn "\n== Atualizar Quantidade =="
  identificador <- solicitarEntrada "ID do item"
  quantidadeString <- solicitarEntrada "Nova quantidade"
  
  processarComandoComQuantidade Atualizar inventario logs quantidadeString $ \quantidade -> do
    tempo <- getCurrentTime
    return (atualizarQuantidade tempo identificador quantidade inventario)

comandoListar :: Inventario -> [LogEntry] -> IO ()
comandoListar inventario logs = do
  putStrLn "\n== Itens no Inventário =="
  if Map.null inventario
    then putStrLn "O inventário está vazio."
    else mapM_ imprimirItem (Map.elems inventario)
  cicloDeComandos inventario logs
  where
    imprimirItem (Item id nome quantidade categoria) =
      putStrLn ("ID: "++id ++ " | Nome: " ++ nome ++ " | Quantidade: " ++ show quantidade ++ " | Categoria: " ++ categoria)

comandoRelatorio :: Inventario -> [LogEntry] -> IO ()
comandoRelatorio inventario logs = do
  putStrLn "\n== Relatórios de Auditoria =="
  putStrLn "\n-- Logs de Erro --"
  mapM_ (putStrLn . show) (logsDeErro logs)
  idItem <- solicitarEntrada "\nDigite um ID para ver o histórico do item (ou deixe vazio para pular)"
  if not (null idItem)
    then do
      putStrLn ("\nHistórico para o item '" ++ idItem ++ "':")
      mapM_ (putStrLn . show) (historicoPorItem idItem logs)
    else return ()
  case itemMaisMovimentado logs of
    Nothing   -> putStrLn "\nNenhum item foi movimentado ainda."
    Just mid  -> putStrLn ("\nItem mais movimentado (aproximado): " ++ mid)
  cicloDeComandos inventario logs
