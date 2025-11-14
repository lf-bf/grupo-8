module LogicV2
  ( addItem
  , removeItem
  , updateQty
  , criarLogFalha
  ) where

import qualified Data.Map as Map
import Data.Time (UTCTime)
import Control.Monad (when)
import Types


buscarItem :: String -> Inventario -> Either String Item
buscarItem identificador inventario =
  case Map.lookup identificador inventario of
    Nothing -> Left "Item não encontrado."
    Just item -> Right item


addItem
  :: UTCTime -> String -> String -> Int -> String -> Inventario
  -> Either String ResultadoOperacao
addItem tempo identificador nome quantidade categoria inventario = do
  when (null identificador) $
    Left "O ID do item não pode ser vazio."
  when (quantidade <= 0) $
    Left "A quantidade inicial deve ser maior que zero."
  when (Map.member identificador inventario) $
    Left "O ID do item já está cadastrado."

  let novoItem = Item identificador nome quantidade categoria
      inventarioAtualizado = Map.insert identificador novoItem inventario
      mensagemLog = "ADICIONADO " ++ identificador ++ " (" ++ nome ++ "), quantidade =" ++ show quantidade
      entradaLog = LogEntry tempo Adicionar mensagemLog Sucesso
  
  return (inventarioAtualizado, entradaLog)


removeItem
  :: UTCTime -> String -> Int -> Inventario
  -> Either String ResultadoOperacao
removeItem tempo identificador quantidadeARemover inventario = do
  itemExistente <- buscarItem identificador inventario
  
  when (quantidadeARemover <= 0) $
    Left "A quantidade a remover deve ser maior que zero."
  when (quantidade itemExistente < quantidadeARemover) $
    Left "Estoque insuficiente para a remoção."

  let novaQuantidade = quantidade itemExistente - quantidadeARemover
      funcaoDeAtualizacao _ = 
        if novaQuantidade > 0 
          then Just (itemExistente { quantidade = novaQuantidade }) 
          else Nothing
            
      inventarioAtualizado = Map.update funcaoDeAtualizacao identificador inventario
      mensagemLog = "REMOVIDO " ++ show quantidadeARemover ++ " de " ++ identificador
      entradaLog = LogEntry tempo Remover mensagemLog Sucesso

  return (inventarioAtualizado, entradaLog)


updateQty
  :: UTCTime -> String -> Int -> Inventario
  -> Either String ResultadoOperacao
updateQty tempo identificador novaQuantidade inventario = do
  itemExistente <- buscarItem identificador inventario

  when (novaQuantidade < 0) $
    Left "A quantidade não pode ser um valor negativo."

  let itemAtualizado = itemExistente { quantidade = novaQuantidade }
      inventarioAtualizado = Map.insert identificador itemAtualizado inventario
      mensagemLog = "ATUALIZADO " ++ identificador ++ " -> quantidade =" ++ show novaQuantidade
      entradaLog = LogEntry tempo Atualizar mensagemLog Sucesso

  return (inventarioAtualizado, entradaLog)


criarLogFalha :: UTCTime -> AcaoLog -> String -> LogEntry
criarLogFalha tempo acao mensagem = LogEntry tempo acao mensagem (Falha mensagem)