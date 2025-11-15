module Types
  ( Item(..)
  , Inventario
  , AcaoLog(..)
  , StatusLog(..)
  , LogEntry(..)
  , ResultadoOperacao
  ) where

import Data.Time (UTCTime)
import qualified Data.Map as Map


data Item = Item
  { itemID     :: String
  , nome       :: String
  , quantidade :: Int
  , categoria  :: String
  } deriving (Show, Read)


type Inventario = Map.Map String Item

data AcaoLog
  = Adicionar
  | Remover
  | Atualizar
  deriving (Show, Read)

data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read)

type ResultadoOperacao = (Inventario, LogEntry)
