module Reports
  ( logsDeErro
  , historicoPorItem
  , itemMaisMovimentado
  ) where

import Types
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter seFalha
  where
    seFalha (LogEntry _ _ _ (Falha _)) = True
    seFalha _                          = False

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem identificador =
  filter (\log -> identificador `elem` words (detalhes log))

itemMaisMovimentado :: [LogEntry] -> Maybe String
itemMaisMovimentado logs
  | null logs = Nothing
  | otherwise =
      let termosTotais :: [String]
          termosTotais = concatMap (words . detalhes) logs

          contagemTermos :: Map.Map String Int
          contagemTermos = Map.fromListWith (+) [ (termo, 1) | termo <- termosTotais ]

          listaDeContagens :: [(String, Int)]
          listaDeContagens = Map.toList contagemTermos

      in if null listaDeContagens
           then Nothing
           else Just . fst $ maximumBy (comparing snd) listaDeContagens