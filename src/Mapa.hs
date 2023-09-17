module Mapa (
    carregarMapa, salvarMapa,
    Nome, Localizacao, Estradas, Cidade, Mapa,
    desenharCidade, desenharEstrada, desenharMapa
) where

import Data.List (intercalate)
import Data.Text (pack)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import CodeWorld

-- mapas e seus IO
type Nome = String
type Localizacao = (Double, Double)
type Estradas = [Nome]
type Cidade = (Nome, Localizacao, Estradas)
type Mapa = [Cidade]

carregarMapa :: FilePath -> IO Mapa
carregarMapa = fmap (catMaybes . map parseCSV . lines) . readFile

salvarMapa :: FilePath -> Mapa -> IO ()
salvarMapa fp = writeFile fp . unlines . map cityToCSV
    where 
      cityToCSV :: Cidade -> String
      cityToCSV (n, (x, y), es) = intercalate "," [n, show x, show y, unwords es]

-- desenho de mapas
desenharCidade :: Cidade -> Picture
desenharCidade (name, (x, y), _) = cidade & legendaCidade
  where
    cidade = translated x y $ circle 1 -- talvez criar uma Picture legal para cidades
    legendaCidade = translated x (y - 3) $ lettering $ pack name

desenharEstrada :: Cidade -> Cidade -> Picture
desenharEstrada (_, (x1, y1), _) (_, (x2, y2), _) =
  polyline [(x1, y1), (x2, y2)]

desenharMapa :: Mapa -> Picture
desenharMapa xs =
  pictures [desenharCidade city | city <- xs] <>
  pictures [desenharEstrada city1 city2 | city1@(_, _, es) <- xs,
                                   city2 <- filter (ehVizinha es) xs]
    where
      ehVizinha es (n, _, _) = n `elem` es

-- funções internas
parseCSV :: String -> Maybe Cidade
parseCSV csvString = do
  let values = splitCSV csvString
  case values of
    name : double1Str : double2Str : rest -> do
      double1 <- readMaybe double1Str
      double2 <- readMaybe double2Str
      return (name, (double1, double2), rest)
    _ -> error "Arquivo .mapa inválido"

splitCSV :: String -> [String]
splitCSV [] = []
splitCSV vals =
    let (inicio, resto) = break (== ',') vals
    in inicio : case resto of
                [] -> []
                (_:xs) -> splitCSV xs
