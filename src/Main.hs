import CodeWorld
import Mapa

main :: IO ()
main = drawingOf $ desenharMapa marioWorld
  where
    marioWorld <- carregarMapa "teste.mapa"
