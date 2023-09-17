import CodeWorld
import Mapa

main :: IO ()
main = do
   marioWorld <- carregarMapa "teste.mapa"
   drawingOf $ desenharMapa marioWorld
