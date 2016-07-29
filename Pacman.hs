import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

-- All the types we need
data MoveDir = Right | Left | Up | Down

data Actor = Actor { x :: Double
                   , y :: Double
                   , dir :: MoveDir
                   }

data GameState = GameState { pacman :: Actor
                           }

-- Global constants
pacmanRadius :: Double
pacmanRadius = 20

-- Initialization functions
mkCanvas :: Double -> Double -> IO Elem
mkCanvas width height = do
    canvas <- newElem "canvas"
    setProp canvas "width" $ show width
    setProp canvas "height" $ show height
    setStyle canvas "display" "block"
    setStyle canvas "border" "1px solid #524F52"
    setStyle canvas "margin" "0px auto 0 auto"
    setStyle canvas "backgroundColor" "#524F52"
    return canvas

initialState :: GameState
initialState = GameState { pacman = Actor { x = 250.0, y = 250.0, dir = Up } }

-- functions to draw all the game things
yellow :: Picture () -> Picture ()
yellow = color (RGB 200 200 0)

drawPacman :: Actor -> Picture ()
drawPacman (Actor x y _) = yellow $ fill $ circle (x, y) pacmanRadius

gamePicture :: GameState -> Picture ()
gamePicture state = do
    drawPacman $ pacman state

main :: IO ()
main = do
    canvasElem <- mkCanvas 500 500
    appendChild documentBody canvasElem
    Just canvas <- getCanvas canvasElem
    render canvas $ gamePicture initialState

