import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (..)
import Array exposing (..)

colA = Color.blue
colB = Color.purple
pitSize = 20
homeSize = 40
boardWidth = 300
boardHeight = 300
textSizeMod = 1.6
numPits = 6

type alias Board = { values: Array Int, homeA: Int, pitA0: Int, homeB: Int, pitB0: Int }

gameBoard = { values = repeat (2 + numPits*2) 0, homeA = 0, pitA0 = 1, homeB = 1 + numPits, pitB0 = 2 + numPits }

drawBoard : Board -> Element
drawBoard board =
  let
    pitStyle s c = { defaultStyle | color <- c, bold <- True, height <- Just (s * textSizeMod) }    
    valueText s c val = fromString val |> Text.style (pitStyle s c) |> text |> moveY (s*0.2)
    pit s c val = [circle s |> outlined (solid c), valueText s c val]
    boardValue i = 
      case (get i board.values) of 
        Just n -> toString n
        _ -> "0"
    homeA = pit homeSize colA (boardValue board.homeA)
    homeB = pit homeSize colB (boardValue board.homeB)
  in
    collage boardWidth boardHeight (List.map (\el -> el |> moveX 100) homeA ++ List.map (\el -> el |> moveX -100) homeB)
    
main : Element
main =
  drawBoard gameBoard