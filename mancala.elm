import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (..)
import Array exposing (..)
import Maybe exposing (..)

colA = Color.blue
colB = Color.purple
pitSize = 20
homeSize = 40
boardWidth = 400
boardHeight = 300
boardHW = boardWidth/2
boardHH = boardHeight/2
textSizeMod = 1.6
numPits = 6

type alias Board = { values: Array Int, homeA: Int, pitA0: Int, homeB: Int, pitB0: Int, posXs : Array Float, posYs : Array Float }

pitXs = Array.initialize numPits (\n -> 1.2*(toFloat n)/(toFloat (numPits - 1)) - 0.6)

prepend : a -> Array a -> Array a
prepend n a = Array.append (fromList [n]) a

reverse : Array a -> Array a
reverse a = fromList <| List.reverse <| toList a

gameBoard = { 
  values = repeat (2 + numPits*2) 0, 
  posXs = prepend -0.8 <| Array.append pitXs <| prepend 0.8 <| reverse pitXs,
  posYs = prepend 0.0 <| Array.append (repeat numPits 0.6) <| prepend 0.0 <| repeat numPits -0.6,
  homeA = 0, 
  pitA0 = 1, 
  homeB = 1 + numPits, 
  pitB0 = 2 + numPits }

drawPit : Board -> Int -> List Form
drawPit board i =
  let
    x = (withDefault 0 <| get i board.posXs) * boardHW
    y = (withDefault 0 <| get i board.posYs) * boardHH
    s = if i == board.homeA || i == board.homeB then homeSize else pitSize
    c = if i < numPits + 1 then colA else colB
    v = toString <| withDefault 0 <| get i board.values
    pitStyle s c = { defaultStyle | color <- c, bold <- True, height <- Just (s * textSizeMod) }    
    valueText s c v = fromString v |> Text.style (pitStyle s c) |> text |> moveY (s*0.2)
  in
    [ circle s |> outlined (solid c) |> move (x, y), 
      valueText s c v |> move (x, y) ]

drawBoard : Board -> Element
drawBoard board =
    collage boardWidth boardHeight <| List.concatMap (\i -> drawPit board i) <| [0..(2*numPits + 1)]
    
main : Element
main =
  drawBoard gameBoard
  --show gameBoard.posXs
  --collage boardWidth boardHeight <| drawPit gameBoard 8
  --show <| fromList <| List.reverse <| toList pitXs