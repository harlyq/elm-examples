import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (..)
import Array exposing (..)
import Maybe exposing (..)
import Mouse exposing (..)
import Time exposing (..)
import Signal exposing (..)
import Mouse exposing (..)

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
totalPits = 2 + numPits*2

lerp : Float -> Float -> Time -> Time -> Float
lerp from to duration t =
  from + (to - from)*(t/duration)
  
toCartesian : (Int, Int) -> (Float, Float)
toCartesian (x,y) = ((toFloat x)/boardWidth*2 - 1, 1 - (toFloat y)/boardHeight*2)

type alias Board = { 
  values: Array Int, 
  homeA: Int, 
  pitA0: Int,
  homeB: Int, 
  pitB0: Int, 
  posXs : Array Float, 
  posYs : Array Float,
  scales : Array Float}

pitXs = Array.initialize numPits (\n -> 1.2*(toFloat n)/(toFloat (numPits - 1)) - 0.6)

pushl : a -> Array a -> Array a
pushl n a = Array.append (fromList [n]) a

reverse : Array a -> Array a
reverse a = fromList <| List.reverse <| toList a

gameBoard = { 
  values = repeat totalPits 0, 
  posXs = pushl -0.8 <| Array.append pitXs <| pushl 0.8 <| reverse pitXs,
  posYs = pushl 0.0 <| Array.append (repeat numPits 0.6) <| pushl 0.0 <| repeat numPits -0.6,
  scales = repeat totalPits 1,
  homeA = 0, 
  pitA0 = 1, 
  homeB = 1 + numPits, 
  pitB0 = 2 + numPits }
  
pitX board i = (withDefault 0 <| get i board.posXs) * boardHW
pitY board i = (withDefault 0 <| get i board.posYs) * boardHH
pitS board i = (withDefault 1 <| get i board.scales) * (if i == board.homeA || i == board.homeB then homeSize else pitSize)

drawPit : Board -> Int -> List Form
drawPit board i =
  let
    x = pitX board i
    y = pitY board i
    r = pitS board i
    c = if i < numPits + 1 then colA else colB
    v = toString <| withDefault 0 <| get i board.values
    pitStyle r c = { defaultStyle | color <- c, bold <- True, height <- Just (r * textSizeMod) }    
    valueText r c v = fromString v |> Text.style (pitStyle r c) |> text |> moveY (r*0.2)
  in
    [ circle r |> outlined (solid c) |> move (x, y), 
      valueText r c v |> move (x, y) ]

isInside : Board -> (Int, Int) -> Int -> Bool
isInside board (mx,my) i =
  let
    x = (pitX board i) - (toFloat mx - boardHW)
    y = (pitY board i) - (toFloat my - boardHH)
    r = pitS board i
  in
    sqrt(x*x + y*y) < r
    
getPitFromMouse : Board -> (Int, Int) -> Maybe Int
getPitFromMouse board (mx,my) =
  List.head <| List.filter (isInside board (mx,my)) [0 .. totalPits - 1]
  
drawBoard : Board -> Element
drawBoard board =
  collage boardWidth boardHeight <| List.concatMap (drawPit board) <| [0..(2*numPits + 1)]

animateBoard : Time -> Board -> Board
animateBoard ms board =
  let
    i = board.homeA
    v = withDefault 1 <| get i board.scales
  in
    { board | scales <- set i (v + 0.01) board.scales }
    
layer1 = drawBoard gameBoard
layer2 = Signal.map (show << getPitFromMouse gameBoard) Mouse.position
  
main : Signal Element
main =
  Signal.map (above layer1) layer2
  --show gameBoard.posXs
  --collage boardWidth boardHeight <| drawPit gameBoard 8
  --show <| fromList <| List.reverse <| toList pitXs
  --Signal.map (\a -> show <| toCartesian a) Mouse.position
  --show <| lerp 0 10 (5*second) (3.5*second)
  --Signal.map drawBoard <| Signal.foldp animateBoard gameBoard (fps 30)
  
  
