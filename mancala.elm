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
import Debug exposing (..)

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
  if t < duration then from + (to - from)*(t/duration) else to
  
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

type AnimationStyle = Scale | PosY
type alias Animation = { style: AnimationStyle, tween: Float -> Float, i: Int, t: Float }

tickAnimation : Time -> Animation -> Animation
tickAnimation ms animation = { animation | t <- animation.t + (inSeconds ms) }

tickAnimationList : Time -> List Animation -> List Animation
tickAnimationList ms animations = List.map (tickAnimation ms) animations

tickBoard : Board -> Animation -> Board
tickBoard board animation =
  let
    i = animation.i
    v = animation.tween animation.t
  in
    case animation.style of
      Scale -> { board | scales <- set i v board.scales }
      PosY -> { board | scales <- set i v board.posYs }
      
tickBoardAnimations : List Animation -> Board -> Board
tickBoardAnimations animations board =
  List.foldl (\anim board -> tickBoard board anim) board animations

animateBoard : Time -> Board -> Board
animateBoard ms board =
  let
    i = board.homeA
    v = withDefault 1 <| get i board.scales
  in
    { board | scales <- set i (v + 0.01) board.scales }

animationList : List Animation
animationList = [{style = Scale, tween = lerp 1 2 0.5, i = 0, t = 0}]

isActive animation = animation.t < 10

layer1 = Signal.map drawBoard gameBoard'
layer2 = Signal.map2 (\board pos -> show <| getPitFromMouse board pos) gameBoard' Mouse.position
deltaTime = fps 30
gameBoard' = Signal.foldp tickBoardAnimations gameBoard animationList3'
animationList' = Signal.foldp tickAnimationList animationList deltaTime
animationList2' = Signal.map (\animList -> List.filter isActive animList) animationList'

animationList3' = Debug.watch "animationList" <~ animationList2'

--gameMerge = Signal.map2 (\animationList dt -> Signal.foldp (tickBoardAnimations animationList) gameBoard dt) animationList' deltaTime
  
main : Signal Element
main =
  Signal.map2 above layer1 layer2
  --show gameBoard.posXs
  --collage boardWidth boardHeight <| drawPit gameBoard 8
  --show <| fromList <| List.reverse <| toList pitXs
  --Signal.map (\a -> show <| toCartesian a) Mouse.position
  --show <| lerp 0 10 (5*second) (3.5*second)
  --Signal.map drawBoard <| Signal.foldp animateBoard gameBoard (fps 30)
  
  
