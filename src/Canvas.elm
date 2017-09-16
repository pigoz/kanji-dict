module Canvas exposing (..)

import Color
import Html exposing (..)
import Element
import Collage exposing (Form, LineStyle, defaultLine)
import MouseEvents exposing (MouseEvent, onMouseMove, onMouseDown, onMouseUp, relPos)
import Kanji as K exposing (StrokePoint)

strokePoint : MouseEvent -> StrokePoint
strokePoint event =
  let pos = relPos event
  in (pos.x |> toFloat, pos.y |> toFloat)

type alias Model = K.Kanji

type Msg
  = Move MouseEvent
  | Down MouseEvent
  | Up MouseEvent

update : Msg -> Model -> Model
update msg model =
  case msg of
    Down e -> K.newStroke model <| strokePoint e
    Move e -> K.addToStroke model <| strokePoint e
    Up e -> K.endStroke model <| strokePoint e

render : Int -> Model -> Html Msg
render size model =
  let
    sizef = toFloat size
    drawing = (layout sizef) ++ (K.render { w = sizef , h = sizef } model)
  in
  div [ onMouseMove Move, onMouseDown Down, onMouseUp Up ]
    [
      Collage.collage size size drawing |> Element.toHtml
    ]

layoutLine : LineStyle -> Float -> Float -> Form
layoutLine style dim x =
  Collage.rotate
    (degrees x)
    (Collage.traced style (Collage.segment (0, -dim) (0, dim)))

layout : Float -> List Form
layout size =
  let
    style = Collage.solid <| Color.grayscale 0.2
    border = Collage.square size |> Collage.outlined style
    lines = [0, 45, 90, 135] |> List.map (layoutLine style size)
  in
    border :: lines
