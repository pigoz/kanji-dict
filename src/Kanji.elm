module Kanji exposing
  (
    StrokePoint,
    Stroke,
    Kanji,
    empty,
    render,

    newStroke,
    addToStroke,
    endStroke
  )

import Collage exposing (defaultLine)

type alias StrokePoint = (Float, Float)
type alias Stroke = List StrokePoint
type alias Kanji = {
  drawing: Bool,
  prev: List Stroke,
  last: Stroke -- staging area where we draw
}

type alias KanjiEdit = Kanji -> StrokePoint -> Kanji

empty : Kanji
empty =
  { drawing = False, prev = [], last = [] }

strokes : Kanji -> List Stroke
strokes k =
  k.prev ++ [k.last]

txstroke : (StrokePoint -> StrokePoint) -> Stroke -> Stroke
txstroke fn s = List.map fn s

transform : (StrokePoint -> StrokePoint) -> Kanji -> Kanji
transform fn k =
  { k | prev = List.map (txstroke fn) k.prev, last = (txstroke fn) k.last }

newStroke : KanjiEdit
newStroke k p =
  { prev = k.prev ++ [k.last], last = [p], drawing = True }

addToStroke : KanjiEdit
addToStroke k p =
  case k.drawing of
    True -> { k | last = k.last ++ [p] }
    False -> k

endStroke : KanjiEdit
endStroke k p =
  let r = addToStroke k p
  in { r | drawing = False }

type alias Viewport = { w: Float, h: Float }

-- http://package.elm-lang.org/packages/evancz/elm-graphics/1.0.1/Collage
originTopToCenter : Viewport -> (Float, Float) -> (Float, Float)
originTopToCenter wp pt =
  let
    ptx = Tuple.first pt
    pty = Tuple.second pt
  in
    (ptx - wp.w / 2, wp.h / 2 - pty)

render : Viewport -> Kanji -> List Collage.Form
render wp k =
  let
    ss = k |> transform (originTopToCenter wp) |> strokes
    style = { defaultLine | width = 10 }
  in List.map (\x -> Collage.traced style (Collage.path x)) ss
