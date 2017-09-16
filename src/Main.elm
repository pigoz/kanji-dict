import Html exposing (..)

import Color

import Kanji as K
import Canvas

type alias Model = { k : K.Kanji }

type Msg
  = CanvasMsg Canvas.Msg

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

init : (Model, Cmd Msg)
init =
  ({ k = K.empty }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CanvasMsg m -> ({ k = Canvas.update m model.k }, Cmd.none)

subscriptions :  Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div []
  [
    text "draw your kanji!"
  , Canvas.render 300 model.k |> Html.map CanvasMsg
  ]

