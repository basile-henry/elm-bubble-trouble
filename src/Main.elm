module Main where

import Char exposing (KeyCode, toCode)
import Color exposing (..)
import Graphics.Element exposing (Element, down, left, right, midRight, flow, leftAligned, relative, midTopAt, container)
import AnimationFrame exposing (frame)
import Signal exposing (foldp, merge)
import Window exposing (dimensions)
import Time exposing (Time, inSeconds, timestamp)
import Signal exposing (map, map2)
import Math.Vector2 exposing (vec2)
import Keyboard exposing (keysDown)
import Set exposing (toList)
import Text exposing (height, fromString)
import String exposing (append)

import BubbleTrouble.Game as Game
import BubbleTrouble.Ball as Ball
import BubbleTrouble.Player as Player

maxDt : Float
maxDt = 0.05

dims : (Int, Int)
dims = (720, 512)

type alias Model =
    { fps : Float
    , game : Game.Model
    }

main : Signal Element
main =
    let ticks    = map (Ticks << toDts << inSeconds) frame
        controls = map Control << timestamp <| map toList keysDown
        signals  = merge controls ticks
    in
        signals
            |> foldp update init
            |> map2 view dimensions

type Action = Ticks (List Float)
            | Control (Time, List KeyCode)

update : Action -> Model -> Model
update action model =
    case action of
        Ticks dts -> 
            let update' dt g = Game.update (Game.Tick dt) g
                game' = List.foldr update' model.game dts
            in
                { model
                    | game = game'
                    , fps = 1 / List.sum dts
                }

        Control ctrls ->
            { model | game = Game.update (Game.Control ctrls) model.game }

view : (Int, Int) -> Model -> Element
view (wx, wy) model =
    let gameView =
            Game.view model.game
        fpsView =
            model.fps
                |> round
                |> toString
                |> String.padLeft 3 ' '
                |> append "fps: "
                |> fromString
                |> height 20
                |> leftAligned
                |> container (fst dims) 30 midRight
        views = [gameView, fpsView]
    in
        flow down views
                |> container wx wy (midTopAt (relative 0.5) (relative 0.05))

toDts : Time -> List Float
toDts dt =
    if dt < maxDt then
        [dt]
    else
        maxDt :: (toDts <| dt - maxDt)

init : Model
init =
    Model 0
        <| Game.Model
            dims
            [ Ball.Model (vec2  250 100) (vec2 -30 0) 50.0 -250 lightGreen 3 dims
            , Ball.Model (vec2 -300   0) (vec2  50 0) 30.0 -250 lightBlue  1 dims
            ]
            [ (Game.Controls 37 39 38 , Player.newPlayer red dims)
            ]

