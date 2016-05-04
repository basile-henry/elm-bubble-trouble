module BubbleTrouble.Game where

import Char exposing (KeyCode)
import Color exposing (..)
import Graphics.Collage exposing (circle, filled, move, collage)
import Graphics.Element exposing (..)
import Time exposing (Time)

import BubbleTrouble.Ball as Ball
import BubbleTrouble.Player as Player

type alias Model =
    { dims : (Int, Int)
    , balls : List Ball.Model
    , players : List ControledPlayer
    }

type alias ControledPlayer = (Controls, Player.Model)

type alias Controls =
    { left : KeyCode
    , right : KeyCode
    , shoot : KeyCode
    }

type Action = Tick Float
            | Control (Time, List KeyCode)

update : Action -> Model -> Model
update action model =
    case action of
        Tick dt ->
            { model
                | balls = List.map (Ball.update <| Ball.Tick dt) model.balls
                , players = List.map (updatePlayer dt) model.players
            }


        Control (t, keys) ->
            { model
                | players = List.map (controlPlayer t keys) model.players
            }

view : Model -> Element
view model =
    let balls   = List.map Ball.view model.balls
        players = List.map Player.view <| List.map snd model.players
        forms   = List.append players balls
    in
        forms
            |> collage (fst model.dims) (snd model.dims)
            |> color lightYellow

controlPlayer : Time -> List KeyCode -> ControledPlayer -> ControledPlayer
controlPlayer t keys (ctrls, player) =
    let player' =
            if List.member ctrls.left keys then
                if List.member ctrls.right keys then
                    Player.update (Player.Go Player.Stop) player
                else
                    Player.update (Player.Go Player.Left) player
            else if List.member ctrls.right keys then
                Player.update (Player.Go Player.Right) player
            else
                Player.update (Player.Go Player.Stop) player
        player'' =
            if List.member ctrls.shoot keys then
                Player.update (Player.Shoot t) player'
            else
                player'
    in
        (ctrls, player'')


updatePlayer : Float -> ControledPlayer -> ControledPlayer
updatePlayer dt (ctrls, player) =
    (ctrls, Player.update (Player.Tick dt) player)
