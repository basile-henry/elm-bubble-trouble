module BubbleTrouble.Game where

import Char exposing (KeyCode)
import Color exposing (..)
import Graphics.Collage exposing (circle, filled, move, collage)
import Graphics.Element exposing (..)
import Time exposing (Time)

import Utils exposing (lum)
import BubbleTrouble.Ball as Ball
import BubbleTrouble.Player as Player
import BubbleTrouble.Projectile as Projectile
import BubbleTrouble.Collisions exposing (ballCollisions, playerCollision)

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
            let newBalls = updateBalls dt model.players model.balls
            in
                { model
                    | balls = newBalls
                    , players = List.map (updatePlayer dt newBalls) model.players
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
            |> color (lum 1.15 lightYellow)

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


updatePlayer : Float -> List Ball.Model -> ControledPlayer -> ControledPlayer
updatePlayer dt balls (ctrls, player) =
    (ctrls, playerCollision balls <| Player.update (Player.Tick dt) player)

updateBalls : Float -> List ControledPlayer -> List Ball.Model -> List Ball.Model
updateBalls dt players =
        List.map (Ball.update <| Ball.Tick dt)
            >> ballCollisions (List.concatMap (.projectiles << snd) players)
