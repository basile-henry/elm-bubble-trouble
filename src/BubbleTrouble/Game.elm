module BubbleTrouble.Game where

import Char exposing (KeyCode)
import Color exposing (..)
import Graphics.Collage exposing (circle, filled, move, collage, gradient, rect)
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
            let bs = updateBallsPhysics dt model.balls
                (ps, bs') = updateBallsWithPlayers model.players bs
            in
                { model
                    | balls = bs'
                    , players = List.map (updatePlayer dt bs') ps
                }

        Control (t, keys) ->
            { model
                | players = List.map (controlPlayer t keys) model.players
            }

view : Model -> Element
view model =
    let (w', h') = model.dims
        (w, h) = (toFloat w', toFloat h')
        balls = List.map Ball.view model.balls
        players = List.map Player.view <| List.map snd model.players
        background =
            rect w h
                |> gradient (backgroundGradient (-w/2, h/2))
        forms = background :: List.append players balls
    in
        forms
            |> collage w' h'

backgroundGradient : (Float, Float) -> Gradient
backgroundGradient pos =
    radial pos 10.0 pos 400.0
        [ (0, white)
        , (1, lightOrange)
        ]

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

updateBallsPhysics : Float -> List Ball.Model -> List Ball.Model
updateBallsPhysics dt =
    List.map (Ball.update <| Ball.Tick dt)

updateBallsWithPlayers : List ControledPlayer -> List Ball.Model -> (List ControledPlayer, List Ball.Model)
updateBallsWithPlayers players balls =
    case players of
        [] -> ([], balls)

        (p::ps) ->
            let (p', bs) = updateBallsWithPlayer p balls
                (ps', bs') = updateBallsWithPlayers ps bs
            in
                (p' :: ps', bs')

updateBallsWithPlayer : ControledPlayer -> List Ball.Model -> (ControledPlayer, List Ball.Model)
updateBallsWithPlayer (ctrls, player) balls =
    let (projs, newBalls) = ballCollisions player.projectiles balls
        newPlayer = { player | projectiles = projs }
    in
        ((ctrls, newPlayer), newBalls)
