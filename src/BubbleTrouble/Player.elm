module BubbleTrouble.Player where

import Color exposing (Color, Gradient, complement, linear, white, toHsl, hsla)
import Graphics.Collage exposing (Form, polygon, gradient, move, group, dashed, outlined, circle)
import Math.Vector2 exposing (Vec2, vec2, add, scale, sub, getX, toTuple, fromTuple, getY, normalize)
import Time exposing (Time)

import Utils exposing (lum, transparency)
import BubbleTrouble.Projectile as Projectile

import Debug

type alias Model =
    { pos : Vec2
    , speed : Float
    , color : Color
    , dir : Direction
    , lastShot : Time
    , projectiles : List Projectile.Model
    , status : Status
    , dims : (Int, Int)
    }

type Direction  = Left
                | Right
                | Stop

type Status = Alive
            | Dead

type Action = Go Direction
            | Shoot Time
            | Tick Float
            | Hit

shape : List (Float, Float)
shape = [(-20, 0), (0, 40), (20, 0)]

newPlayer : Color -> (Int, Int) -> Model
newPlayer color dims =
    let y = (\x -> -x/2) <| toFloat <| snd dims
    in
        Model (vec2 0 y) 160 color Stop 0 [] Alive dims

update : Action -> Model -> Model
update action player =
    case action of
        Go dir' ->
            if player.status == Alive then
                { player | dir = Debug.watch "Direction" dir' }
            else
                { player | dir = Stop }

        Shoot t ->
            let pos = add player.pos <| vec2 0 40
                newProjs =
                    if player.projectiles == [] then
                        Projectile.simpleProjectile pos player.color player.dims :: player.projectiles
                    else
                        player.projectiles
            in
                if player.status == Alive then
                    { player
                        | lastShot = t
                        , projectiles = newProjs
                    }
                else
                    player

        Tick dt ->
            let (_, h) = player.dims
                projs =
                    player.projectiles
                        |> List.map (Projectile.update (Projectile.Tick dt))
                        |> List.filter ((\x -> x < toFloat h/2) << getY << .pos)
                newPos =
                    player.dir
                        |> toVec
                        |> scale (dt * player.speed)
                        |> add player.pos
                        |> inBounds player.dims
            in
                { player
                    | pos = newPos
                    , projectiles = projs
                }

        Hit ->
            { player
                | status = Dead
                , dir = Stop
            }

view : Model -> Form
view player =
    let playerView =
            polygon shape
                |> gradient (getGradient player)
                |> move (Debug.watch "Position" <| toTuple player.pos)
        projViews = List.map (Projectile.view) player.projectiles
    in
        group <| projViews ++ [playerView]

toVec : Direction -> Vec2
toVec dir =
    case dir of
        Left  -> vec2 -1 0
        Right -> vec2  1 0
        Stop  -> vec2  0 0

inBounds : (Int, Int) -> Vec2 -> Vec2
inBounds (w', _) pos =
    let (x, y) = toTuple pos
        w = toFloat w'
    in
        if x < -w/2 then
            vec2 (-w/2) y
        else if x > w/2 then
            vec2 (w/2)  y
        else
            pos

getGradient : Model -> Gradient
getGradient player =
    let color =
            if player.status == Alive then
                player.color
            else
                transparency 0.2 player.color
        (w, h) = player.dims
        topLeft = vec2 (-(toFloat w)/2) (toFloat h / 2)
        offset = getX << normalize <| sub topLeft player.pos
        (r, theta) = toPolar (offset * 8, -20)
        p1 = (\(x, y) -> (x, y+40)) <| fromPolar (r, theta + 0.6)
        p2 = (\(x, y) -> (x, y+40)) <| fromPolar (r, theta - 0.6)
    in
        linear p1 p2
            [ (0.0, color)
            , (0.1, color)
            , (0.3, lum 1.5 color)
            , (0.5, lum 5.0 color)
            , (0.8, lum 1.5 color)
            , (1.0, color)
            ]

getSegments : Model -> List (Vec2, Vec2)
getSegments player =
    let vecs = List.map (add player.pos << fromTuple) shape
    in
        List.map2 (,) vecs <| List.drop 1 vecs ++ List.take 1 vecs
