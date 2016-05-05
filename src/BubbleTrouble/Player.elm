module BubbleTrouble.Player where

import Color exposing (Color, Gradient, complement, linear, white, toHsl, hsla)
import Graphics.Collage exposing (Form, polygon, gradient, move, group, dashed, outlined, circle)
import Math.Vector2 exposing (Vec2, vec2, add, scale, toTuple, getY)
import Time exposing (Time)

import Utils exposing (lum)
import BubbleTrouble.Projectile as Projectile

import Debug

type alias Model =
    { pos : Vec2
    , speed : Float
    , color : Color
    , dir : Direction
    , lastShot : Time
    , projectiles : List Projectile.Model
    , dims : (Int, Int)
    }

type Direction  = Left
                | Right
                | Stop

type Action = Go Direction
            | Shoot Time
            | Tick Float

newPlayer : Color -> (Int, Int) -> Model
newPlayer color dims =
    let y = (\x -> -x/2) <| toFloat <| snd dims
    in
        Model (vec2 0 y) 160 color Stop 0 [] dims

update : Action -> Model -> Model
update action player =
    case action of
        Go dir' ->
            { player | dir = Debug.watch "Direction" dir' }

        Shoot t ->
            let pos = add player.pos <| vec2 0 40
                newProjs =
                    if player.projectiles == [] then
                        Projectile.simpleProjectile pos player.color player.dims :: player.projectiles
                    else
                        player.projectiles
            in
                { player
                    | lastShot = t
                    , projectiles = newProjs
                }

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

view : Model -> Form
view player =
    let playerView =
            polygon [(-20, 0), (0, 40), (20, 0)]
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
    linear (0, 0) (-20, 5)
        [ (0.0, player.color)
        , (0.1, player.color)
        , (0.3, lum 1.5 player.color)
        , (0.5, white)
        , (0.8, lum 1.5 player.color)
        , (1.0, player.color)
        ]
