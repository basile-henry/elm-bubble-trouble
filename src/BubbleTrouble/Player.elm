module BubbleTrouble.Player where

import Color exposing (Color, Gradient, complement, linear, white, toHsl, hsla)
import Graphics.Collage exposing (Form, polygon, gradient, move)
import Math.Vector2 exposing (Vec2, vec2, add, scale, toTuple)
import Time exposing (Time)

import Debug

type alias Model =
    { pos : Vec2
    , speed : Float
    , color : Color
    , dir : Direction
    , lastShot : Time
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
        Model (vec2 0 y) 160 color Stop 0 dims

update : Action -> Model -> Model
update action player =
    case action of
        Go dir' ->
            { player | dir = Debug.watch "Direction" dir' }

        Shoot t ->
            { player
                | lastShot = t
                , color = complement player.color
            }

        Tick dt ->
            { player
                | pos =
                    player.dir
                        |> toVec
                        |> scale (dt * player.speed)
                        |> add player.pos
                        |> inBounds player.dims
            }

view : Model -> Form
view player =
    polygon [(-20, 0), (0, 40), (20, 0)]
        |> gradient (getGradient player)
        |> move (Debug.watch "Position" <| toTuple player.pos)

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

lum : Float -> Color -> Color
lum w c =
    let {hue, saturation, lightness, alpha} = toHsl c
    in
        hsla hue saturation (w * lightness) alpha

