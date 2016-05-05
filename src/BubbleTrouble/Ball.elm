module BubbleTrouble.Ball where

import Color exposing (Color, Gradient, radial, toHsl, hsla)
import Graphics.Collage exposing (Form, circle, gradient, move)
import Math.Vector2 exposing (Vec2, vec2, add, sub, normalize, scale, toTuple)

import Utils exposing (lum)

import Debug

type alias Model =
    { pos : Vec2
    , speed : Vec2
    , radius : Float
    , gravity : Float
    , color : Color
    , dims : (Int, Int)
    }

type Action = Tick Float

update : Action -> Model -> Model
update action ball =
    case action of
        Tick dt ->
            let pos' = add ball.pos <| scale dt ball.speed
                speed' = case bounce pos' ball of
                    Nothing -> add ball.speed <| scale dt <| scale ball.gravity <| vec2 0 1
                    Just s  -> s
            in
                { ball | pos = pos', speed = speed' }

view : Model -> Form
view ball =
    circle ball.radius
        |> gradient (getGradient ball)
        |> move (toTuple ball.pos)

bounce : Vec2 -> Model -> Maybe Vec2
bounce pos ball =
    let (x, y) = toTuple pos
        (dx, dy) = toTuple ball.speed
        (w', h') = ball.dims
        (w, h) = (toFloat w', toFloat h')
        r = ball.radius
    in
        if x-r < -w/2 then
            if y-r < -h/2 then
                Just <| vec2 (abs dx) (abs dy)
            else if y+r > h/2 then
                Just <| vec2 (abs dx) (negate (abs dy))
            else
                Just <| vec2 (abs dx) (dy)
        else if x+r > w/2 then
            if y-r < -h/2 then
                Just <| vec2 (negate (abs dx)) (abs dy)
            else if y+r > h/2 then
                Just <| vec2 (negate (abs dx)) (negate (abs dy))
            else
                Just <| vec2 (negate (abs dx)) (dy)
        else
            if y-r < -h/2 then
                Just <| vec2 (dx) (abs dy)
            else if y+r > h/2 then
                Just <| vec2 (dx) (negate (abs dy))
            else
                Nothing

getGradient : Model -> Gradient
getGradient ball =
    let ratio = 0.7
        outer = (0, 0)
        (w, h) = ball.dims
        topLeft = vec2 (-(toFloat w)/2) (toFloat h / 2)
        inner =
            ball.pos
                |> sub topLeft
                |> normalize
                |> scale (ratio * ball.radius)
                |> toTuple
        innerR = ball.radius * (1 - ratio) / 3
        stops =
            [ (0,   lum 1.5 ball.color)
            , (0.4, ball.color)
            , (0.8, ball.color)
            , (1.0, lum 0.5 ball.color)
            ]
    in
        radial inner innerR outer ball.radius stops

getSegment 
