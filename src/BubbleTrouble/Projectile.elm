module BubbleTrouble.Projectile where

import Color exposing (Color, Gradient, complement, linear, white, toHsl, hsla)
import Graphics.Collage exposing (Form, segment, dashed, traced)
import Math.Vector2 exposing (Vec2, vec2, add, scale, toTuple)

import Debug

type alias Model =
    { kind : Kind
    , pos : Vec2
    , speed : Vec2
    , color : Color
    , dims : (Int, Int)
    }

type Kind = Simple


simpleProjectile : Vec2 -> Color -> (Int, Int) -> Model
simpleProjectile pos =
    Model Simple pos (vec2 0 150)


type Action = Tick Float

update : Action -> Model -> Model
update action proj =
    case action of
        Tick dt ->
            { proj | pos = add proj.pos <| scale dt proj.speed }

view : Model -> Form
view proj =
    let (x, y) = Debug.watch "Projectile" <| toTuple proj.pos
        b = -(toFloat <| snd proj.dims)/2
    in
        segment (x, y) (x, b)
            |> traced (dashed proj.color)
