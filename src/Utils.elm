module Utils where

import Color exposing (..)

lum : Float -> Color -> Color
lum w c =
    let {hue, saturation, lightness, alpha} = toHsl c
    in
        hsla hue saturation (w * lightness) alpha

type alias ColorInternal =
    { red : Int, green : Int, blue : Int, alpha : Float }

fromRgba : ColorInternal -> Color
fromRgba { red, green, blue, alpha } = rgba red green blue alpha

transparency : Float -> Color -> Color
transparency a c =
    let ci = toRgb c
    in
        fromRgba { ci | alpha = a }