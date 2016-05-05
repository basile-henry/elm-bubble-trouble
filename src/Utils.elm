module Utils where

import Color exposing (..)

lum : Float -> Color -> Color
lum w c =
    let {hue, saturation, lightness, alpha} = toHsl c
    in
        hsla hue saturation (w * lightness) alpha