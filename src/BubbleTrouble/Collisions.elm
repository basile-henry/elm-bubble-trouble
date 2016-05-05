module BubbleTrouble.Collisions where

import Math.Vector2 exposing (Vec2, vec2, add, sub, dot, scale, norm, normalize, distance)
import BubbleTrouble.Ball as Ball
import BubbleTrouble.Player as Player
import BubbleTrouble.Projectile as Projectile

ballCollisions : List Projectile.Model -> List Ball.Model -> List Ball.Model
ballCollisions projs =
    let ps = List.map Projectile.getSegment projs
        collides ball =
            List.any (\p -> distPointSegment ball.pos (fst p) (snd p)) ps
    in
        List.filter (not << collides)

playerCollisions : List Ball.Model -> List Player.Model -> List Player.Model


distPointSegment : Vec2 -> Vec2 -> Vec2 -> Float
distPointSegment p a b =
    let v = sub b a
        vn = normalize v
        dc = dot vn <| sub p a
        dv = norm v
        c =
            if dc > dv then
                b
            else if dc < 0 then
                a
            else
                add a <| scale dc vn
    in
        distance p c
