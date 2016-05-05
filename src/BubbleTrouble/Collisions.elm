module BubbleTrouble.Collisions where

import Math.Vector2 exposing (Vec2, vec2, add, sub, dot, scale, length, normalize, distance)
import BubbleTrouble.Ball as Ball
import BubbleTrouble.Player as Player
import BubbleTrouble.Projectile as Projectile

ballCollisions : List Projectile.Model -> List Ball.Model -> List Ball.Model
ballCollisions projs =
    let segs = List.map Projectile.getSegment projs
        collides ball =
            List.any (\seg -> distPointSegment ball.pos seg < ball.radius) segs
    in
        List.filter (not << collides)

playerCollision : List Ball.Model -> Player.Model -> Player.Model
playerCollision balls player =
    let collides =
            List.any
                (\b ->
                    List.any (\seg -> distPointSegment b.pos seg < b.radius)
                        <| Player.getSegments player)
                balls
    in
        if collides then
            Player.update Player.Hit player
        else
            player

distPointSegment : Vec2 -> (Vec2, Vec2) -> Float
distPointSegment p (a, b) =
    let v = sub b a
        vn = normalize v
        dc = dot vn <| sub p a
        dv = length v
        c =
            if dc > dv then
                b
            else if dc < 0 then
                a
            else
                add a <| scale dc vn
    in
        distance p c
