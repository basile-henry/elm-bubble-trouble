module BubbleTrouble.Collisions where

import Math.Vector2 exposing (Vec2, vec2, add, sub, dot, scale, length, normalize, distance)

import BubbleTrouble.Ball as Ball
import BubbleTrouble.Player as Player
import BubbleTrouble.Projectile as Projectile

ballCollisions : List Projectile.Model -> List Ball.Model -> (List Projectile.Model, List Ball.Model)
ballCollisions projs balls =
    case balls of
        [] ->
            (projs, [])

        (b::bs) ->
            case Maybe.oneOf <| List.indexedMap (colliders b) projs of
                Just (i, p) ->
                    let ps = List.take i projs ++ List.drop (i + 1) projs
                        (ps', bs') = ballCollisions ps bs
                    in
                        (ps', Ball.getChilds b ++ bs')

                Nothing ->
                    let (ps, bs') = ballCollisions projs bs
                    in
                        (ps, b :: bs')


colliders : Ball.Model -> Int -> Projectile.Model -> Maybe (Int, Projectile.Model)
colliders b i p =
    if distPointSegment b.pos (Projectile.getSegment p) < b.radius then
        Just (i, p)
    else
        Nothing

    --let collides ball =
    --        List.any (\seg -> distPointSegment ball.pos seg < ball.radius) segs
    --    collision ball =
    --        case getCollider ball projs of
    --            Just p ->

    --        if collides ball then
    --            Ball.getChilds ball
    --        else
    --            [ball]
    --in
    --    List.concatMap collision

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

--collide : Ball.Model -> List Projectile.Model -> (Bool, List Projectile.Model)
--collide ball projs =
--    List.map (\p ->
--            if distPointSegment ball.pos (Projectile.getSegments p) < ball.radius then
--                Just p
--            else
--                getCollider ball ps

--        [] ->
            

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
