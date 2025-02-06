module Axis3d.Extra exposing (intersectionAxisAlignedBoundingBox3d)

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d exposing (Direction3d)
import Length
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Vector3d exposing (Vector3d)


intersectionAxisAlignedBoundingBox3d : Axis3d units coordinates -> BoundingBox3d units coordinates -> Maybe { t : Float, intersection : Point3d units coordinates, normal : Direction3d coordinates }
intersectionAxisAlignedBoundingBox3d rayXX boundingBox =
    let
        ray : Point3d units coordinates
        ray =
            Axis3d.originPoint rayXX

        delta : Vector3d units coordinates
        delta =
            Axis3d.direction rayXX
                |> Vector3d.withLength (Quantity 1)

        extrema =
            BoundingBox3d.extrema boundingBox

        boxMin =
            Point3d.xyz extrema.minX extrema.minY extrema.minZ

        boxMax =
            Point3d.xyz extrema.maxX extrema.maxY extrema.maxZ

        boxMin2 =
            Point3d.unwrap boxMin

        boxMax2 =
            Point3d.unwrap boxMax

        ray2 =
            Point3d.unwrap ray

        delta2 =
            Vector3d.unwrap delta

        t1 =
            (boxMin2.x - ray2.x) / delta2.x

        t2 =
            (boxMax2.x - ray2.x) / delta2.x

        txNear =
            min t1 t2

        txFar =
            max t1 t2

        t3 =
            (boxMin2.y - ray2.y) / delta2.y

        t4 =
            (boxMax2.y - ray2.y) / delta2.y

        tyNear =
            min t3 t4

        tyFar =
            max t3 t4

        t5 =
            (boxMin2.z - ray2.z) / delta2.z

        t6 =
            (boxMax2.z - ray2.z) / delta2.z

        tzNear =
            min t5 t6

        tzFar =
            max t5 t6

        tmin =
            max (max txNear tyNear) tzNear

        tmax =
            min (min txFar tyFar) tzFar

        normal =
            if txNear > tyNear then
                if tzNear > txNear then
                    if ray2.z > (boxMax2.z + boxMin2.z) / 2 then
                        Direction3d.positiveZ

                    else
                        Direction3d.negativeZ

                else if ray2.x > (boxMax2.x + boxMin2.x) / 2 then
                    Direction3d.positiveX

                else
                    Direction3d.negativeX

            else if tzNear > tyNear then
                if ray2.z > (boxMax2.z + boxMin2.z) / 2 then
                    Direction3d.positiveZ

                else
                    Direction3d.negativeZ

            else if ray2.y > (boxMax2.y + boxMin2.y) / 2 then
                Direction3d.positiveY

            else
                Direction3d.negativeY
    in
    if tmax < 0 then
        Nothing

    else if tmin > tmax then
        Nothing

    else if tmin < 0 then
        if tmax < 1 then
            Just
                { intersection = Point3d.translateBy (Vector3d.scaleBy tmax delta) ray
                , normal = normal
                , t = tmax
                }

        else
            Nothing

    else if tmin < 1 then
        Just
            { intersection = Point3d.translateBy (Vector3d.scaleBy tmin delta) ray
            , normal = normal
            , t = tmin
            }

    else
        Nothing
