module Axis3d.Extra exposing (intersectionAxisAlignedBoundingBox3d)

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d
import Point3d
import Quantity exposing (Quantity(..))
import Vector3d


intersectionAxisAlignedBoundingBox3d : Axis3d units coordinates -> BoundingBox3d units coordinates -> Maybe (Axis3d units coordinates)
intersectionAxisAlignedBoundingBox3d ray boundingBox =
    let
        extrema =
            BoundingBox3d.extrema boundingBox

        (Quantity minX) =
            extrema.minX

        (Quantity minY) =
            extrema.minY

        (Quantity minZ) =
            extrema.minZ

        (Quantity maxX) =
            extrema.maxX

        (Quantity maxY) =
            extrema.maxY

        (Quantity maxZ) =
            extrema.maxZ

        delta =
            Vector3d.withLength (Quantity 1000) (Axis3d.direction ray)

        --
        ray2 =
            ray
                |> Axis3d.originPoint
                |> Point3d.unwrap

        delta2 =
            delta
                |> Vector3d.unwrap

        t1 =
            (minX - ray2.x) / delta2.x

        t2 =
            (maxX - ray2.x) / delta2.x

        txFar =
            max t1 t2

        t3 =
            (minY - ray2.y) / delta2.y

        t4 =
            (maxY - ray2.y) / delta2.y

        tyFar =
            max t3 t4

        t5 =
            (minZ - ray2.z) / delta2.z

        t6 =
            (maxZ - ray2.z) / delta2.z

        tzFar =
            max t5 t6

        tmax =
            min (min txFar tyFar) tzFar
    in
    if tmax < 0 then
        Nothing

    else
        let
            txNear =
                min t1 t2

            tyNear =
                min t3 t4

            tzNear =
                min t5 t6

            tmin =
                max (max txNear tyNear) tzNear
        in
        if tmin > tmax then
            Nothing

        else
            let
                normal =
                    if txNear > tyNear then
                        if tzNear > txNear then
                            if ray2.z > (maxZ + minZ) / 2 then
                                Direction3d.positiveZ

                            else
                                Direction3d.negativeZ

                        else if ray2.x > (maxX + minX) / 2 then
                            Direction3d.positiveX

                        else
                            Direction3d.negativeX

                    else if tzNear > tyNear then
                        if ray2.z > (maxZ + minZ) / 2 then
                            Direction3d.positiveZ

                        else
                            Direction3d.negativeZ

                    else if ray2.y > (maxY + minY) / 2 then
                        Direction3d.positiveY

                    else
                        Direction3d.negativeY
            in
            if tmin < 0 then
                if tmax < 1 then
                    Just
                        (Axis3d.through (Point3d.translateBy (Vector3d.scaleBy tmax delta) (Axis3d.originPoint ray)) normal)

                else
                    Nothing

            else if tmin < 1 then
                Just
                    (Axis3d.through (Point3d.translateBy (Vector3d.scaleBy tmin delta) (Axis3d.originPoint ray)) normal)

            else
                Nothing
