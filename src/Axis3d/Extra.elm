module Axis3d.Extra exposing (intersectionAxisAlignedBoundingBox3d)

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d exposing (Direction3d)
import Length
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Rectangle3d
import SketchPlane3d
import Vector3d exposing (Vector3d)


intersectionAxisAlignedBoundingBox3d : Axis3d units coordinates -> BoundingBox3d units coordinates -> Maybe (Axis3d units coordinates)
intersectionAxisAlignedBoundingBox3d ray boundingBox =
    let
        boxCenter =
            BoundingBox3d.centerPoint boundingBox

        extrema =
            BoundingBox3d.extrema boundingBox

        ( dimensionX, dimensionY, dimensionZ ) =
            BoundingBox3d.dimensions boundingBox

        positiveZRect =
            Rectangle3d.centeredOn
                (SketchPlane3d.through
                    (Point3d.translateIn Direction3d.positiveZ
                        (Quantity.half dimensionZ)
                        boxCenter
                    )
                    Direction3d.positiveZ
                )
                ( dimensionX, dimensionY )

        negativeZRect =
            Rectangle3d.centeredOn
                (SketchPlane3d.through
                    (Point3d.translateIn Direction3d.negativeZ
                        (Quantity.half dimensionZ)
                        boxCenter
                    )
                    Direction3d.negativeZ
                )
                ( dimensionX, dimensionY )

        positiveXRect =
            Rectangle3d.centeredOn
                (SketchPlane3d.through
                    (Point3d.translateIn Direction3d.positiveX
                        (Quantity.half dimensionX)
                        boxCenter
                    )
                    Direction3d.positiveX
                )
                ( dimensionZ, dimensionY )

        negativeXRect =
            Rectangle3d.centeredOn
                (SketchPlane3d.through
                    (Point3d.translateIn Direction3d.negativeX
                        (Quantity.half dimensionX)
                        boxCenter
                    )
                    Direction3d.negativeX
                )
                ( dimensionZ, dimensionY )

        positiveYRect =
            Rectangle3d.centeredOn
                (SketchPlane3d.through
                    (Point3d.translateIn Direction3d.positiveY
                        (Quantity.half dimensionY)
                        boxCenter
                    )
                    Direction3d.positiveY
                )
                ( dimensionZ, dimensionX )

        negativeYRect =
            Rectangle3d.centeredOn
                (SketchPlane3d.through
                    (Point3d.translateIn Direction3d.negativeY
                        (Quantity.half dimensionY)
                        boxCenter
                    )
                    Direction3d.negativeY
                )
                ( dimensionZ, dimensionX )

        rayOrigin =
            Axis3d.originPoint ray
    in
    List.foldl
        (\( rect, normalDir ) maybeIntersection ->
            case Axis3d.intersectionWithRectangle rect ray of
                Nothing ->
                    maybeIntersection

                Just intersectionPoint ->
                    let
                        newDist =
                            Point3d.distanceFrom rayOrigin (Rectangle3d.centerPoint rect)
                    in
                    case maybeIntersection of
                        Nothing ->
                            Just ( intersectionPoint, normalDir, newDist )

                        Just ( _, _, dist ) ->
                            if newDist |> Quantity.lessThan dist then
                                Just ( intersectionPoint, normalDir, newDist )

                            else
                                maybeIntersection
        )
        Nothing
        [ ( positiveZRect, Direction3d.positiveZ )
        , ( negativeZRect, Direction3d.negativeZ )
        , ( positiveXRect, Direction3d.positiveX )
        , ( negativeXRect, Direction3d.negativeX )
        , ( positiveYRect, Direction3d.positiveY )
        , ( negativeYRect, Direction3d.negativeY )
        ]
        |> Maybe.map (\( intersectionPoint, normalDir, _ ) -> Axis3d.through intersectionPoint normalDir)
