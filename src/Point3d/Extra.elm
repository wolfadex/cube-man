module Point3d.Extra exposing (constrain)

import Point3d exposing (Point3d)


constrain : { min : Point3d units coordinates, max : Point3d units coordinates } -> Point3d units coordinates -> Point3d units coordinates
constrain bounds point =
    let
        unwrapped =
            Point3d.unwrap point

        lowerUnwrapped =
            Point3d.unwrap bounds.min

        upperUnwrapped =
            Point3d.unwrap bounds.max
    in
    Point3d.unsafe
        { x = max lowerUnwrapped.x (min upperUnwrapped.x unwrapped.x)
        , y = max lowerUnwrapped.y (min upperUnwrapped.y unwrapped.y)
        , z = max lowerUnwrapped.z (min upperUnwrapped.z unwrapped.z)
        }
