module Vector

type public vector = 
    {
        x: int
        y: int
    } 
with
    static member create x y = {x = x; y = y}

    static member op_Addition (lhs, rhs) = 
        vector.create (lhs.x + rhs.x) (lhs.y + rhs.y)

    static member op_Subtraction (lhs, rhs) = 
        vector.create (lhs.x - rhs.x) (lhs.y - rhs.y)

    static member op_Multiply (lhs: vector, scale: int) = 
        vector.create (lhs.x * scale) (lhs.y * scale)

    static member op_LessThanOrEqual (lhs, rhs) = 
        lhs.x <= rhs.x && lhs.y <= rhs.y

    static member op_Equals (lhs, rhs) = 
        lhs.x = rhs.x && lhs.y = rhs.y

    static member op_LessThan (lhs, rhs) = 
        lhs.x < rhs.x && lhs.y < rhs.y

    static member op_GreaterThanOrEqual (lhs, rhs) = 
        lhs.x >= rhs.x && lhs.y >= rhs.y

    static member op_GreaterThan (lhs, rhs) = 
        lhs.x > rhs.x && lhs.y > rhs.y

let abs location =
    vector.create (abs location.x) (abs location.y)

let distanceFrom target location =
    let diff = abs (target - location)
    let sqr x = x * x
    let distanceSq = (sqr diff.x) + (sqr diff.y)
    sqrt (float distanceSq)

module Directions =
    let idle = vector.create 0 0
    let north = vector.create 0 1
    let northEast = vector.create 1 1
    let east = vector.create 1 0
    let southEast = vector.create 1 (-1)
    let south = vector.create 0 -1
    let southWest = vector.create (-1) (-1)
    let west = vector.create (-1) 0
    let northWest = vector.create -1 1

