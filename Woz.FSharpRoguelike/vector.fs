module Vector

type public vector = 
    {
        x: int
        y: int
    } 
with
    static member op_Addition (lhs, rhs) = 
        {x = lhs.x + rhs.x; y = lhs.y + rhs.y}

    static member op_Subtraction (lhs, rhs) = 
        {x = lhs.x - rhs.x; y = lhs.y - rhs.y}

    static member op_Multiply (vector: vector, scale: int) = 
        {x = vector.x * scale; y = vector.y * scale}

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
    {x = abs location.x; y = abs location.y}

let distanceFrom target location =
    let diff = abs (target - location)
    let sqr x = x * x
    let distanceSq = (sqr diff.x) + (sqr diff.y)
    sqrt (float distanceSq)
