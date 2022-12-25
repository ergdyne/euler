module Operators.Calculate

open Types

let single (a: float) (b: float) = function
    | Add -> Value (a + b)
    | Subtract -> Value (a - b)
    | Multiply -> Value (a * b)
    | Divide when b <> 0 -> Value (a / b)
    | ReverseDivide when a <> 0 -> Value (b / a)
    | ReverseSubtract -> Value (b - a)
    | _ -> Error
