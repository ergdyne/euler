module Operators.Types

type Operator =
    | Add
    | Subtract
    | Multiply
    | Divide
    | ReverseDivide
    | ReverseSubtract

type Number =
    | Value of float
    | Error
