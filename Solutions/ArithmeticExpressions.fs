module Solutions.ArithmeticExpressions

open Operators.Types
open Operators

let operators = Functions.Union.listAll<Operator> ()

let chooseList l = function
    | Error -> None
    | Value v -> Some (l, v)

let solvePairs (first: list<list<float> * float>) (second: list<float>) =
    List.allPairs first second
    |> List.filter (fun ((l, _), b) -> not (List.contains b l) )
    |> List.map (fun ((l, a), b) ->
        operators
        |> List.map (fun o -> b::l, Calculate.single a b o)
    )
    |> List.concat
    |> List.choose (fun (l, v) -> chooseList l v)

let chooseInt (_, x) =
    match System.Int32.TryParse (x.ToString()) with
    | (true, v) when v > 0 -> Some v
    | _ -> None

let solveDigits (digits: list<float>) =
    let pairValues = solvePairs (digits |> List.map (fun x -> [x], x)) digits
    let triples = solvePairs pairValues digits
    solvePairs triples digits
    |> List.choose chooseInt
    |> List.sort
    |> List.distinct

let chooseContinuous (i, x) =
    if i + 1 = x then Some x else None

let longestStretch (digits: list<float>) =
    solveDigits digits
    |> List.mapi (fun i x -> i, x)
    |> List.choose chooseContinuous
    |> List.length

let longest () =
    let leadingPairs = List.allPairs [1.0..6.0] [2.0..7.0] |> List.filter (fun (a, b) -> a <> b)
    let endingPairs = List.allPairs [3.0..8.0] [4.0..9.0] |> List.filter (fun (a, b) -> a <> b)

    List.allPairs leadingPairs endingPairs
    |> List.map (fun ((a, b), (c, d)) -> [a;b;c;d] |> List.sort |> List.distinct)
    |> List.filter (fun l -> l.Length = 4)
    |> List.distinct
    |> List.map (fun digits -> digits, longestStretch digits)
    |> List.maxBy (fun (_, x) -> x)
