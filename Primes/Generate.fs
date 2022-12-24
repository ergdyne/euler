module Primes.Generate


let isPrime (primes: list<int>) (candidate: int) =
    let cutoff = (float candidate ** 0.5) |> int

    let check state prime =
        if state = false then
            false
        elif candidate % prime = 0 then
            false
        else
            true

    primes
    |> List.filter (fun x -> x <= cutoff)
    |> List.fold check true

let rec nHelper (n: int) (candidate: int) (primes: list<int>) =
    if primes.Length = n then
        primes
    elif isPrime primes candidate then
        nHelper n (candidate + 2) (candidate::primes)
    else
        nHelper n (candidate + 2) primes

let nDescending(n: int) =
    if n < 1 then
        []
    else
        nHelper n 3 [2]

let n (n:int) =
    nDescending n |> List.rev
