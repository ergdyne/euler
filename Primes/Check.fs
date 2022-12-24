module Primes.Check

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
