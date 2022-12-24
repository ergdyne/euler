module Primes.Generate

let rec nextPrime (candidate: int) (primes: list<int>) =
    if Check.isPrime primes candidate then
        candidate
    else
        nextPrime (candidate + 2) primes

let rec nHelper (n: int) (candidate: int) (primes: list<int>) =
    if primes.Length = n then
        primes
    else
        let x = nextPrime (candidate) primes
        nHelper n (x + 2) (x::primes)

let rec nextPrimesTo (target: int) (primes: list<int>) =
    if primes.Head > target then
        primes
    else
        nextPrimesTo target ((nextPrime (primes.Head + 2) primes)::primes)

let nextNPrimesDescending (n: int) (primes: list<int>) =
    nHelper (n + primes.Length) primes.Head primes

let nPrimesDescending(n: int) =
    if n < 1 then
        []
    else
        nHelper n 3 [2]

let nPrimes (n:int) =
    nPrimesDescending n |> List.rev
