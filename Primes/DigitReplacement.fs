module Primes.DigitReplacement

let baseSets = [
    Set.empty.Add(0)
    Set.empty.Add(1)
]

let primeLimit (length: int) =
    int (10.0 ** (float length / 2.0 ))

let nextIndexSet (newIndex: int) (current: list<Set<int>>) =
    if newIndex < 1 then
        []
    elif newIndex = 1 then
        baseSets
    else
        Set.empty.Add(newIndex)::
        (current |> List.map (fun s -> s.Add(newIndex))) @
        current

let applyReplacements (indexes: Set<int>) (candidate: list<char>) (value: char) =
    candidate
    |> List.mapi (fun i c -> if indexes.Contains i then value else c)
    |> System.String.Concat
    |> int

let getReplacements (indexes: Set<int>) (lastIndex: int) =
    if indexes.Contains lastIndex then
        ['1';'3';'5';'7';'9']
    elif indexes.Contains 0 then
        ['1'..'9']
    else
        ['0'..'9']

let rec countReplacementPrimes
    (indexes: Set<int>)
    (replacements: list<char>)
    (candidate: list<char>)
    (matches: list<int>)
    (primes: list<int>)
    =
    match replacements with
    | [] -> primes, matches
    | x::xs ->
        let toCheck = applyReplacements indexes candidate x
        let newCount =
            if Check.isPrime primes toCheck then toCheck::matches else matches
        countReplacementPrimes
            indexes
            xs
            candidate
            newCount
            primes

let rec countAllIndexes
    (indexSets: list<Set<int>>)
    (candidate: list<char>)
    (maxMatches: list<int>)
    (primes: list<int>)
    =
    match indexSets with
    | [] -> maxMatches
    | indexes::tail ->
        let replacements = getReplacements indexes (candidate.Length - 1)
        let (newCache, matches) = countReplacementPrimes indexes replacements candidate [] primes
        let newCount = if matches.Length > maxMatches.Length then matches else maxMatches
        countAllIndexes tail candidate newCount newCache

let rec checkNextCount
    (target: int)
    (length: int)
    (number: int)
    (currentIndexes: list<Set<int>>)
    (primes: list<int>)
    =
    let candidate = number.ToString() |> Seq.toList
    let indexes, newPrimes =
        if candidate.Length > length then
            let nextLimit = primeLimit candidate.Length
            let ps = Generate.nextPrimesTo nextLimit primes
            let indexSet = nextIndexSet length currentIndexes
            indexSet, ps
        else
            currentIndexes, primes

    if Check.isPrime newPrimes number then
        let matches = countAllIndexes indexes candidate [] newPrimes

        if matches.Length = target then
            matches
        else
            checkNextCount target candidate.Length (number + 2) indexes newPrimes
    else
        checkNextCount target candidate.Length (number + 2) indexes newPrimes

let matchCount (target: int) =
    let indexes = nextIndexSet 1 []
    checkNextCount target 2 11 indexes [7; 5; 3; 2]
    |> List.min
