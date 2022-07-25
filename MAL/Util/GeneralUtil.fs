namespace itu.dk.MAL

module GeneralUtil =

  // List
  ///<summary>
  /// Forces list into head and tail with no warnings.
  /// To be used places where a list must be non-empty and we want to avoid
  /// a compiler warning
  ///</summary>
  let headTail xs = List.head xs, List.tail xs

  let mapJoin (map1 : Map<'a, 'b>) (map2 : Map<'a, 'b>) =
    Map.ofSeq <| Seq.append (Map.toSeq map1) (Map.toSeq map2)
  let SeqCons a b = seq {yield a; yield! b}
  let SeqUnZip (s : (('a * 'b) seq)) : ('a seq * 'b seq) =
    Seq.foldBack (fun (a,b) (ass,bss) ->
        seq {yield a; yield! ass}
      , seq {yield b; yield! bss}
    ) s (Seq.empty, Seq.empty)


  let toMap dictionary =
    (dictionary :> seq<_>)
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

  let rec mapUntil f xs =
    match xs with
    | [] -> (false, [])
    | x::xs' ->
      let stop, x' = f x
      if stop
      then (true, x'::xs')
      else
        let early, xs' = mapUntil f xs'
        (early, x'::xs')

  let takeTail ls i = List.rev <| (List.take i <| List.rev ls)

  let rec intFold f state i =
    match i with
    | 0 -> state
    | n ->
      if n < 0
      then failwith "Can only fold on positives"
      else intFold f (f state n) (n-1)

  let rec intFoldBack f state i =
    match i with
    | 0 -> state
    | n ->
      if n < 0
      then failwith "Can only fold on positives"
      else f (intFold f state (n-1)) n

  type Counter (startCount : int) =
    let mutable iCount = startCount
    member __.inc () =
      iCount <- iCount + 1
      iCount
    member __.dec () =
      iCount <- iCount - 1
      iCount
    member __.count () = iCount

  let rec powerset =
    function
    | [] -> [[]]
    | (x::xs) ->
      let xss = powerset xs
      List.map (fun xs' -> x::xs') xss @ xss

  let mapConcatAdd (key : 'a) (value : 'b) (map : Map<'a, 'b list>) : Map<'a, 'b list>  =
    match Map.tryFind key map with
    | None -> Map.add key [value] map
    | Some vs -> Map.add key (value :: vs) map

  let mapFindOrEmpty (key : 'a) (map : Map<'a, 'b list>) =
    match Map.tryFind key map with
    | None -> []
    | Some vs -> vs

  let mapChoose (f : 'a -> 'b -> 'c option) (map : Map<'a, 'b>) : Map<'a, 'c> =
    Map.fold
      ( fun acc key v ->
        match f key v with
        | None -> acc
        | Some v' -> acc.Add(key,v')
      ) Map.empty map

  let rec findAndRemove xs f =
    match xs with
    | [] -> None, []
    | x :: xs ->
      if f x
      then
        Some x , xs
      else
        let (res, filt) = findAndRemove xs f
        (res, x :: filt)

  let fst3 (f,_,_) = f
  let snd3 (_,s,_) = s
  let trd3 (_,_,t) = t

