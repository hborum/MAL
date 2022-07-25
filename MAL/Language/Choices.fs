namespace itu.dk.MAL

module Choices =

  let choiceMap f g = function
    | Choice1Of2 c1 -> Choice1Of2 <| f c1
    | Choice2Of2 c2 -> Choice2Of2 <| g c2

  let joinChoice f g = function
    | Choice1Of2 c1 -> f c1
    | Choice2Of2 c2 -> g c2

  let combineChoice c1 c2 =
    match (c1,c2) with
    | Choice1Of2 v1, Choice1Of2 v1' -> Choice1Of2 (v1,v1')
    | Choice2Of2 v2, Choice2Of2 v2' -> Choice2Of2 (v2,v2')
    | _ -> failwith "Cannot combine different choices"

  let split list =
    let parter = function Choice1Of2 _ -> true | _ -> false
    let (ones, twos) = List.partition parter list
    let unwrap1, unwrap2 = (function Choice1Of2 c -> c | _ -> failwith ""), (function Choice2Of2 c -> c | _ -> failwith "")
    List.map unwrap1 ones, List.map unwrap2 twos