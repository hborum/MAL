namespace itu.dk.MAL

module Monads =

  type Result<'T,'TError> =
      | Ok of ResultValue:'T
      | Error of ErrorValue:'TError

  type ResultBuilder () =
    member _this.Bind(x : Result<'A,'B>, f : 'A -> Result<'C, 'B>) : Result<'C, 'B> =
      match x with
      | Ok a -> f a
      | Error e -> Error e
    member _this.ReturnFrom x = x
    member _this.Return(x) = Result.Ok x

  type AnnotatedResult<'anno, 'res> = AnnoResult of ('anno -> ('res * 'anno))

  module AnnoResult =
    let inline run state x = let (AnnoResult(f)) = x in f state
    let get = AnnoResult(fun annos -> annos, annos)
    let ok res = AnnoResult(fun annos -> res, annos)
    let add anno = AnnoResult(fun annos -> ((), anno::annos))
    let addF f = AnnoResult(fun annos -> ((), f annos))
    let addAnnoWhenFalse f b  =
      if not b
      then addF f
      else AnnoResult(fun annos -> ((), annos))
    let map f annoRes = AnnoResult(fun (annos: 'anno list) ->
      let x, state = run annos annoRes
      f x, state)

  type AnnoResultBuilder() =
    member this.Zero () = AnnoResult(fun s -> (), s)
    member this.Return x = AnnoResult(fun s -> x, s)
    member inline this.ReturnFrom (x: AnnotatedResult<'s, 'a>) = x

    member this.Bind (x, f) : AnnotatedResult<'s, 'b> =
      AnnoResult(fun state ->
        let (result: 'a), state = AnnoResult.run state x
        AnnoResult.run state (f result))

  let result = ResultBuilder ()
  let annoResult = AnnoResultBuilder ()

  // Todo monadic pipe

  // General Utility
  let rec mapM f list =
    // loop through the list
    match list with
    | [] ->
      // if empty, lift [] to a Result
      result { return [] }
    | head::tail ->
      result {
        let! h = f head
        let! t = mapM f tail
        return (h :: t)
      }

  // General Utility
  let rec mapMA f list =
    // loop through the list
    match list with
    | [] ->
      // if empty, lift [] to a Result
      annoResult { return [] }
    | head::tail ->
      annoResult {
        let! h = f head
        let! t = mapMA f tail
        return (h :: t)
      }

  let rec iterMA f list =
    // loop through the list
    match list with
    | [] -> AnnoResult.ok ()
    | head::tail ->
      annoResult {
        let! h = f head
        return! iterMA f tail
      }

  let rec iterM f list =
    // loop through the list
    match list with
    | [] ->
      // if empty, lift [] to a Result
      result { return ()}
    | head::tail ->
      result {
        let! h = f head
        return! iterM f tail
      }

  let rec foldM f init list =
    match list with
    | [] ->
      result { return init }
    | head::tail ->
      result {
        let! h = f init head
        let! t = foldM f h tail
        return t
      }

  let rec filterM f list =
    foldM (fun acc v -> result {
      let! b = f v
      return if b then v::acc else acc}) [] list

  let rec foldMA f init list =
    match list with
    | [] ->
      annoResult { return init }
    | head::tail ->
      annoResult {
        let! h = f init head
        let! t = foldMA f h tail
        return t
      }

  let rec filterMA f list =
    foldMA (fun acc v -> annoResult {
      let! b = f v
      return if b then v::acc else acc}) [] list

  type OrderedMap<'k,'v> = ('k * 'v) list
  type MonadFunc = interface end
  type RSWMonad<'ReadData, 'State, 'WriteData, 'Result> =
    'ReadData -> 'State -> 'Result * 'State * 'WriteData seq

  let bindRSW (gen : RSWMonad<'R,'S,'W,'T>) (func : 'T -> RSWMonad<'R,'S,'W,'U>) : RSWMonad<'R,'S,'W,'U> =
      fun env s ->
      let (aile, state1, stmts) = gen env s
      let (aile', state2, stmts') = func aile env state1
      in (aile', state2, Seq.append stmts' stmts)

  let retRSW aile : RSWMonad<'R, 'S,'W,'T> = fun _ s -> (aile, s, Seq.empty)

  let fmap (func : 'T -> 'U) (gen : RSWMonad<'R,'S,'W,'T>) =
    fun env s -> let (a, s', ss) = gen env s in (func a, s', ss)

  let emit stmts : RSWMonad<'R,'S,'W,unit> = fun _ s ->  ((), s, stmts)

  type RSWBuilder() =
      member _.Bind(comp, func) = bindRSW comp func
      member _.Return(value) = retRSW value
      member _.ReturnFrom(comp) = comp
  ///<summary>
  ///Creates a new RSWMonad by removing the write-data from 'generator', and adding it to the result.
  ///</summary>
  let capture (generator : RSWMonad<'R,'S,'W,'T>) : RSWMonad<'R,'S,'W,'T * 'W seq> =
      fun env s -> let (a, s', ss) = generator env s in ((a, ss), s', Seq.empty)

  let ILBuilder = new RSWBuilder()


  let mapRSW (func : 'T -> RSWMonad<'R, 'S,'W,'U>) (seq: 'T seq) : RSWMonad<'R, 'S,'W, 'U seq> =
    Seq.foldBack (
      fun t rws ->
        ILBuilder {
          let! a = func t
          let! rest = rws
          return Seq.append (Seq.singleton a) rest
        }) seq (retRSW Seq.empty)

  let mapRSW_ (func : 'T -> RSWMonad<'R,'S,'W, 'U>) (seq: 'T seq) : RSWMonad<'R,'S,'W, unit> =
      fun env s -> let (_, s', ss) = mapRSW func seq env s in ((), s', ss)

  let updateMany (updaters : RSWMonad<'R,'S,'W,'R> seq) : RSWMonad<'R,'S,'W,'R> =
      let combine c1 c2 = bindRSW c1 (fun env -> (fun _ -> c2 env))
      Seq.reduce combine updaters

  let ask : ('a -> 'b -> 'a * 'b * 'c seq) = fun env s -> (env, s, Seq.empty)
  let local f comp = fun env s -> comp (f env) s
  let rec foldRWS f init list =
    match list with
    | [] ->
      ILBuilder { return init }
    | head::tail ->
      ILBuilder {
        let! h = f init head
        let! t = foldRWS f h tail
        return t
      }
  let getState = fun _ s -> (s, s, Seq.empty)
  let addState k v = fun _ s -> ((), (k, v)::s, Seq.empty)
  let putState s = fun _ _ -> ((), s, Seq.empty)
  let modifyState f = fun _ s -> ((), f s, Seq.empty)

  type RSMonad<'ReadData, 'StateData, 'Result> = 'ReadData -> 'StateData -> 'Result * 'StateData
  let bindRS comp func = fun read state -> let (result, state') = comp read state in func result read state'
  let retRS value = fun read state -> (value, state)

  type RSBuilder() =
    member _.Bind(comp, func) = bindRS comp func
    member _.Return(value) = retRS value
    member _.ReturnFrom(comp) = comp

  let compileBuilder = new RSBuilder()

  let askRS = fun env s -> (env, s)
  let rec foldRS f init list =
    match list with
    | [] ->
      compileBuilder { return init }
    | head::tail ->
      compileBuilder {
        let! h = f init head
        let! t = foldRS f h tail
        return t
      }
  let getStateRS = fun _ s -> (s, s)
  let addStateRS k v = fun _ s -> ((), (k, v)::s)
  let putStateRS s = fun _ _ -> ((), s)
  let modifyStateRS f = fun _ s -> ((), f s)

  let mapRS (func : 'T -> RSMonad<'R, 'S,'U>) (seq: List<'T>) : RSMonad<'R, 'S, List<'U>> =
    List.foldBack (fun t rws -> compileBuilder { let! a = func t in let! rest = rws in return a::rest}) seq (retRS [])

  let mapRS_ (func : 'T -> RSMonad<'R,'S,'U>) (seq: List<'T>) : RSMonad<'R,'S,unit> =
      fun env s -> let (_, s') = mapRS func seq env s in ((), s')


  type MaybeBuilder() =

    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some a -> f a

    member this.ReturnFrom x = x

    member this.Return(x) =
        Some x


