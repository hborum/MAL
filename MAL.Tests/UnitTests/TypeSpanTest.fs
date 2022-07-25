module TypeSpanTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open Gen
let rand = new System.Random(0)

let swap (a: _[]) x y =
    let tmp = a.[x]
    a.[x] <- a.[y]
    a.[y] <- tmp

// shuffle an array (in-place)
let shuffle a =
    Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a

let lions = [0;1]
let wolves = [2;3]
let herbiousvores = [4;5]
let allAnimals = lions @ wolves @ herbiousvores

let testAnimals =
  let lions_obj =
    List.map
      (fun i ->
        let l = new Lion<obj>()
        l.Id <- i
        l :> obj
      ) lions
  let wolves_obj =
    List.map
      (fun i ->
        let l = new Wolf<obj>()
        l.Id <- i
        l :> obj
      ) wolves
  let herbiousvores_obj =
    List.map
      (fun i ->
        let l = new Herbiousvore<obj>()
        l.Id <- i
        l :> obj
      ) herbiousvores
  List.toArray <|  lions_obj
                 @ wolves_obj
                 @ herbiousvores_obj

let rawTypeSpan () =
  let types = [| typeof<Lion<obj>>; typeof<Wolf<obj>> ; typeof<Herbiousvore<obj>> |]
  shuffle testAnimals
  new TypeSpan<Animal<obj>>(testAnimals, types)

let simpleFilterTest<'T when 'T :> Animal<obj> and 'T : not struct> animals =
  let mutable ids = []
  let filtered = rawTypeSpan().Filter<'T>()
  let enum = filtered.GetEnumerator()
  Assert.IsTrue(List.length animals = filtered.Length)
  while enum.MoveNext() do
    printf "%d" enum.Current.Id
    ids <- enum.Current.Id :: ids
  Assert.IsTrue(List.forall (fun i -> List.contains i ids) animals)
  Assert.IsTrue(List.length animals = List.length ids)

[<TestClass>]
type TypeSpanTest () =
  [<TestMethod>]
  member __.EnumAll () =
    let mutable ids = []
    let enum = rawTypeSpan().GetEnumerator()
    while enum.MoveNext() do
      ids <- enum.Current.Id :: ids
    Assert.IsTrue(List.forall (fun i -> List.contains i ids) allAnimals)

  [<TestMethod>]
  member __.FilterLions () =
    simpleFilterTest<Lion<obj>> lions

  [<TestMethod>]
  member __.FilterWolves () =
    simpleFilterTest<Wolf<obj>> wolves

  [<TestMethod>]
  member __.FilterHerbivoures () =
    simpleFilterTest<Herbiousvore<obj>> herbiousvores

  [<TestMethod>]
  member __.FilterCarnivore () =
    simpleFilterTest<Carnivore<obj>> <| lions @ wolves

  [<TestMethod>]
  member __.FilterWolfLion () =
    let mutable ids = []
    let enum = rawTypeSpan().Filter<Wolf_Lion<obj>>().GetEnumerator()
    while enum.MoveNext() do
      ids <- () :: ids
    Assert.IsTrue(List.length (lions @ wolves) = List.length ids)

  [<TestMethod>]
  member __.FilterNothing () =
    let mutable ids = []
    let enum = rawTypeSpan().Filter<Wolf_Lion<Wolf<obj>>>().GetEnumerator()
    while enum.MoveNext() do
      ids <- () :: ids
    Assert.IsTrue(0 = List.length ids)


  [<TestMethod>]
  member __.DoubleFilterFilterWolfLion () =
    let mutable ids = []
    let enum = rawTypeSpan().Filter<Carnivore<obj>>().Filter<Lion<obj>>().GetEnumerator()
    while enum.MoveNext() do
      ids <- enum.Current.Id :: ids
    Assert.IsTrue(List.forall (fun i -> List.contains i ids) lions)
    Assert.IsTrue(List.length lions = List.length ids)
