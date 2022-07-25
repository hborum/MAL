module TyperTests

open itu.dk.MAL
open AST
open AnalyserTypes
open Microsoft.VisualStudio.TestTools.UnitTesting

let typerTestErr_withData (source : System.String) err (dataDef) : bool =
  let prog = MALInterface.MAL ("", dataDef + source)
  if (Array.length prog.ParseErrors) > 0
  then Array.iter
         (fun pA -> printf "%s" pA.message
         ) prog.ParseErrors
       false
  else
    let errs = prog.Analyser.Errors
    let success =
      List.exists (
        fun ((err'),_) ->
          match err' with
          | NoInfo s
          | InternalError s
          | BadArgs s
          | VarDoesNotExist s
          | DoesNotContain s
          | OnlyDoAction s
          | NotRecord s
          | ConditionalNotBool s
          | BranchesNotTheSame s
          | NotSubTypeOf s
          | WrongArity s
          | NotCallable s
          | UncoveredBranch s
          | NotEnumarable s
          | NotSame s
          | Warning s
          | InvalidType s
          | DuplicateFunction s
          | RecursiveFunction s
          | BadInheritance s
          | ModuleError s
          | InitError s
          | UsageBeforeInit s
          | ElementsNotTheSame s   -> err' = err s
        ) errs
    if success
    then
      true
    else
      printfn "Error in %s" source
      printfn "%A\nDoes not contain %A" errs err
      false

let typerTestErr (program : System.String) err : bool =
  typerTestErr_withData program err TestDataDef.dataDefinition

let typerTestGood (program : string) : bool =
  let prog = MALInterface.MAL ("", TestDataDef.dataDefinition + program)
  if (Array.length prog.ParseErrors) > 0
  then Array.iter
         (fun pA -> printf "%s" pA.message
         ) prog.ParseErrors
       false
  else
    let errs = prog.Analyser.Errors
    if List.length errs > 0
    then
      printfn "Following unexpected errors:"
      List.iter (fun (err, (posBegin,_)) -> prog.PrintErr err (posBegin)) errs
      false
    else true

[<TestClass>]
type TypeErrors () =
  [<TestMethod>]
  member __.binOp () =
    let prog =
      "action init()
      with
      Global.TestReserve = 2 + false
      end"
    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.badDictMap () =
    let prog =
      "
      action init()
      with
        let dict = createMap([1;2;3],[\"a\";\"b\";\"c\"])
        let test =
          map (k,v) in dict
          with
            k + v
          end
      end

      action manage()
      with
      end"
    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.singleOnDict () =
    let prog =
      "
      action init()
      with
        let dict = createMap([1;2;3],[\"a\";\"b\";\"c\"])
        let test =
          map kvp in dict
          with
            2
          end
      end

      action manage()
      with
      end"
    let err = NotEnumarable
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.kvpOnList () =
    let prog =
      "
      action init()
      with
        let list = [1;2;3]
        let test =
          map (k,v) in list
          with
            k + v
          end
      end

      action manage()
      with
      end"
    let err = NotEnumarable
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.mixedList () =
    let prog =
      "
      action init()
      with
        update x in Policies
        with
          let g = [1; {2.0, 0}; 3]
        end
      end

      action manage()
      with
      end"
    let err = ElementsNotTheSame
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.varUnbound () =
    let prog =
      "action init()
      with
      Global.TestReserve = x
      end"
    let err = VarDoesNotExist
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.varUnbound2 () =
    let prog =
      "action init()
      with
        update grp in Groups
        with
          let x = 5
          grp.Reserve = 1
        end
        Global.TestReserve = x
      end"

    let err = (VarDoesNotExist)
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.varUnbound3 () =
    let prog =
      "action init()
      with
       Global.TestReserve = Qwop.TestReserve
      end
      "
    let err = VarDoesNotExist
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.filterNonSub () =
    let prog =
      "
      action init()
      with
        update grp in Groups:ActualExpense
        with
          grp.Reserve = 2
        end
      end"
    let err = NotSubTypeOf
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.conditionalNotBool () =
    let prog =
      "action init()
      with
         update grp in Groups
         with
          grp.Reserve = if 2 then 3 else 4
        end
      end"
    let err = ConditionalNotBool
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.branchErr () =
    let prog =
      "action init()
      with
        update grp in Groups
        with
          grp.Reserve = if true then 3 else false
        end
      end"
    let err = BranchesNotTheSame
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.matchOnNonRec () =
    let prog =
      "action init()
      with
       Global.TestReserve =
        match 1 with
        | A a -> 2
        end
      end"
    let err = NotRecord
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.uncoveredBranch2 () =
    let prog =
      "
      action init()
      with
        let A =
          map grp in Groups
          with
            match grp with
            | Interest a -> 2
            | Expense a -> 2
            end
          end
      end"
    let err = UncoveredBranch
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.extraCoveredBranch () =
    let prog =
      "action init()
      with
        let A =
          map grp in Groups
          with
            match grp with
            | Interest a -> 2
            | Expense a -> 2
            | Risk a -> 2
            | MarketInterestRate mi -> 3
            | KExpense a -> 2
            end
          end
      end"
    let err = UncoveredBranch
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.branchErr2 () =
    let prog =
      "action init()
      with
        let A =
          map grp in Groups
          with
            match grp with
            | Interest a -> 2
            | Expense a -> 2
            | Risk a -> false
            end
          end
      end"

    let err = BranchesNotTheSame
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.wrongArity () =
    let prog =
      "action init()
      with
      Global.TestReserve = f()
      end

      fun f(a : Bool, b: Float) = 2
      "

    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.wrongArity2 () =
    let prog =
      "action init()
      with
      Global.TestReserve = f(2,3)
      end

      fun f() = 2
      "
    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.wrongArg () =
    let prog =
      "action init()
      with
      let mapM = createMap(Groups, Groups)
      let a = mapM(Global)
      Global.TestReserve = 2
      end
      "

    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.wrongArg2 () =
    let prog =
      "action init()
      with
      let times = map g in Groups with g.Reserve end
      let results = map g in Groups with g.Reserve * 2 end
      let tFun = createPiecewiseConstant(times, results)
      Global.TestReserve = tFun(false)
      end
      "
    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.mapOnNonList () =
    let prog =
      "action init()
      with
      Global.TestReserve = sum(map x in 2 with 3 end)
      end
      "
    let err = NotEnumarable
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.condNotBool2 () =
    let prog =
      "action init()
      with
      Global.TestReserve = sum(map x in Groups where 2 with 3 end)
      end
      "
    let err = ConditionalNotBool
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.condNotBool3 () =
    let prog =
      "action init()
      with
      update pol in Policies
      where 2
      with
        Global.TestReserve = 3
      end
      end
      "
    let err = ConditionalNotBool
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.wrongAssign () =
    let prog =
      "
      data Policy
        Reserve : Float
      end

      action init()
      with
          update pol in Policies
           with
            pol.Reserve = false
          end
       end
      "
    let err = NotSame
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.wrongAssign2 () =
    let prog =
      "data Global
       tfun : IFunction
      end

      action init()
      with
        Global.tfun = f
      end

      fun f(d : Float) = 2
      "
    let err = NotSame
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.badArgsBuiltin () =
    let prog =
      "
      action init()
      with
        let a = unique(2,3)
      end
      "
    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.badArgsBuiltin2 () =
    let prog =
      "
      action init()
      with
        let a = unique(2)
      end
      "

    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.badArgsBuiltin3 () =
    let prog =
      "
      action init()
      with
        let a = unique()
      end
      "
    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.invalidType () =
    let prog =
      "
      action manage()
      with
        let x = 2
      end

      fun f(cf: CashFlo) =
      match cf with
      | ActualExpense a -> 2
      | WithExpenses a -> 2
      | WithoutExpenses a -> 2
      end"
    let err = InvalidType
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.uncoveredBranch1 () =
    let prog =
      "action init()
      with
        update group in Groups:{Interest}
        with
          let c =
            match group with
            | Interest i -> f(i)
            | Risk r -> 2
            end
        end
      end
      "
    let err = UncoveredBranch
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.emptyFilter () =
    let prog =
      "action init()
      with
      update group in Groups:{}
      with
        let c =
          match group with
          | Interest i -> f(i)
          | Risk r -> 2
          end
        end
      end
      "
    let err = UncoveredBranch // Todo would we like not filter on empty?
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.newData () =
    let prog =
      "
      data Foo
        Bar : Float
      end

      action init()
      with
        let b = new Foo{ ASD = 3 }
      end
      "
    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.newData2 () =
    let prog =
      "
      data Foo
        Bar : Float
      end

      action init()
      with
        let b = new Foo{ Bar = 2 , ASD = 3}
      end
      "
    let err = DoesNotContain
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.newData3 () =
    let prog =
      "
      data Foo
        Bar : Float
      end

      action init()
      with
        let b = new FFoo{ Bar = 3 }
      end
      "
    let err = InvalidType
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.badArgs () =
    let prog =
      "action init()
      with
      update group in Groups:{Interest, Risk}
      with
        let c = f(group)
        let c = j(group)
      end
      end

      fun f(i : {Interest, Expense}) =
        3

      fun j(i : {Risk, Expense}) =
        3

      "
    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.duplicateFunction () =
    let prog =
      "action init()
      with
        let a = 2
      end

      fun f(i : {Interest, Expense}) =
        3

      fun f(i : {Risk, Expense}) =
        3

      "

    let err = DuplicateFunction
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.recursiveError () =
    let prog =
      "action init()
      with
        let a = 2
      end

      fun f(i : int) =
        f(2)
      "
    let err = RecursiveFunction
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.recursiveError2 () =
    let prog =
      "action init()
      with
        let a = 2
      end

      fun f(i : int) =
        j(2)

      fun j(i : int) =
        f(2)
    "
    let err = RecursiveFunction
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.doFunction () =
    let prog =
      "
      fun setCf(f : Float) =
        3

      action init()
      with
        do setCf(5)
      end
    "
    let err = OnlyDoAction
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.doRecursion1 () =
    let prog =
      "
      action a(f : Float)
      with
        do a(f)
      end

      action init()
      with
        do a(2)
      end
    "
    let err = RecursiveFunction
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.doRecursion2 () =
    let prog =
      "
      action a(f : Float)
      with
        do a(f)
      end

      action b(f : Float)
      with
        do b(f)
      end

      action init()
      with
        do a(2)
      end
    "
    let err = RecursiveFunction
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.doWrongArity1 () =
    let prog =
      "
      action setCf(f : Float)
      with
        update cf in CashFlows:{WithExpenses, WithoutExpenses}
        with
          let a = 2
        end
      end

      action init()
      with
        do setCf()
      end
    "
    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.doWrongArity2 () =
    let prog =
      "
      action setCf(f : Float)
      with
        update cf in CashFlows:{WithExpenses, WithoutExpenses}
        with
          let a = 2
        end
      end

      action init()
      with
        do setCf(2, 3)
      end
    "
    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.unusedDeclaration () =
    let prog =
      "
      action a (g : Group)
      with
        // both input and result is accessed
	      let a = g.Input.Reserve
	      let b = g.Result.AccumulatedAtEnd
      end

      action init()
      with
      end

      action manage()
      with
      end
    "
    let err = DoesNotContain
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.notInMode () =
    let prog =
      "
      action a (g : Group)
      with
	        let b = g.Result.AccumulatedAtEnd
      end

      action init()
      with
        update g in Groups
        with
          do a(g)
        end
      end

      action manage()
      with
      end
    "
    let err = DoesNotContain
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.map () =
    let prog =
      "
          data Group
            Reserve : Float
          end

          action init()
          with
            let b =
              map g1, g2 in Groups, Groups
              with
                g1.Reserve + false
              end
          end

          action manage()
          with
          end
        "
    let err = BadArgs
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.badInhName () =
    let prog =
      "
          data Foo extends Bar
          end
        "
    let err = BadInheritance
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.circularInh1 () =
    let prog =
      "
          data Foo extends Bar
          end

          data Bar extends Foo
          end
        "
    let err = BadInheritance
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.circularInh2 () =
    let prog =
      "
          data Foo extends Baz
          end

          data Bar extends Foo
          end

          data Baz extends Bar
          end
        "
    let err = BadInheritance
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.sameDataTwice () =
    let prog =
      "
          data Foo
          end

          data Foo
          end
        "
    let err = BadInheritance
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
  member __.overrideNonWithSub () =
    let prog =
      "
          data Foo
            Reserve : Float
          end

          data Bar extends Foo
            Reserve : Bool
          end
        "
    let err = BadInheritance
    Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
    member __.assOption () =
      let prog =
        "
        action init()
        with
          Global.maybe = 2
        end

        action manage()
        with
        end
      "
      let err = NotSame
      Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
    member __.readOption () =
      let prog =
        "
        action init()
        with
          let a = 2 + Global.maybe
        end

        action manage()
        with
        end
      "
      let err = BadArgs
      Assert.IsTrue(typerTestErr prog err)

  [<TestMethod>]
    member __.missingField () =
      let prog =
        "
        action init()
        with
          let a = 2
        end

        action manage()
        with
        end
      "
      let err = BadInheritance
      Assert.IsTrue(typerTestErr_withData prog err TestDataDef.dataDefinitionMissing1)

[<TestClass>]
type TypeSuccess () =

  [<TestMethod>]
  member __.list () =
    let prog =
      "
       action manage()
       with
           let y = [1;2;3]
       end"
    Assert.IsTrue(typerTestGood prog)

  [<TestMethod>]
  member __.pair () =
    let prog =
      "
       action manage()
       with
           let y = {1,\"a\"}
       end"
    Assert.IsTrue(typerTestGood prog)

  [<TestMethod>]
  member __.prog1 () =
    let prog =
      "
       action manage()
       with
           let y = 5
           let x = y
       end"
    Assert.IsTrue(typerTestGood prog)

  [<TestMethod>]
  member __.prog2 () =
    let prog =
      "action init()
      with
        let A =
          map grp in Groups
          with
          let a =
            match grp with
            | ReserveGroup _ -> 2
            | MarketRateInterest mriGrp -> 2
            end
            in a
            end
          end
      end"
    Assert.IsTrue(typerTestGood prog)

  [<TestMethod>]
  member __.prog3 () =
    let prog =
      "
      action init()
      with
        let mapM = createMap(map g in Groups:ReserveGroup with {g,g} end)
        update group in Groups:Interest
        with
          let a = mapM(group)
          group.Reserve <| a.Reserve
        end
      end
      "
    Assert.IsTrue(typerTestGood prog)

  [<TestMethod>]
  member __.prog4 () =
    let prog =
      "action init()
      with
        update group in Groups:{Interest, Risk}
        with
        group.Reserve <|
          match group with
          | Interest i -> 2
          | Risk r -> 2
          end
        end
      end
      "
    Assert.IsTrue(typerTestGood prog)

  [<TestMethod>]
  member __.prog5 () =
    let prog =
      "action init()
      with
        update group in Groups:{Interest, Risk}
        with
          let a = f(j(j(group)))
          let b = g(group)
          let c =
            match group with
            | Interest i -> f(i)
            | Risk r -> 2
            end
        end
      end
      fun f(g : {Interest, Risk}) =
        3

      fun j(g : {Interest, Risk}) =
        g

      fun h(g : {Risk, Interest}) =
        3

      fun g(h : Group) =
        3
      "
    Assert.IsTrue(typerTestGood prog)

  [<TestMethod>]
  member __.prog7 () =
    let prog =
      "action init()
      with
      Global.TestReserve = sum(map group in Groups where false with 3 end)
      end
      "
    Assert.IsTrue(typerTestGood prog)

  [<TestMethod>]
  member __.prog9 () =
    let prog =
      "action manage()
      with
      let x = f(5)
      end
      fun f (f : Float) = f "
    Assert.IsTrue(typerTestGood prog)

  [<TestMethod>]
  member __.doer () =
    let prog =
      "
      action setGroup(group : ReserveGroup, f : Float)
      with
        group.Reserve <| f
      end

      action setPolicies()
      with
        update p in Policies
        with
          update g in p.Groups:ReserveGroup
          with
            do setGroup(g, p.Reserve)
          end
        end
      end

      action init()
      with
        update g in Groups:ReserveGroup
        with
          do setGroup(g, 3)
        end
        do setPolicies()
      end
    "
    Assert.IsTrue(typerTestGood prog)


  [<TestMethod>]
  // Todo fix this
  member __.unusedDeclaration () =
    let prog =
      "
      fun f (g : ReserveGroup) =
	      g.Reserve

      action a (g : ReserveGroup)
      with
	      let a = 2
      end

      action init()
      with
      end

      action manage()
      with
      end
    "
    Assert.IsTrue(typerTestGood prog)

  [<TestMethod>]
  member __.map () =
    let prog =
      "
      action init()
      with
        let a =
          map g in Groups
          with
            2
          end
        let b =
          map g1, g2 in Groups:ReserveGroup, Groups:ReserveGroup
          with
            g1.Reserve + g2.Reserve
          end
      end

      action manage()
      with
      end
    "
    Assert.IsTrue(typerTestGood prog)

  [<TestMethod>]
  member __.inheritsPropogates () =
    let prog =
      "
      action init()
      with
        let a =
          map g in Groups:Expense
          with
            g.Reserve + 2
          end
      end

      action manage()
      with
      end
    "
    Assert.IsTrue(typerTestGood prog)

   [<TestMethod>]
    member __.assOption () =
      let prog =
        "
        action init()
        with
          Global.maybe = None : Float
          Global.maybe = Some 2
          let a =
            match Global.maybe with
            | None -> 3
            | Some v -> v + 2
            end
        end

        action manage()
        with
        end
      "
      Assert.IsTrue(typerTestGood prog)
