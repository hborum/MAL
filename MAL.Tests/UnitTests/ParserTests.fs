module ParserTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open itu.dk.MAL

let assumeEquivalent prog1 prog2 = List.forall ((=) true) <| List.map2 ASTUtil.decEquivalent prog1 prog2

let typeIncomplete source =
  let prog = MALInterface.MAL ("", source)
  ignore <| Array.iter (printf "%A") prog.ParseErrors
  prog.ParseErrors.Length > 0

[<TestClass>]
type LanguageConstructs () =
  [<TestMethod>]
  member __.mapOnDict () =
    let prog =
      "
      action init()
      with
        let dict = createMap([1;2;3],[\"a\";\"b\";\"c\"])
        let test =
          map (k,v) in dict
          with
            k
          end
      end

      action manage()
      with
      end"
    Assert.IsFalse <| typeIncomplete prog

  [<TestMethod>]
  member __.pair () =
    let prog =
      "
      action init()
      with
        update x in Policies
        with
          let g = {1,2}
          x.Reserve = 2
        end
      end

      action manage()
      with
      end"
    Assert.IsFalse <| typeIncomplete prog

  [<TestMethod>]
  member __.userList () =
    let prog =
      "
      action init()
      with
        update x in Policies
        with
          let g = [1;2;3]
          x.Reserve = 2
        end
      end

      action manage()
      with
      end"
    Assert.IsFalse <| typeIncomplete prog

  [<TestMethod>]
  member __.listOfPairs () =
    let prog =
      "
      action init()
      with
        update x in Policies
        with
          let g = [{1,2};{2,3}]
          x.Reserve = 2
        end
      end

      action manage()
      with
      end"
    Assert.IsFalse <| typeIncomplete prog

[<TestClass>]
type IncompletePrograms () =


  [<TestMethod>]
  member __.trailingList () =
    let prog =
      "
      action init()
      with
        update x in Policies
        with
          let g = [1; 2;]
          x.Reserve = 2
        end
      end

      action manage()
      with
      end"
    Assert.IsTrue <| typeIncomplete prog

  [<TestMethod>]
  member __.halfPair () =
    let prog =
      "
      action init()
      with
        update x in Policies
        with
          let g = {2.0,}
          x.Reserve = 2
        end
      end

      action manage()
      with
      end"
    Assert.IsTrue <| typeIncomplete prog

  [<TestMethod>]
  member __.extend () =
    let prog =
      "
    extend Group
      Grove Float
    end

    exnd Policy
    Grove2 : Float
    end


    extend Policy
    Grove2 : Float
    end

    action init()
    wth
      update x in Policies
        x.Reserve = 2
      end
    end

    action manage()
    with
      let g = g ()
    end

    fun g () =
      2 * false
    "
    Assert.IsTrue(typeIncomplete prog)

  [<TestMethod>]
  member __.initManage () =
    let prog =
      "
    acton init()
    with
      update x in Policies
        x.Reserve = 2
      end
    end

    action manage()
    with
      let g = g ()
    ed

    fun g () =
      2 * false
    "
    Assert.IsTrue(typeIncomplete prog)

  [<TestMethod>]
  member __.funSyntax () =
    let prog =
      "
    int
      update x in Policies
        x.Reserve = 2
      end
    end

    fun g =
      2 * false
    "
    Assert.IsTrue(typeIncomplete prog)

  [<TestMethod>]
  member __.transferSyntax () =
    let prog =
      "
    action init()
    with
      update x in Policies
        x.Reserve -> 2 -q> x.Reserve
        x.Reserve <q- 2
      end
    end

    "
    Assert.IsTrue(typeIncomplete prog)

  [<TestMethod>]
  member __.doErr1 () =
    let prog =
      "
    actsdon init()
    with
      update x in Policies
        x.Reserve = 2
      end
    end

    action manage()
    with
      do a
    ed

    fun g () =
      2 * false
    "
    Assert.IsTrue(typeIncomplete prog)

  [<TestMethod>]
  member __.doErr2 () =
    let prog =
      "
    action init()
    with
      updaste x in Policies
        x.Reserve = 2
      end
    end

    action manage()
    with
      do a(
    ed

    fun g () =
      2 * false
    "
    Assert.IsTrue(typeIncomplete prog)


  [<TestMethod>]
  member __.dataErr () =
    let prog =
      "
      data Foo
        fie : Float
      end

    action init()
    with
      update x in Policies
        x.Reserve = 2
      end
    end

    action manage()
    with
      do a(
    ed

    fun g () =
      2 * false
    "
    Assert.IsTrue(typeIncomplete prog)

  [<TestMethod>]
  member __.actionErr () =
    let prog =
      "
      data Foo
        fie : Float
      end

    action init()
    with
      update x in Policies
        x.Reserve = 2
      end
    end

    action manage()
    with
      do g()
    end

    action g () =
      2 * false
    "
    Assert.IsTrue(typeIncomplete prog)

  [<TestMethod>]
  member __.actionErr2 () =
    let prog =
      "
      data Foo
        fie : Float
      end

    action init()
    with
      update x in Policies
        x.Reserve = 2
      end
    end

    action manage()
    with
      do g()
    end

    action g ()
    wth
      2 * false
    end
    "
    Assert.IsTrue(typeIncomplete prog)

  [<TestMethod>]
  member __.actionErr3 () =
    let prog =
      "
      data Foo
        fie : Float
      end

    action init()
    with
      update x in Policies
        x.Reserve = 2
      end
    end

    action manage()
    with
      do g()
    end

    act g ()
    with
      2 * false
    "
    Assert.IsTrue(typeIncomplete prog)

  [<TestMethod>]
  member __.dataBlock () =
    "
    data Expenses
    	CashFlowExpense :
    	, FreePolicyFee = value
    	, SurrenderFee = value
    	, PolicyFee = value
    	, ReserveExpense = value
    end

    data PolicyInput extends Input
    	CalculationEndTime : Float
    end


    data Group extends BaseEntity
    	Assets   : List<Asset>
    	Policies : List<Policy>
    end

    action init()
    with
    end

    action manage()
    with
    end"
    |> typeIncomplete
    |> Assert.IsTrue

  [<TestMethod>]
  member __.noneSomeErr () =
    let prog =
      "
    action manage()
    with
      let b = None :
      let b = None Float
      let c = Some
      let d = Some 3
    end
    "
    Assert.IsTrue(typeIncomplete prog)

  [<TestMethod>]
  member __.dataError () =
    let prog =
      "
      data Policy
      Foo : Float
      Pol
      Bar : Bar
      end

      action init()
      with
      end

      action manage()
      with
      end
    "
    Assert.IsTrue(typeIncomplete prog)