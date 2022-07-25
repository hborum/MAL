namespace itu.dk.MAL

module Printer =

  open AST
  open System

  let rep i = String.replicate (i*2) " "

  let ppTag (ex: subTypeMap) (tag : pureTag) =
    if ex.ContainsKey tag
    then tag
    else
      match Map.tryPick (fun k v -> if List.exists ((=) tag) v then Some k else None) ex with
      | Some sup -> sprintf "%s:%s" sup tag
      | None -> failwith <| sprintf "Impossible tag type while printing %s" tag

  let printTagSyntax tags =
    match tags with
    | tag::[] -> tag
    | _ -> sprintf "{%s}" <| String.Join(",", tags)

  let rec printTyp typ =
    match typ with
    | TErr            -> "TERR"
    | TMissing _      -> "MISSING"
    | TDouble  _      -> "Float"
    | TBool    _      -> "Bool"
    | TReserve    _   -> "Reserve"
    | TStr     _      -> "String"
    | TModule (s, _)     -> sprintf "Module %s" s
    | TGeneric (_, GenericList, ts)         -> sprintf "List<%s>" <| String.Join(",", (List.map (printTyp << snd) ts))
    | TGeneric (_, GenericOption, ts)       -> sprintf "Option<%s>" <| String.Join(",", (List.map (printTyp << snd) ts))
    | TGeneric (_, GenericPair, ts)         -> sprintf "Pair<%s>" <| String.Join(",", (List.map (printTyp << snd) ts))
    | TGeneric (_, GenericMap, ts)          -> sprintf "Map<%s>" <| String.Join(",", (List.map (printTyp << snd) ts))
    | TGeneric (_, GenericSysFun, ts)       -> sprintf "Func<%s>" <| String.Join(",", (List.map (printTyp << snd) ts))
    | TGeneric (_, GenericUnknown s, ts)    -> sprintf "%s<%s>" s <| String.Join(",", (List.map (printTyp << snd) ts))
    | TEnum (_,s)         -> s
    | TRec (_, pureTags)  -> printTagSyntax pureTags
    | TFun(f) -> printFun f
    | TAct(_)    -> "action (todo)"
    | TVoid -> "void"
  and printFun f =
    match f with
    | BIFun (inTs, outT) ->
      let inputPrint = if inTs.IsEmpty then "()" else List.map printTyp inTs |> List.reduce (sprintf "%s * %s")
      sprintf "(%s -> %s)" inputPrint (printTyp outT)
    | IFun _            -> "IFunction"
    | ParametricFun _    -> "ParametricFun"

  let printBinOp binOp =
    match binOp with
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Mod -> "%"
    | Div -> "/"
    | Eq -> "=="
    | LT -> "<"
    | LTE -> "<="
    | GT -> ">"
    | GTE -> ">="
    | LOR -> "||"
    | LAND -> "&&"

  let indent level = String.replicate level "  "
  let printPattern = function
    | PatMissing _ -> ""
    | PatTag ((_,tag),(_,x)) -> sprintf "%s %s" tag x
    | PatNone _ -> kw_none
    | PatSome (_,(_,x)) -> sprintf "%s %s" kw_some x
  let rec printExpr annoPrint level (e : expr<'eA, 'sA>)  =
    let printExpr = printExpr annoPrint
    let lvl1 = level + 1
    let ind = (indent level)
    let e_str,t_str =
      match e with
      | EMissing (anno, _)      -> "##MIS", (annoPrint anno)
      | CDouble  (anno, _, f)   -> sprintf "%f" f, (annoPrint anno)
      | CBool    (anno, _, b)   -> sprintf "%b" b, (annoPrint anno)
      | CStr     (anno, _, str) -> sprintf "\"%s\"" str, (annoPrint anno)
      | CEnum    (anno,_, s)    -> s, (annoPrint anno)
      | Pair     (anno,_,e1,e2)   -> sprintf "{%s,%s}" (printExpr level e1) (printExpr level e2), (annoPrint anno)
      | ENone    (anno, _, typAnno) -> sprintf "%s : %s" kw_none (printTyp typAnno), (annoPrint anno)
      | ESome    (anno,_,e)         -> sprintf "%s %s" kw_some (printExpr level e), (annoPrint anno)
      | Var      (anno, _, (_, x))  -> x,  annoPrint anno
      | Proj     (anno,_,e,(_,x))   -> sprintf "%s.%s" (printExpr level e) x, annoPrint anno
      | Filter   (anno,_,e,tags)   ->
        let e_str = printExpr level e
        let anno_str = annoPrint anno
        let tag_str = printTagSyntax <| List.map snd tags
        sprintf "%s:%s" e_str tag_str, anno_str
      | Let      (anno, _, lbs, e)     ->
        let lets = String.concat "\n" <| List.map (printLb annoPrint level) lbs
        sprintf "%s\n%s%s%s%s\n%s%s" lets ind kw_in (indent lvl1) (printExpr lvl1 e) ind kw_end, annoPrint anno
      | If       (anno,_,e1,e2,e3)     ->
        sprintf "%s %s\n%s%s %s\n%s%s %s" kw_if (printExpr lvl1 e1) ind kw_then (printExpr lvl1 e2) ind kw_else (printExpr lvl1 e3), annoPrint anno
      | Match    (anno,_,e1,mcs)       ->
        let mcs = String.concat "\n" <| List.map (printMatchCase annoPrint lvl1) mcs
        sprintf "%s %s %s\n%s\n%s%s" kw_match (printExpr lvl1 e1) kw_with mcs ind kw_end, annoPrint anno
      | Map      (anno,_,fis,where,e)   ->
        let forIns = String.concat "\n" <| List.map (printForIn annoPrint kw_map lvl1) fis
        let where_str = printWhere annoPrint level where
        sprintf "%s\n%s%s%s\n%s%s\n%s%s" forIns where_str ind kw_with (indent lvl1) (printExpr lvl1 e) ind kw_end, annoPrint anno
      | List     (anno,_,es,tOpt)         ->
        if es.IsEmpty && tOpt.IsSome then sprintf "[] : %s" (printTyp tOpt.Value), ""
        else sprintf "[%s]" (String.concat ";" <| List.map (printExpr lvl1 << snd) es), annoPrint anno
      | FCall    (anno,_,e,es)         ->
        let es_str = String.concat "," <| List.map (printExpr lvl1 << snd) es
        sprintf "%s(%s)" (printExpr level e) es_str, annoPrint anno
      | BinOp    (anno,_,binOp,e1,e2)  ->
        sprintf "%s%s%s" (printExpr level e1) (printBinOp binOp) (printExpr level e2), annoPrint anno
      | CreateRec (anno,_,(_,name), fields) ->
        let printField (((_, x), value ) as field) = sprintf "\n%s%s = %s" ind x (printExpr level value)
        let inits = String.concat ", " <| List.map printField fields
        sprintf "%s %s{%s%s}" kw_new name inits (if inits.Length <> 0 then (ind+"\n") else ""), (annoPrint anno)
      | EPar     (anno,_,e1)           -> sprintf "(%s)" <| printExpr level e1, annoPrint anno
    e_str + t_str
  and printForIn annoPrint kind level (sA, iters) =
    let reg many = List.map (fun ((_,x),e) -> x, (printExpr annoPrint (level+1) e)) many
    let kvp = fun ((_,(_,k),(_,v)), e) -> [sprintf "(%s,%s)" k v, (printExpr annoPrint (level+1) e)]
    let iter,src = Choices.joinChoice reg kvp iters |> List.unzip
    let xs = String.concat "," <| iter
    let exprs = String.concat "," <| src
    sprintf "%s%s %s %s %s" (indent level) kind xs kw_in exprs
  and printWhere annoPrint level whereCon =
    match whereCon with
    | None -> ""
    | Some (_, e) ->
      sprintf "%s%s %s\n" (indent level) kw_where (printExpr annoPrint (level+1) e)
  and printMatchCase annoPrint level (_, pattern, e) =
    sprintf "%s| %s -> %s" (indent level) (printPattern pattern) (printExpr annoPrint (level + 1) e)
  and printLb annoPrint level (_,(_, x),e) =
    sprintf "%slet %s = %s" (indent level) x <| printExpr annoPrint (level + 1) e

  let printProjs x projs =
    let projs_str = String.concat "" <| List.map (fun (_,_,(_,x)) -> sprintf ".%s" x) projs
    sprintf "%s%s" x projs_str

  let printAst annoPrint (decs : dec<'eA, 'sA> list) =
    let rec printStmt level stmt =
      match stmt with
      | SkippedStmt _             -> ""
      | SUpdate (_,forIn, where, stmt') ->
        let forIn_str = printForIn annoPrint kw_update level forIn
        let where_str = printWhere annoPrint level where
        let body = printStmt (level+1) stmt'
        sprintf "%s\n%s%s%s\n%s\n%s%s\n" forIn_str where_str (indent level) kw_with body (indent level) kw_end
      | SAss  (_, ((_, (_,x)), projs), e) ->
        let proj_str = printProjs x projs
        sprintf "%s%s =\n%s%s" (indent level) proj_str (indent (level+1)) (printExpr annoPrint (level + 1) e)
      | SLet   (lb) -> printLb annoPrint level lb
      | SDo (_, ((_, (_,x)), projs), args) ->
        let argString = (String.concat "," <| List.map (printExpr annoPrint 0 << snd) args)
        sprintf "%s %s(%s)" kw_do (printProjs x projs) argString
      | SDoCSharp (_, (_,ident)) ->
        sprintf "%s %s()" kw_doCSharp ident
      | SOverwrite(_,((_, (_,x)), projs), e) ->
        let proj_str = printProjs x projs
        sprintf "%s%s <|\n%s%s" (indent level) proj_str (indent (level+1)) (printExpr annoPrint (level + 1) e)
      | STransfer(_,((_, (_,x1)), projs1), e, ((_, (_,x2)), projs2)) ->
        let proj_str1 = printProjs x1 projs1
        let proj_str2 = printProjs x2 projs2
        sprintf "%s%s |> %s |> %s" (indent level) proj_str1 (printExpr annoPrint 0 e) proj_str2
      | SBlock (_,stmts) -> String.concat "\n" <| List.map (printStmt level) stmts
      | SMatch (_, switch, cases) ->
        let printSwitchCase (_,pat,stmt) =
          let case =
            match pat with
            | PatMissing _ -> ""
            | PatTag ((_,tag),(_,x)) -> sprintf "%s %s" tag x
            | PatNone _ -> kw_none
            | PatSome (_,(_,x)) -> sprintf "%s %s" kw_some x
          let t = printStmt (level+2) stmt
          //printfn "Inner case:%s" t
          sprintf "%s%s %s:\n%s" (indent <| level+1) kw_case case t//(printStmt (level+2) stmt)
        let scs = String.concat "\n" <| List.map printSwitchCase cases
        sprintf "%s%s (%s){\n%s\n%s}" (indent level) kw_switch (printExpr annoPrint level switch) scs (indent level)
      | SIf(_, cond, trueB, falseOpt) ->
        let cStr = printExpr annoPrint level cond
        let tStr = printStmt (level+1) trueB
        let fStr = Option.map (fun (_,f) -> sprintf "%s%s {\n%s\n%s}" (indent level) kw_else (printStmt (level+1) f) (indent level)) falseOpt |> Option.defaultValue ""
        sprintf "%s%s(%s){\n%s\n%s}\n%s" (indent level) kw_if cStr tStr (indent level) fStr

    let printParamInfo (_,(_,f),t) = sprintf "%s : %s" f <| printTyp t

    let rec printDec (dec: dec<'eA, 'sA>) =
      match dec with
      | SkippedDec _ -> ""
      | EndOfFile _ -> ""
      | FunDec (_, (_, f), paramInfos, e_body) ->
        let param_str = String.concat ", " <| List.map printParamInfo paramInfos
        let e_str = printExpr annoPrint 1 e_body
        sprintf "%s %s (%s) = \n%s\n" kw_fun f param_str e_str
      | ActDec (_, (_, f), paramInfos, stmt) ->
        let param_str = String.concat ", " <| List.map printParamInfo paramInfos
        let stmts_str = (printStmt 1 stmt)
        sprintf "%s %s (%s) %s \n%s\n%s\n" kw_action f param_str kw_with stmts_str kw_end
      | Data (_, d) ->
        let dataName = match d.name with (_, x) -> x
        let signature = sprintf "%s %s%s\n" kw_data dataName (d.extends |> Option.map ((+) " extends ") |> Option.defaultValue "")
        let printField ((_, (_, x), typ ) as field, _) = sprintf "    %s : %s\n" x (printTyp typ)
        List.fold (fun state field -> state + (printField field)) signature d.fields + "end\n"
      | Import(_, modul) ->
        sprintf "%s %s\n" kw_import (snd modul)

    String.concat "\n" <| List.map printDec decs

  let printASTWithTypeInfo<'a,'b> : dec<typ<'a>, 'b> list -> string = printAst (sprintf ":%s" << printTyp)
  let printASTWithNoInfo<'a,'b> : dec<'a, 'b> list -> string = printAst (fun _ -> "")

  let printErr str err errPos =
    let lineStart (str : string) pos =
      let mutable lineStart = pos
      let mutable lineStop = pos
      let mutable loop = true
      while lineStart > 0 && loop do
        if (str.[lineStart]) = '\n'
        then
          loop <- false
          lineStart <- lineStart + 1
        else
          lineStart <- lineStart - 1
      loop <- true
      while lineStop < str.Length && loop do
        if (str.[lineStop]) = '\n'
        then loop <- false
        else lineStop <- lineStop + 1
      (str.Substring(lineStart, lineStop - lineStart), pos - lineStart)

    let (errLine,errLinePos) = lineStart str errPos
    printfn "Error %A\n%s" (err) (errLine);
    printfn "%s^" (String.replicate errLinePos " ")
    ()