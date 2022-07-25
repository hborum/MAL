namespace itu.dk.MAL

#nowarn "40" // Some soundness will be checked at runtime

///<summary>
///This parser is bases on Christian and Simon's work on creating a faul-tolerent parser for MAL.
///The parser should never fail on parsing a text file, instead of failing it should return an AST
///which contains holes whenever parsing could not suceed.
///The basic idea seems to be:
/// - Whenever multiple tokens can be expected then a `try`-parser is used.
///   Which means a `try`-parser should only be used in situations where an alternative branch is present.
/// - Whenever a token is expected then an `eat`-parser is used.
///   If the `eat`-parser fails then it returns the ast specified by its failure function.
/// - Optional syntax is parsed using ´pOption´. This is only intended for usages where the AST contains a corresponding `Option ast`
///</summary>

module Parser =
  open FParsec
  open AST
  open Structure
  let csProvider = new Microsoft.CSharp.CSharpCodeProvider()
  let operandsInfo =
    [
      (Left, [("||", LOR)]);
      (Left, [("&&", LAND)]);
      // Note order is important for same prefix
      (Left, [ (">=", GTE); (">", GT); ("<=", LTE); ("<", LT); ("==", Eq) ]);
      (Left, [("+", Plus); ("-", Minus)]);
      (Left, [("*", Mult); ("/", Div) ; ("%", Mod)]);
    ]

  let pChainl1 p p2 op =
      p >>= fun x -> ((many (op .>>. p2))) |>> fun opPs ->
          List.fold (fun x (f, y) -> f x y) x opPs

  let pChainr1 p p2 op =
          let rec calc x rhs =
              match rhs with
              | (f, y)::tl -> f x (calc y tl)
              | [] -> x

          pipe2 p (many (op .>>. p2)) calc

  let assocToChain assoc =
    match assoc with
    | Left -> pChainl1
    | Right -> pChainr1

  let parseFullPos =
    parse {
      let! pos = getPosition
      return pos.Index
    }

  let comment =
    parse {
      do! skipString "//"
      do! skipManyTill anyChar (skipNewline <|> eof)
    } <|>
    parse {
      do! skipString "/*"
      do! skipManyTill anyChar (skipString "*/" <|> eof)
    }

  let pWhiteSpace: Parser<unit, unit> =
    parse {
      do! skipMany (spaces1 <|> comment)
    }

  let parseToken p =
    parse {
      let! fullStart = parseFullPos
      do! pWhiteSpace
      let! start = parseFullPos
      let! res = p
      //let! tokenEnd = parseFullPos
      return (FullPos(int fullStart, int start), res)
    }

  let pOption parser =
      parse {
        let! ast = parser
        return Some ast
      }
    <|>
      parse {
        return None
      }

  let pName resCheck =
    parse {
      let! (first, res) = (letter <|> (pchar '_')) .>>. manyChars (letter <|> digit <|> (pchar '_'))
      let name = string first + res
      if resCheck && List.contains name keywords
      then return! fail (sprintf "%s is a reserved keyword" name)
      else return name
    }

  let pString =
    parseToken <| parse {
      let! (first, res) = (letter <|> (pchar '_')) .>>. manyChars (letter <|> digit <|> (pchar '_'))
      let name = string first + res
      return name
    }

  ///<summary>
  ///This parser will parse using `p`.
  ///Upon failure it returns the invocation of failureReturn
  ///</summary>
  let eat1 (failureReturn : int64 -> 'a) (p : Parser<'a, unit>) =
    fun stream ->
      let pos = parseFullPos stream
      let reply = attempt(p) stream
      if reply.Status <> Ok
      then
        Reply(failureReturn (pos.Result))
      else
        reply

  let eat1Str (p : Parser<constantSyntax<Position> * string,unit>)
    : CharStream<unit> -> Reply<constantSyntax<Position> * string> =
    eat1 (fun pos -> ({kwStart = Missing(int pos); kwEnd = Missing(int pos); str = ""}, "")) p

  let eat1Typ p =
    eat1 (fun pos -> (Missing(int pos), TErr)) p

  let eat1Unit p =
    eat1 (fun pos -> (Missing(int pos), ())) p

  let eat1Expr p =
    eat1 (fun x -> (EMissing((), Missing(int x)))) p

  let eat1Format p =
    eat1 (fun _ -> MissingFormat) p

  let eat1NoPos def p =
    fun stream ->
      let pos = parseFullPos stream
      let mutable reply = attempt(p) stream
      if reply.Status <> Ok then
        reply <- Reply(def (pos.Result))
      reply

  let eat1ExprNoPos p =
    eat1NoPos (fun x -> (EMissing((), Missing(int x)))) p

  (* This can fail *)
  let try1 p =
    parse {
      // Assert that p parses. If it does not, this will fail with a non-fatal error.
      do! followedBy p
      let! res = p
      return res
    }


  let parseNameInternal limit =
    parse {
      let! pos, s = parseToken (pName limit)
      let! endPos = parseFullPos
      let syntax = {kwStart = pos; kwEnd = FullPos(int endPos, int endPos); str = s}
      return (syntax,s)
    }

  let parseNameUnlimited = parseNameInternal false
  let parseName = parseNameInternal true

  let parseStringContent =
    parse {
      let! str = manyTill anyChar (followedBy <| pchar '"')
      return System.String.Concat(Array.ofList(str))
    }

  let parseDouble =
    parseToken <| parse {
      let! float = regex "-?[0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)?" .>> notFollowedBy letter
      return System.Double.Parse(float, System.Globalization.CultureInfo.InvariantCulture),float
    }

  let parseKeyword kw =
    parseToken <| skipString kw .>> notFollowedBy (letter <|> digit <|> (pchar '_'))

  let parseSymbol symbol =
    parseToken <| skipString symbol

  let parseEof =
    parseToken <| eof

  let skipLine =
    parseToken <| (notFollowedBy eof >>. restOfLine false)

  let parseTag = eat1Str <| parseName
  let parseMulTags =
    parse {
        let! (bStart, ()) = attempt <| parseSymbol "{"
        let! tags = sepBy1 parseTag (attempt <| parseSymbol ",")
        let! (bEnd, _) = eat1Unit <| (parseSymbol "}")
        return (bStart, tags)
      }

  let parseTags =
      parseMulTags
    <|>
      parse {
       let! pos = parseFullPos
       let! tag = parseTag
       return FullPos (int pos,int pos),[tag]
      }

  let labelToType lab (pos: Position) =
    match lab with
    | "Float" -> TDouble(pos)
    | "Bool" -> TBool(pos)
    | "String" -> TStr(pos)
    | "Reserve" -> TReserve(pos)
    | "IFunction" -> TFun(IFun(pos))
    | t -> TRec(pos, [t])

  let rec parseType : Parser<typ<Position>, unit> =
      parse {
        let! (pos, tags) = parseMulTags
        return TRec(pos, List.map snd tags)
      }
    <|>
      parse {
        let! (namePos, name) = eat1Str <| parseName
        return!
          parse {
              let! (dStart, _) = attempt <| parseSymbol "<"
              let! gens =
                  parse {
                    let! ((_,t1), ts) =
                      (attempt <| parseTyp) .>>.
                        (many <|
                          parse {
                            let! (sep,_) = attempt <| parseSymbol ","
                            let! (_,expr) = eat1Typ parseTyp
                            let syntax = { kwSep = Some sep }
                            return (syntax, expr)
                          }
                        )
                    return ({kwSep = None}, t1)::ts
                  }
                <|>%
                  []
              let! (dEnd, _) = eat1Unit <| (parseSymbol ">")
              let syntax = {kwName = namePos.kwStart;
                            kwLA = dStart;
                            kwRA = dEnd}
              let genId =
                match name with
                | "Map"     -> GenericMap
                | "Func"    -> GenericSysFun
                | "List"    -> GenericList
                | "Option"  -> GenericOption
                | "Pair"    -> GenericPair
                | _         -> GenericUnknown name
              return TGeneric(syntax, genId, gens)
            }
          <|>%
            if List.contains name enumerations
            then TEnum(namePos.kwStart, name)
            else if name.Length > 0
            then labelToType name namePos.kwStart
            else TMissing(namePos.kwStart)
      }

  and parseTyp : Parser<Position * typ<Position>, unit> =
    parse {
      let! res = parseType
      // We don't need the leading SyntaxAnno here, since the type contains all info needed
      return (NoPos, res)
    }

  let parseParameter =
    parse {
      let! (nameSyntax, parName) = attempt <| parseName
      let! (colon, _) = eat1Unit <| parseSymbol ":"
      let! (_, parType) = eat1Typ parseTyp
      let syntax = {kwColon = colon;
                    kwComma = None}
      return paramInfo(syntax, (nameSyntax, parName), parType)
    }

  let parseCommaParameter =
    parse {
      let! (comma,_) = attempt <| parseSymbol ","
      let! (nameSyntax, parName) = eat1Str parseName
      let! (colon, _) = eat1Unit <| parseSymbol ":"
      let! (_, parType) = eat1Typ parseTyp
      let syntax = {kwColon = colon;
                    kwComma = Some comma}
      return paramInfo(syntax, (nameSyntax, parName), parType)
    }

  let rec parseMatchPattern =
      parse {
        let! (nonePos,_) = attempt <| parseKeyword kw_none
        return PatNone(nonePos)
      }
    <|>
      parse {
        let! (somePos,_) = attempt <| parseKeyword kw_some
        let! (namePos, name) = eat1Str <| parseName
        return PatSome(somePos,(namePos, name))
      }
    <|>
      parse {
        let! (tagPos, tag)  = attempt <| parseName
        let! (namePos, name) = eat1Str <| parseName
        return PatTag((tagPos.kwStart, tag), (namePos, name))
      }

  let parseKVP = parse {
    let! (dStart, _) = attempt <| parseSymbol "("
    let! key = eat1Str parseName
    let! (comma, _) = eat1Unit <| parseSymbol ","
    let! value = eat1Str parseName
    let! (dEnd, _) = eat1Unit <| parseSymbol ")"
    let syntax = {kwName = dStart;
                  kwLA = dStart;
                  kwComma = comma;
                  kwRA = dEnd}
    return syntax, key, value
  }

  let rec parseExpr = parseOp operandsInfo
  and parseOpLevel opsInfo =
    parse {
      let opParsers =
        List.map
          (fun (op, aOp) ->
          parse {
            let! (binOpPos, _) = attempt <| parseSymbol op
            return (fun e1 e2 -> BinOp ((), binOpPos, aOp, e1, e2))
        }) opsInfo
      let asSeq = List.toSeq opParsers
      return! choice asSeq
    }
  and parseOp (operandsInfo: (assoc * (string * binOp) list) list) =
    match operandsInfo with
    | [] -> pAtomExpr
    | (assoc, infos)::ops' ->
      parse {
        let e1Parser = (parseOp ops') // Can fail
        let e2Parser = (eat1Expr <| parseOp ops') // Cannot fail
        let! l = (assocToChain assoc) e1Parser e2Parser (parseOpLevel infos)
        return l
      }
  and parseString (pos, lastS) =
    parse {
      match () with
      | _ when lastS = kw_true  -> return CBool((), pos, true)
      | _ when lastS = kw_false -> return CBool((), pos, false)
      | _ when lastS = kw_LumpedBio -> return CEnum((), pos, kw_LumpedBio)
      | _ when lastS = kw_LumpedFree -> return CEnum((), pos, kw_LumpedFree)
      | _ when lastS = kw_LumpedSBio -> return CEnum((), pos, kw_LumpedSBio)
      | _ when lastS = kw_LumpedSFree -> return CEnum((), pos, kw_LumpedSFree)
      | _ when lastS = kw_LumpedSSurr -> return CEnum((), pos, kw_LumpedSSurr)
      | _ when lastS = kw_StateActive -> return CEnum((), pos, kw_StateActive)
      | _ when lastS = kw_StateDead -> return CEnum((), pos, kw_StateDead)
      | _ when lastS = kw_StateDisabled -> return CEnum((), pos, kw_StateDisabled)
      | _ when lastS = kw_FreePolicyNone -> return CEnum((), pos, kw_FreePolicyNone)
      | _ when lastS = kw_FreePolicyScaled -> return CEnum((), pos, kw_FreePolicyScaled)
      | _ when lastS = kw_FreePolicyRemoved -> return CEnum((), pos, kw_FreePolicyRemoved)
      | _ when List.contains lastS keywords -> return! fail (sprintf "%s is a reserved keyword" lastS)
      | _ -> return! parseAddr (Var((), pos, (pos, lastS)))
    }
  and parseAddr prevExpr =
     parse {
        let! (dotPos, ()) = attempt <| parseSymbol "@"
        let (leftPos, _) = PositionBounds.exprSAnno PositionBounds.Left prevExpr
        let! (nameSA,name) = eat1Str <| parseName
        let fkPos = { kwStart = dotPos; kwEnd = dotPos; str = "" }
        let! proj_expr = parseAddr( Proj((), {kwDot = dotPos}, (Var ((), fkPos, (fkPos,"@"))), (nameSA,name)) )
        let (rightPos, _) = PositionBounds.exprSAnno PositionBounds.Right proj_expr
        let forIn = forIn( ({ commas = [] ; kwFor = leftPos; kwIn = leftPos}, Choice1Of2 [(fkPos,"@"), prevExpr ]) )
        return Map((), { loopName = "InternalMap" ; kwWith = dotPos; kwEnd = rightPos}, [forIn], None, proj_expr)
      }
    <|>
      parse {
        let! (dotPos, ()) = attempt <| parseSymbol "."
        let! (nameSA,name) = eat1Str <| parseName
        return! parseAddr(Proj((), {kwDot = dotPos}, prevExpr, (nameSA,name)))
      }
    <|>
      parse {
        let! (colPos, ()) = attempt <| parseSymbol ":"
        let! (_, tags) = parseTags
        return! parseAddr(Filter((), {kwColon = colPos}, prevExpr, List.map (fun (sA, tag) -> (sA.kwStart, tag)) tags))
      }
    <|>
      parse {
        let! (lPos, _) = attempt <| parseSymbol "("
        let! exprs = parseExprs ","
        let! (rPos, _) = eat1Unit <| parseSymbol ")"
        let fSyntax = { kwLPar = lPos; kwRPar = rPos }
        return! parseAddr (FCall((), fSyntax, prevExpr, exprs))
      }
    <|>
      parse {
          return prevExpr
      }

  and pAtomExpr: Parser<expr<unit, Position>, unit> =
    parse {
      let! (sQuote, _) = attempt <| parseSymbol "\""
      let! str = parseStringContent
      let! (eQuote, _) = eat1Unit <| parseSymbol "\""
      let syntax = {kwStart = sQuote; kwEnd = eQuote; str = sprintf "\"%s\"" str}
      return CStr((), syntax, str)
    }
  <|>
    parse {
      let! (dStart, _) = attempt <| parseSymbol "{"
      let! e1 = eat1Expr parseExpr
      let! (comma, _) = eat1Unit <| parseSymbol ","
      let! e2 = eat1Expr parseExpr
      let! (dEnd, _) = eat1Unit <| parseSymbol "}"
      let syntax = {kwName = dStart;
                    kwLA = dStart;
                    kwComma = comma;
                    kwRA = dEnd}
      return Pair((), syntax, e1, e2)
    }
  <|>
    parse {
      let! (dStart, _) = attempt <| parseSymbol "["
      let! elems = parseExprs ";"
      let! (dEnd, _) = eat1Unit <| parseSymbol "]"
      let syntax = {kwLBrck = dStart;
                    kwRBrck = dEnd}
      let! anno =
        pOption <|
          parse {
            let! (dStart, _) = attempt <| parseSymbol ":"
            let! _,t = parseTyp
            return t
          }
      return List((), syntax, elems,anno)
    }
  <|>
    parse {
      let! (numberPos, (number, flString)) = attempt <| parseDouble
      let! numberEnd = parseFullPos
      let syntax = {kwStart = numberPos; kwEnd = FullPos (int numberEnd, int numberEnd); str = sprintf "\"%s\"" flString}
      return CDouble((), syntax, number)
    }
  <|>
    parse {
      let! bindings = many1 parseLetBinding
      let! (inPos, ()) = eat1Unit <| parseKeyword kw_in
      let! e2 = eat1Expr parseExpr
      let! (endPos, ()) = eat1Unit <| parseKeyword kw_end
      let letSyntax = {kwIn = inPos; kwEnd = endPos}
      return Let((), letSyntax, bindings, e2)
    }
  <|>
    parse {
      let! (inPos, ()) = attempt <| parseKeyword kw_some
      let! e = eat1Expr parseExpr
      return ESome((), inPos, e)
    }
  <|>
    parse {
      let! (inPos, ()) = attempt <| parseKeyword kw_none
      let! (colPos, ()) = eat1Unit <| parseSymbol ":"
      let! typ = parseType
      return ENone((), { kwNone = inPos ; colon = colPos}, typ)
    }
  <|>
    parse {
      let! (matchPos, ()) = attempt <| parseKeyword kw_match
      let! e1 = eat1Expr parseExpr
      let! (withPos, ()) = eat1Unit <| parseKeyword "{"
      let! cases = many <| parse {
        let! (barPos, ()) = attempt <| parseSymbol "|"
        let! pattern = eat1 (fun pos -> PatMissing(Missing (int pos))) parseMatchPattern
        let! (arrowPos, ()) = eat1Unit <| parseSymbol "->"
        let! e = eat1Expr parseExpr
        let matchCaseSyntax = { kwBar = barPos; kwArrow = arrowPos }
        return matchCase(matchCaseSyntax, pattern, e)
      }
      let! (endPos, ()) = eat1Unit <| parseKeyword "}"
      let matchSyntax = { kwMatch = matchPos; kwWith = withPos; kwEnd = endPos }
      return Match((), matchSyntax, e1, cases)
    }
  <|>
    parse {
      let! (ifPos, ()) = attempt <| parseKeyword kw_if
      let! e1 = eat1Expr parseExpr
      let! (thenPos, ()) = eat1Unit <| parseKeyword kw_then
      let! e2 = eat1Expr parseExpr
      let! (elsePos, ()) = eat1Unit <| parseKeyword kw_else
      let! e3 = eat1Expr parseExpr
      let ifSyntax = { kwIf = ifPos; kwThen = thenPos; kwElse = elsePos }
      return If((), ifSyntax, e1, e2, e3)
    }
  <|>
    parse {
      let! iters = many1 <| (parseDoIn kw_map (parseDoInDict <|> parseDoInMany))
      let! filter = parseWhere
      let! (withPos, ()) = eat1Unit <| parseKeyword "{"
      let! e = eat1Expr parseExpr
      let! (endPos, _) = eat1Unit <| parseKeyword "}"
      let forSyntax = { loopName = kw_map ; kwWith = withPos; kwEnd = endPos }
      return Map((), forSyntax, iters, filter, e)
    }
  <|>
    parse {
      let! (newPos, ()) = attempt <| parseKeyword kw_new
      let! (recPos, recName) = eat1Str <| parseName
      let! (lPos, ()) = attempt <| parseSymbol "{"
      let! fields =
        sepBy
          (parse {
            let! (tagSa, fieldName) = attempt <| parseName
            let! (equal, ()) = eat1Unit <| parseSymbol "="
            let! expr = eat1Expr parseExpr
            return (((tagSa, fieldName), expr), equal)
            }
          )
          (attempt <| parseSymbol ",")
      let fields, equals = List.unzip fields
      let! (rPos, ()) = attempt <| parseSymbol "}"

      let syntax =
        { kwNew = newPos
        ; LBrack = lPos
        ; RBrack = rPos
        ; commas = []
        ; equals = equals
        }
      return CreateRec((), syntax, (recPos, recName), fields)
    }
  <|>
    parse {
      let! (sPar, _) = attempt <| parseSymbol "("
      let! e = eat1Expr parseExpr
      let! (ePar, _) = eat1Unit <| parseSymbol ")"
      return! parseAddr <| EPar((), {kwPStart = sPar; kwPEnd = ePar}, e)
    }
  <|>
    parse {
      let! (startPos, x) = attempt <| pString
      let! endPos = parseFullPos
      let syntax = {kwStart = startPos; kwEnd = FullPos (int endPos, int endPos); str = x}
      return! parseString (syntax, x)
    }

  and parseLetBinding =
    parse {
      let! (letPos, ()) = attempt <| parseKeyword kw_let
      let! (varPos, var) = eat1Str <| parseName
      let! (letEq, ()) = eat1Unit <| parseSymbol "="
      let! e1 = eat1Expr parseExpr
      return letBinding({ kwLet = letPos; kwEq = letEq}, (varPos, var), e1)
    }
  //and parseIter = (parseKVP |>> Choice2Of2) <|> (eat1Str parseName |>> Choice1Of2)

  and parseDoIn kw iterParser =
    parse {
      let! (forPos, _) = attempt <| parseKeyword kw
      let! inPos,iters = iterParser
      return forIn({commas = [] ; kwFor = forPos; kwIn = inPos}, iters)
    }
  and parseDoInSingle =
    parse {
      let! i = eat1Str parseName
      let! (inPos, _) = eat1Unit <| parseKeyword kw_in
      let! inExpr = eat1Expr parseExpr
      return inPos, Choice1Of2 [i, inExpr]
    }

  and parseDoInMany =
    parse {
      let! vars = (sepBy1 (eat1Str parseName) (attempt <| parseSymbol ","))
      let! (inPos, _) = eat1Unit <| parseKeyword kw_in
      let! ins = (sepBy1 (eat1Expr parseExpr) (attempt <| parseSymbol ","))
      //problem: zip
      return inPos, Choice1Of2 (List.zip vars ins)
    }

  and parseDoInDict =
    parse {
      let! iter = parseKVP
      let! (inPos, _) = eat1Unit <| parseKeyword kw_in
      let! inExpr = eat1Expr parseExpr
      return inPos, Choice2Of2 (iter, inExpr)
    }

  and parseWhere =
    pOption <|
      parse {
        let! (wherePos, _) = attempt <| parseKeyword kw_where
        let! whereExpr = eat1Expr parseExpr
        let whereSyntax = { kwWhere = wherePos }
        return where(whereSyntax, whereExpr)
      }

  and parseSymbExpr symb =
    parse {
      let! (sep,_) = attempt <| parseSymbol symb
      let! expr = eat1Expr parseExpr
      let syntax = { kwSep = Some sep }
      return (syntax, expr)
    }
  and parseExprs (symbSep : string) =
      parse {
        let! (e1, es) = (attempt <| parseExpr) .>>. (many (parseSymbExpr symbSep))
        return ({kwSep = None}, e1)::es
      }
    <|>%
      []

  let parseLocation =
    many <| parse {
      let! (dotPos, _) = attempt <| parseSymbol "."
      let! (varPos, var) = eat1Str <| parseNameUnlimited
      return ((), {kwDot = dotPos}, (varPos, var))
    }

  let rec parseStatement =
      parse {
        let! letBinding = parseLetBinding
        return SLet(letBinding)
      }
    <|>
      parse {
        let! forIn = parseDoIn kw_update (parseDoInDict <|> parseDoInSingle)
        let! filter = parseWhere
        let! (withPos, _) = eat1Unit <| parseKeyword "{"
        let! stmts = many parseStatement
        let! (endPos, _) = eat1Unit <| parseKeyword "}"
        let syntax = { loopName = kw_update
                     ; kwWith = withPos
                     ; kwEnd = endPos }
        return SUpdate(syntax, forIn, filter, SBlock (withPos,stmts))
      }
    <|>
      parse {
        let! (doKeywordPos, _) = attempt <| parseKeyword kw_doCSharp
        let! (xPos, x) = attempt <| parseName
        let acall = { kwDoCS = doKeywordPos}
        return SDoCSharp(acall, (xPos, x))
      }
    <|>
      parse {
        let! (doKeywordPos, _) = attempt <| parseKeyword kw_do
        let! (xPos, x) = attempt <| parseName
        let! projs = parseLocation
        let! (lPos, _) = eat1Unit <| parseSymbol "("
        let! exprs = parseExprs ","
        let! (rPos, _) = eat1Unit <| parseSymbol ")"
        let acall = { kwDo = doKeywordPos; kwLPar = lPos; kwRPar = rPos }
        return SDo(acall, (((), (xPos, x)), projs), exprs)
      }
    <|>
      parse {
        let! (xPos, x) = attempt <| parseName
        let! projs = parseLocation
        let! s =
            parse {
              let! (equalPos, _) = attempt <| parseSymbol "<|"
              let! expr = eat1Expr parseExpr
              let syntax = {Arrow  = equalPos}
              return SOverwrite(syntax, (((), (xPos, x)), projs), expr)
            }
          <|>
            parse {
              let! (lArrow, _) = attempt <| parseSymbol "|>"
              let! expr = eat1Expr parseExpr
              let! (rArrow, _) = eat1Unit <| parseSymbol "|>"
              let! (xPos2, x2) = attempt <| parseName
              let! projs2 = parseLocation

              let syntax = {LArrow = lArrow ; RArrow = rArrow}
              return STransfer(syntax, (((), (xPos, x)), projs ), expr, (((), (xPos2, x2)), projs2))
            }
          <|>
            parse {
              let! (equalPos, _) = eat1Unit <| parseSymbol "="
              let! expr = eat1Expr parseExpr
              let syntax = {kwEq = equalPos}
              return SAss(syntax, (((), (xPos, x)), projs), expr)
            }
        return s
      }
    <|>
      parse {
        let! (ifPos, ()) = attempt <| parseKeyword kw_if
        let! (lPar, ()) = eat1Unit <| parseSymbol "("
        let! e1 = eat1Expr parseExpr
        let! (rPar, ()) = eat1Unit <| parseSymbol ")"
        let parseLBrace = eat1Unit <| parseSymbol "{"
        let parseRBrace = eat1Unit <| parseSymbol "}"
        let! (lbrace, ()) = parseLBrace
        let! trueBranch = many parseStatement
        let! (rbrace, ()) = parseRBrace
        let! falseBranch =
          pOption <| parse {
            let! (elsePos, ()) = attempt <| parseKeyword kw_else
            let! (lbrace, ()) = parseLBrace
            let! falseBranch = many parseStatement
            let! (rbrace, ()) = parseRBrace
            return {kwElse = elsePos; lBrace = lbrace; rBrace = rbrace}, SBlock(lbrace, falseBranch)
          }
        let ifSyntax = { kwIf = ifPos; lPar = lPar; rPar = rPar; lBrace = lbrace; rBrace = rbrace }
        return SIf(ifSyntax, e1, SBlock(lbrace, trueBranch) , falseBranch)
      }
    <|>
      parse {
        let! (switchPos, ()) = attempt <| parseKeyword kw_switch
        let! (lPar, ()) = eat1Unit <| parseSymbol "("
        let! e1 = eat1Expr parseExpr
        let! (rPar, ()) = eat1Unit <| parseSymbol ")"
        let! (lBrace, ()) = eat1Unit <| parseSymbol "{"
        let parseCase = parse {
          let! (casePos, ()) = attempt <| parseKeyword kw_case
          let! pat = eat1 (fun pos -> PatMissing(Missing (int pos))) parseMatchPattern
          let! (colPos, ()) = eat1Unit <| parseSymbol ":"
          let! body = many parseStatement
          let syntax = { kwCase = casePos; kwColon = colPos }
          return syntax, pat, SBlock(colPos, body)
        }
        let! cases = many parseCase
        let! (rBrace, ()) = eat1Unit <| parseSymbol "}"
        let syntax = { kwSwitch = switchPos; lPar = lPar; rPar = rPar; lBrace = lBrace; rBrace = rBrace}
        return SMatch(syntax, e1, cases)
      }

  let parseParams =
      parse {
        let! (p1, pars) = parseParameter .>>. (many parseCommaParameter)
        return p1::pars
      }
    <|>%
      []

  let rec parseDeclaration =
      parse {
        let! (exportPos, _) = attempt <| parseKeyword kw_export
        let! dec = parseDecOrSkip
        return Export(exportPos, dec)
      }
    <|>
      parse{
        let! (importPos, _) = attempt <| parseKeyword kw_import
        let! identifier = eat1Str <| parseName
        let sAImport = { kwImport = importPos }
        return Import(sAImport, identifier)
      }
    <|>
      parse{
        let! (importPos, _) = attempt <| parseKeyword kw_importData
        let! identifier = eat1Str <| parseName
        let sAImport = { kwImport = importPos }
        return DataImport(sAImport, identifier)
      }
    <|>
      parse {
        let! (funKeywordPos, _) = attempt <| parseKeyword kw_fun
        let! (fPos, fName) = eat1Str <| parseName
        let! (lParPos, _) = eat1Unit <| (parseSymbol "(")
        let! parameters = parseParams
        let! (rParPos, _) = eat1Unit <| (parseSymbol ")")
        let! (equalPos, _) = eat1Unit <| (parseSymbol "=")
        let! expr = eat1Expr parseExpr
        let syntax = {kwFun = funKeywordPos;
                      kwLPar = lParPos;
                      kwRPar = rParPos;
                      kwEq = equalPos}
        return FunDec(syntax, (fPos, fName), parameters, expr)
      }
    <|>
      parse {
        let! (funKeywordPos, _) = attempt <| parseKeyword kw_action
        let! (fPos, fName) = eat1Str <| parseName
        let! (lParPos, _) = eat1Unit <| (parseSymbol "(")
        let! parameters = parseParams
        let! (rParPos, _) = eat1Unit <| (parseSymbol ")")
        let! (withPos, _) = eat1Unit <| parseKeyword "{"
        let! stmts = many parseStatement
        let! (endPos, _) = eat1Unit <| parseKeyword "}"
        let syntax = {kwAct = funKeywordPos;
                      kwLPar = lParPos;
                      kwRPar = rParPos;
                      kwWith = withPos;
                      kwEnd = endPos}
        return ActDec(syntax, (fPos, fName), parameters, SBlock (withPos, stmts))
      }
    <|>
      // TODO: Improve Extend
      parse {
        let pFormat =
            parse {
              let! (pos, _) = attempt <| parseKeyword "CashFlow"
              return CashFlow
            }
          <|>
            parse {
              let! (pos, _) = attempt <| parseKeyword "PresentValueEndPoint"
              return PresentValueEnd
            }
          <|>
            parse {
              let! (pos, _) = attempt <| parseKeyword "PresentValueMidPoint"
              return PresentValueMid
            }
        let pMode =
            parse {
              let! (pos, _) = attempt <| parseKeyword "Average"
              return Average
            }
          <|>
            parse {
              let! (pos, _) = attempt <| parseKeyword "Debug"
              return Debug
            }
        let! (extPos, _) = attempt <| parseKeyword kw_data
        let! (namePos, name) = eat1Str <| parseName

        // maybe parse newline here to see if we should expect "extends" or not
        let! exts =
          pOption
            (parse {
              let! kw_extends,_ = attempt <| parseKeyword kw_extends
              let! _, name = eat1Str <| parseName
              return kw_extends, name
            })
        let extendsPos, extName = match exts with
                                  | None -> None, None
                                  | Some (a,b) -> Some a, Some b

        let! (endPos, _ ) = eat1Unit <| parseSymbol "{"
        let! fields =
          many (
            parse {
              // this is not the best solution,
              // we would also like an error message when there is a colon but missing name and/or type around it
              let! (namePos, name) = attempt <| parseName
              let! colonPos, _ = eat1Unit <| parseSymbol ":"
              let! (_,typ) = eat1Typ parseTyp
              let! output =
                pOption <| parse {
                  let! _ = attempt <| parseSymbol ","
                  let! kw_output, _ = eat1Unit <| parseKeyword kw_output
                  let! format = eat1Format pFormat
                  let! _, _ = eat1Unit <| parseSymbol "("
                  let! modes = sepBy pMode (try1 <| parseSymbol ",")
                  let! _, _ = eat1Unit <| parseSymbol ")"
                  return (typ, format, kw_output, modes)
                }
              return (({kwColon = colonPos}, (namePos, name), typ), output)
            }
          )
        let! (endPos, _ ) = eat1Unit <| parseSymbol "}"

        let extSyn = { kwData = extPos; kwEnd = endPos ; kwExtends = extendsPos }
        let dataDec = {name = namePos, name ; fields = fields ; extends = extName}
        return Data (extSyn, dataDec)
      }
    <|>
      parse {
           let parseContract contract_name =
            pOption
              (parse {
                let! (req_pos, _) = attempt <| parseKeyword contract_name
                let! (lpar1_pos, _) = eat1Unit <| parseKeyword "{"
                let! req_fields =
                  many (
                    parse {
                      let! (namePos, name) = attempt <| parseName
                      let! colonPos,_ = eat1Unit <| parseSymbol ":"
                      let! (_,typ) = eat1Typ parseTyp
                      return (({kwColon = colonPos}, (namePos, name), typ))
                    }
                  )
                let! (rpar1_pos, _) = eat1Unit <| parseKeyword "}"
                return req_fields, req_pos, lpar1_pos, rpar1_pos
              })


           let! (con_pos, _) = attempt <| parseKeyword kw_contract
           let! name = eat1Str <| parseName
           let! ext_res =
             pOption
               (parse {
                 let! ext_pos,_ = attempt <| parseKeyword kw_extends
                 let! _, name = eat1Str <| parseName
                 return ext_pos,name
               })

           let! (_, _ ) = eat1Unit <| parseSymbol "{"
           let! reqs = parseContract kw_requires
           let req_fields, req_pos, lpar1_pos, rpar1_pos =
             Option.defaultValue
                ( []
                , NoPos
                , NoPos
                , NoPos) reqs

           let! provs = parseContract kw_provides
           let prov_fields, prov_pos, lpar2_pos, rpar2_pos =
            Option.defaultValue
               ( []
               , NoPos
               , NoPos
               , NoPos) provs
           let! (end_pos, _ ) = eat1Unit <| parseSymbol "}"

           let extSyn =
             { kwContract  = con_pos
             ; kwExtends   = Option.map fst ext_res
             ; kwRequires  = req_pos, lpar1_pos, rpar1_pos
             ; kwProvides  = prov_pos, lpar2_pos, rpar2_pos
             ; kwEnd       = end_pos
             }

           return Contract (extSyn, name, Option.map snd ext_res, req_fields, prov_fields)
         }

  and parseDecOrSkip =
      attempt parseDeclaration
    <|>
      parse {
        let! (skipped, str) = attempt <| skipLine
        return SkippedDec(skipped, str)
      }

  let parseModuleDef =
      parse {
        let! (module_pos, _) = attempt <| parseKeyword kw_module
        let! identifier = eat1Str <| parseName
        let sAModule = { kwModule  = module_pos }
        return ImportModul(sAModule, identifier)
      }
    <|>% MainModul

  let parseFile =
      parse {
        let! moduleType = parseModuleDef
        let! decs = many parseDecOrSkip
        let! (endOfFile, _) = eat1Unit <| parseEof
        return moduleType , decs @ [EndOfFile(endOfFile)]
      }

  let rec parseModules path =
    parse {
      let! modul = parseFile
      match modul with
      | MainModul (_),decs
      | ImportModul (_,_),decs ->
        let otherModules =
          List.choose
            ( fun dec ->
              match dec with
              | Import(_, (nm_s,nm)) ->
                match doParse path (sprintf "%s.fmal" nm) with
                | None -> None
                | Some (parseResult) -> Some(DependAll, (nm_s, nm),parseResult)
              | DataImport(_, (nm_s,nm)) ->
                match doParse path (sprintf "%s.fmal" nm) with
                | None -> None
                | Some (parseResult) -> Some(DependData, (nm_s, nm),parseResult)
              | _ -> None
            ) decs
        return (modul , otherModules)
    }

  ///<summary>
  /// Parses the module found at `path`+`filename`.
  /// When succesful:
  ///  Returns a parseResult
  /// where the parseResult.dependencies is a flat map of all module dependencies.
  /// Invariant:
  ///   If the parsed module is an ImportModule then the dependencies should be
  ///   an empty list. Import-declerations are not allowed in import-modules.
  ///</summary>
  and doParse path fileName : program<Unit, Position> option =
    try
      let source = System.IO.File.ReadAllText <| path + fileName
      match run (parseModules path) source with
      | Success((modul, dependencies), _, pos) ->
        Some
          { modul = modul
          ; dependencies = dependencies
          ; source = source
          }
      | Failure(errorMsg, err, _) ->
          printfn "Failure: %s" errorMsg
          None
    with
      | e ->
        printfn "Could not find %s" (path + fileName)
        None

  let doParseSource path source : program<Unit, Position> option =
    match run (parseModules path) source with
    | Success((modul, dependencies), _, pos) ->
      Some
        { modul = modul
        ; dependencies = dependencies
        ; source = source
        }
    | Failure(errorMsg, err, _) ->
        printfn "Failure: %s" errorMsg
        None
