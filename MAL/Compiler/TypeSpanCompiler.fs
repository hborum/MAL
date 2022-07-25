namespace itu.dk.MAL

open Analysis
open SyntaxUtil
open Monads
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis
open AST
open ToIntermediate
open FromIntermediate
open Microsoft.CodeAnalysis.CSharp.Syntax

module TypeSpanCompiler =
  let addMembersNS members (node : NamespaceDeclarationSyntax) = node.AddMembers members
  let addMembersNSM members = modifyStateRS <| addMembersNS [|members|]

  let genTypeSpans subTypes (analyser: Analyser<'eA>) =
    let allTypeSpans =
      List.filter
        (fun sb ->
          let sb = List.sort sb
          ((sb = subTypes) || List.length sb <> 1 && Set.contains sb analyser.UnionTags)
          //&& sb <> ["ThreeStateResult"] && sb <> ["OneStateResult"] // We want to create singleton TypeSpans and used unions
        ) <| GeneralUtil.powerset subTypes

    let listName tag = sprintf "%s_list" tag


    let mapper (typeSpanUnion : string list) =
      compileBuilder {
        let genName tags = genTypString analyser.DataStructure.subTypes (TRec tags) { typeIdentifierOptions with addGeneric = false } 
        let union_name = genName typeSpanUnion
        let union_type_name = genTypString analyser.DataStructure.subTypes (TRec typeSpanUnion) typeIdentifierOptions
        let typeSpanName = sprintf "TypeSpan_%s" union_name //Identical to interface decleration where we add generic

        let classNode =
          SyntaxFactory.ClassDeclaration(typeSpanName)
            .WithModifiers(SyntaxTokenList.Create(SyntaxFactory.ParseToken("public")))
            .WithBaseList(
              SyntaxFactory.BaseList(
                SyntaxFactory.SeparatedList<BaseTypeSyntax>(
                  [ SyntaxFactory.SimpleBaseType(SyntaxFactory.IdentifierName("ICountable")) :> BaseTypeSyntax
                  ; SyntaxFactory.SimpleBaseType(SyntaxFactory.ParseTypeName(sprintf "IIndexable<%s>" union_type_name)) :> BaseTypeSyntax
                  ; SyntaxFactory.SimpleBaseType(SyntaxFactory.ParseTypeName(sprintf "IUniquable<%s<TFunction>>" typeSpanName)) :> BaseTypeSyntax
                  ]
                )
              )
            )

        let classNode = classNode.AddTypeParameterListParameters(SyntaxFactory.TypeParameter("TFunction"))
        let identifier = SyntaxFactory.IdentifierName "TFunction"
        let sepList = SyntaxFactory.SeparatedList([SyntaxFactory.TypeConstraint <| SyntaxFactory.ParseTypeName "IFunction" :> TypeParameterConstraintSyntax])
        let constraintClause = SyntaxFactory.TypeParameterConstraintClause(identifier, sepList)
        let classNode = classNode.AddConstraintClauses constraintClause

        let! (analyser : Analyser<'eA>) = askRS

        let leafTypes = Seq.collect (fun tag -> ASTUtil.leafTypesOf analyser.DataStructure.subTypes tag) typeSpanUnion |> Seq.sort
        let subTypes  = Seq.collect (fun tag -> ASTUtil.subTypesOf analyser.DataStructure.subTypes tag) typeSpanUnion |> Seq.sort

        let isLeafTypeSpan = Seq.length typeSpanUnion = 1
                             && Seq.toList (ASTUtil.leafTypesOf analyser.DataStructure.subTypes (Seq.head typeSpanUnion)) = Seq.toList typeSpanUnion

        let tagArrays =
          if isLeafTypeSpan
          then Seq.map (fun tag -> genTypeIdentifier analyser (TList (convertType (nos_TRec [tag]))) false) leafTypes
          else Seq.map (fun tag -> genTypeIdentifier analyser (TTypeSpan (convertType (nos_TRec [tag]))) false) leafTypes

        let allSubTypeSpans =
             (Seq.toList subTypes)
          |> GeneralUtil.powerset
          |> List.map (List.sort)
          |> List.filter
              (fun sb ->
                (List.length sb = 1 || Set.contains sb analyser.UnionTags)
                &&
                not (
                  // We do not want to generate filters that corresponds to our original type
                  // E.g. for `Policy` we do not want to generate a filter for {OneStatePolicy, ThreeStatePolicy}
                  List.length typeSpanUnion = 1 &&
                  (
                    Map.containsKey (List.head typeSpanUnion) analyser.DataStructure.subTypes &&
                    List.forall (fun t -> List.contains t sb) analyser.DataStructure.subTypes.[List.head typeSpanUnion]
                  ) || (typeSpanUnion = sb) // Also no identiy filtering
                )
              )
        let allSubTypeSpans =
          List.map (fun tags ->
            match List.tryFind (fun (k, v) ->  List.forall (fun t -> List.contains t tags) v) (Map.toList analyser.DataStructure.subTypes) with
            | Some (k,v) -> [k]
            | None -> tags
          ) allSubTypeSpans

        // Add underlying arrays as fields
        let classNode =
          Seq.fold2 (fun (classNode' : ClassDeclarationSyntax) (tag : string) tagArrayType ->
              let sepList = SyntaxFactory.SeparatedList([SyntaxFactory.VariableDeclarator(tag)])
              let varDec = SyntaxFactory.VariableDeclaration(tagArrayType, sepList)
              classNode'.AddMembers(SyntaxFactory.FieldDeclaration(varDec).WithModifiers(
                            SyntaxFactory.TokenList(
                                [|
                                    SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword) ;
                                    SyntaxFactory.Token(SyntaxKind.PublicKeyword)|])))
            ) classNode leafTypes tagArrays

        // Add Count var and Count() method
        let classNode =
          classNode.AddMembers(
            SyntaxFactory.FieldDeclaration(
              SyntaxFactory.VariableDeclaration(
                SyntaxFactory.ParseTypeName("int"),
                SyntaxFactory.SeparatedList(
                  [SyntaxFactory.VariableDeclarator("TSCount")]
                )
              )
            ).WithModifiers(SyntaxFactory.TokenList( [| SyntaxFactory.Token(SyntaxKind.PublicKeyword) |])))
        let classNode =
          classNode.AddMembers(
              SyntaxFactory.MethodDeclaration(
                SyntaxFactory.PredefinedType(
                  SyntaxFactory.Token(SyntaxKind.IntKeyword)),
                SyntaxFactory.Identifier("Count")
              ).WithModifiers(
                SyntaxFactory.TokenList(
                  SyntaxFactory.Token(SyntaxKind.PublicKeyword))
              ).WithBody(
                  SyntaxFactory.Block(
                    SyntaxFactory.ReturnStatement(
                      SyntaxFactory.IdentifierName("TSCount")
                    ) :> StatementSyntax
                  )
                )
              )

        // Add Indexible
        let classNode =
          classNode.AddMembers(
            SyntaxFactory.MethodDeclaration(
              SyntaxFactory.IdentifierName(union_type_name),
              SyntaxFactory.Identifier("index"))
                .WithParameterList(
                  SyntaxFactory.ParameterList(
                    SyntaxFactory.SingletonSeparatedList<ParameterSyntax>(
                      SyntaxFactory.Parameter(
                       SyntaxFactory.Identifier("i")
                      ).WithType(
                        SyntaxFactory.PredefinedType(
                          SyntaxFactory.Token(SyntaxKind.IntKeyword))))))
                    .WithBody(
                  SyntaxFactory.Block(
                    SyntaxFactory.SingletonList<StatementSyntax>(
                        SyntaxFactory.ReturnStatement(
                            SyntaxFactory.ElementAccessExpression(
                                SyntaxFactory.ThisExpression()
                            ).WithArgumentList(
                               SyntaxFactory.BracketedArgumentList(
                                    SyntaxFactory.SingletonSeparatedList<ArgumentSyntax>(
                                        SyntaxFactory.Argument(
                                            SyntaxFactory.IdentifierName("i"))))))))
                ).WithModifiers(SyntaxTokenList.Create(SyntaxFactory.ParseToken("public")))
              )
        let genCount (tag : string) =
           if isLeafTypeSpan
           then
             SyntaxFactory.MemberAccessExpression(
               SyntaxKind.SimpleMemberAccessExpression,
               SyntaxFactory.IdentifierName(tag),
               SyntaxFactory.IdentifierName("Count")) :> ExpressionSyntax
           else
               SyntaxFactory.MemberAccessExpression(
                 SyntaxKind.SimpleMemberAccessExpression,
                 SyntaxFactory.IdentifierName(tag),
                 SyntaxFactory.IdentifierName("TSCount")) :> ExpressionSyntax

        // Shared count init
        let countSum =
          Seq.fold
            (fun sum (tag : string) ->
              SyntaxFactory.BinaryExpression(
                SyntaxKind.AddExpression,
                sum, genCount tag
                ) :> ExpressionSyntax
            ) (genCount (Seq.head leafTypes)) (Seq.tail leafTypes)

        let countAssign =
          SyntaxFactory.ExpressionStatement(
            SyntaxFactory.AssignmentExpression(
              SyntaxKind.SimpleAssignmentExpression,
              SyntaxFactory.IdentifierName("TSCount"),countSum
            )
          ) :> StatementSyntax

        // Add Uniqueable
        let args =
          Seq.map (
            fun (tag : string) ->
              if isLeafTypeSpan
              then
                SyntaxFactory.Argument(
                  SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                      SyntaxKind.SimpleMemberAccessExpression,
                      SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                          SyntaxKind.SimpleMemberAccessExpression,
                          SyntaxFactory.IdentifierName(tag),
                          SyntaxFactory.IdentifierName("Distinct"))),
                      SyntaxFactory.IdentifierName("ToList"))))
              else
                SyntaxFactory.Argument(
                         SyntaxFactory.InvocationExpression(
                          SyntaxFactory.MemberAccessExpression(
                             SyntaxKind.SimpleMemberAccessExpression,
                             SyntaxFactory.IdentifierName(tag),
                             SyntaxFactory.IdentifierName("unique"))))
                   ) leafTypes
        let classNode =
                 classNode.AddMembers(
                   SyntaxFactory.MethodDeclaration(
                     SyntaxFactory.GenericName(
                       SyntaxFactory.Identifier(typeSpanName)
                     ).WithTypeArgumentList(
                        SyntaxFactory.TypeArgumentList(
                          SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                            SyntaxFactory.IdentifierName("TFunction")))),
                     SyntaxFactory.Identifier("unique")
                   ).WithModifiers(
                     SyntaxFactory.TokenList(
                       SyntaxFactory.Token(SyntaxKind.PublicKeyword))
                   ).WithBody(
                      SyntaxFactory.Block(
                         SyntaxFactory.SingletonList<StatementSyntax>(
                           SyntaxFactory.ReturnStatement(
                             SyntaxFactory.ObjectCreationExpression(
                               SyntaxFactory.GenericName(
                                 SyntaxFactory.Identifier(typeSpanName)
                               ).WithTypeArgumentList(
                                 SyntaxFactory.TypeArgumentList(
                                   SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                                     SyntaxFactory.IdentifierName("TFunction"))))
                               ).WithArgumentList(
                                  SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList<ArgumentSyntax>(args))))))))

        // Easy constructor
        let constructorParam =
                 Seq.map2 (fun tag tagArrayType -> SyntaxFactory.Parameter(SyntaxFactory.Identifier(sprintf "_%s" tag)).WithType(tagArrayType)) leafTypes tagArrays
        let constructorAssign =
                 Seq.map
                   (fun (tag : string) ->
                     let lhs = SyntaxFactory.IdentifierName(tag) :> ExpressionSyntax
                     let rhs = SyntaxFactory.IdentifierName(sprintf "_%s" tag) :> ExpressionSyntax
                     let assign = SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression
                                                       , lhs
                                                       , rhs)
                     SyntaxFactory.ExpressionStatement(assign) :> StatementSyntax
                     ) leafTypes
        let constructor =
                 SyntaxFactory.ConstructorDeclaration(typeSpanName)
                   .WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                   .AddParameterListParameters(Seq.toArray constructorParam)
                   .AddBodyStatements(Seq.toArray constructorAssign)
                   .AddBodyStatements([|countAssign|])

        let classNode = classNode.AddMembers(constructor)

        // Filter constructor
        let initLists =
                 Seq.map (fun tag ->
                   let typStr = genTypString analyser.DataStructure.subTypes (convertType (nos_TRec [tag])) typeIdentifierOptions
                   let genStr = SyntaxFactory.ParseTypeName (sprintf "List<%s>" typStr)
                   let createList =
                     SyntaxFactory.LocalDeclarationStatement(
                       SyntaxFactory
                         .VariableDeclaration(genStr)
                         .WithVariables(
                           SyntaxFactory.SingletonSeparatedList<VariableDeclaratorSyntax>(
                             SyntaxFactory.VariableDeclarator(
                               SyntaxFactory.Identifier(listName tag)
                           ).WithInitializer
                             (
                             SyntaxFactory.EqualsValueClause
                               ( SyntaxFactory.ObjectCreationExpression(
                                   SyntaxFactory.IdentifierName(sprintf "List<%s>" typStr)
                                 ).WithArgumentList(SyntaxFactory.ArgumentList())
                               )
                             )
                           )
                         )
                       ) :> StatementSyntax
                   createList
                 ) leafTypes
        let checkAssign =
                 Seq.map (fun (tag : string) ->
                   let typ =  genTypeIdentifier analyser (convertType (nos_TRec [tag])) false
                   let x = NameGenerator.wishName "a"
                   SyntaxFactory.IfStatement(
                     SyntaxFactory.IsPatternExpression(
                         SyntaxFactory.IdentifierName("elem"),
                         SyntaxFactory.DeclarationPattern(typ,
                             SyntaxFactory.SingleVariableDesignation(
                               SyntaxFactory.Identifier(x)))
                     ),
                     SyntaxFactory.ExpressionStatement(
                       SyntaxFactory.InvocationExpression(
                         SyntaxFactory.MemberAccessExpression(
                           SyntaxKind.SimpleMemberAccessExpression,
                           SyntaxFactory.IdentifierName(listName tag),
                           SyntaxFactory.IdentifierName("Add")))
                         .WithArgumentList(
                           SyntaxFactory.ArgumentList(
                             SyntaxFactory.SingletonSeparatedList<ArgumentSyntax>(
                               SyntaxFactory.Argument(
                                 SyntaxFactory.IdentifierName(x))))))) :> StatementSyntax
                   ) leafTypes
        let assignLists =
                 Seq.map (fun (tag : string) ->
                   if isLeafTypeSpan
                   then
                     SyntaxFactory.ExpressionStatement(
                       SyntaxFactory.AssignmentExpression(
                         SyntaxKind.SimpleAssignmentExpression,
                           SyntaxFactory.IdentifierName(tag),
                               SyntaxFactory.IdentifierName(listName tag)
                       )) :> StatementSyntax
                   else
                     SyntaxFactory.ExpressionStatement(
                       SyntaxFactory.AssignmentExpression(
                         SyntaxKind.SimpleAssignmentExpression,
                           SyntaxFactory.IdentifierName(tag),
                           SyntaxFactory.ObjectCreationExpression(
                             SyntaxFactory.GenericName(
                                 SyntaxFactory.Identifier(sprintf "TypeSpan_%s" (genName [tag]))
                             ).WithTypeArgumentList(
                                 SyntaxFactory.TypeArgumentList(
                                     SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                                         SyntaxFactory.IdentifierName("TFunction"))))
                         ).WithArgumentList(
                             SyntaxFactory.ArgumentList(
                                 SyntaxFactory.SingletonSeparatedList<ArgumentSyntax>(
                                     SyntaxFactory.Argument(
                                         SyntaxFactory.IdentifierName(listName tag))))))
                       ) :> StatementSyntax
                 ) leafTypes
        let forStmt =
                 SyntaxFactory.ForEachStatement(
                   SyntaxFactory.IdentifierName(
                       SyntaxFactory.Identifier(
                           SyntaxFactory.TriviaList(),
                           SyntaxKind.VarKeyword,
                           "var",
                           "var",
                           SyntaxFactory.TriviaList())),
                   SyntaxFactory.Identifier("elem"),
                   SyntaxFactory.IdentifierName("elems"),
                   SyntaxFactory.Block(checkAssign)
                   ) :> StatementSyntax
        let constructor =
                 SyntaxFactory.ConstructorDeclaration(typeSpanName)
                   .WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                   .AddParameterListParameters([| SyntaxFactory.Parameter(SyntaxFactory.Identifier("elems")).WithType(SyntaxFactory.ParseTypeName("IEnumerable<object>")) |])
                   .AddBodyStatements(Seq.toArray initLists)
                   .AddBodyStatements([|forStmt|])
                   .AddBodyStatements(Seq.toArray assignLists)
                   .AddBodyStatements([|countAssign|])
        let classNode = classNode.AddMembers(constructor)

        // CreateIndexer
        //let throwError =
        //         SyntaxFactory.ThrowStatement(
        //           SyntaxFactory.ObjectCreationExpression(
        //             SyntaxFactory.IdentifierName("Exception")
        //           ).WithArgumentList(
        //             SyntaxFactory.ArgumentList(
        //               SyntaxFactory.SingletonSeparatedList<ArgumentSyntax>(
        //                 SyntaxFactory.Argument(
        //                   SyntaxFactory.LiteralExpression(
        //                     SyntaxKind.StringLiteralExpression,
        //                     SyntaxFactory.Literal("Index out of TypeSpan bounds"))))))) :> StatementSyntax

        let ifs,_ =
          Seq.fold(fun (ifs,bound) (tag : string) ->
            let index =
              match bound with
              | None -> SyntaxFactory.IdentifierName("i") :> ExpressionSyntax
              | Some bound ->
                 SyntaxFactory.BinaryExpression(SyntaxKind.SubtractExpression, SyntaxFactory.IdentifierName("i"), SyntaxFactory.ParenthesizedExpression(bound)) :> ExpressionSyntax

            let bound' =
              match bound with
              | None ->
                  if isLeafTypeSpan
                  then
                    SyntaxFactory.MemberAccessExpression(
                      SyntaxKind.SimpleMemberAccessExpression,
                      SyntaxFactory.IdentifierName(tag),
                      SyntaxFactory.IdentifierName("Count")) :> ExpressionSyntax
                  else
                    SyntaxFactory.MemberAccessExpression(
                      SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName(tag),
                        SyntaxFactory.IdentifierName("TSCount")) :> ExpressionSyntax
              | Some bound ->
                SyntaxFactory.BinaryExpression(
                  SyntaxKind.AddExpression,
                  bound,
                  if isLeafTypeSpan
                  then
                    SyntaxFactory.MemberAccessExpression(
                      SyntaxKind.SimpleMemberAccessExpression,
                      SyntaxFactory.IdentifierName(tag),
                      SyntaxFactory.IdentifierName("Count")) :> ExpressionSyntax
                  else
                    SyntaxFactory.MemberAccessExpression(
                      SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName(tag),
                        SyntaxFactory.IdentifierName("TSCount")) :> ExpressionSyntax
                ) :> ExpressionSyntax

            let localIf =
              if isLeafTypeSpan then
                  SyntaxFactory.ReturnStatement(
                                   SyntaxFactory.ElementAccessExpression(
                                     SyntaxFactory.IdentifierName(tag)
                                   ).WithArgumentList(
                                     SyntaxFactory.BracketedArgumentList(
                                       SyntaxFactory.SingletonSeparatedList<ArgumentSyntax>(
                                         SyntaxFactory.Argument(index))))
                                  ) :> StatementSyntax
              else
                if Seq.last leafTypes = tag
                then
                  SyntaxFactory.ReturnStatement(
                    SyntaxFactory.ElementAccessExpression(
                      SyntaxFactory.IdentifierName(tag)
                    ).WithArgumentList(
                      SyntaxFactory.BracketedArgumentList(
                        SyntaxFactory.SingletonSeparatedList<ArgumentSyntax>(
                          SyntaxFactory.Argument(index))))
                   ) :> StatementSyntax
                else
                  SyntaxFactory.IfStatement(
                    SyntaxFactory.BinaryExpression(
                      SyntaxKind.LessThanExpression,
                      SyntaxFactory.IdentifierName("i"),
                      bound'
                      ),
                    SyntaxFactory.ReturnStatement(
                      SyntaxFactory.ElementAccessExpression(
                        SyntaxFactory.IdentifierName(tag)
                      ).WithArgumentList(
                        SyntaxFactory.BracketedArgumentList(
                          SyntaxFactory.SingletonSeparatedList<ArgumentSyntax>(
                            SyntaxFactory.Argument(index))))
                     )
                  ) :> StatementSyntax
            (localIf :: ifs, Some bound')
          ) ([],None) leafTypes
        let ifs = Seq.rev (ifs)
        let a =
          SyntaxFactory.AccessorDeclaration(
            SyntaxKind.GetAccessorDeclaration
          ).WithBody(
            SyntaxFactory.Block(
              SyntaxList(ifs)
            )
          )
        let indexer =
          SyntaxFactory.IndexerDeclaration(
            SyntaxFactory.IdentifierName(union_type_name)
          ).WithModifiers(
            SyntaxFactory.TokenList(
              SyntaxFactory.Token(SyntaxKind.PublicKeyword))
          ).WithParameterList(
            SyntaxFactory.BracketedParameterList(
              SyntaxFactory.SingletonSeparatedList<ParameterSyntax>(
                SyntaxFactory.Parameter(
                  SyntaxFactory.Identifier("i")
                ).WithType(
                  SyntaxFactory.PredefinedType(
                    SyntaxFactory.Token(SyntaxKind.IntKeyword)))))
          ).WithAccessorList(
            SyntaxFactory.AccessorList(
              SyntaxList(a)
            )
          ) :> MemberDeclarationSyntax
        let classNode = classNode.AddMembers(indexer)

        let filtersCache =
          allSubTypeSpans
            |> List.map List.sort
            |> Set.ofList |> Set.filter
                (fun tags ->
                 let leafs = Seq.collect (fun tag -> ASTUtil.leafTypesOf analyser.DataStructure.subTypes tag) tags
                 let leafs = Seq.sort leafs
                 not (Seq.length tags = 1 && Seq.length leafs = 1 && Seq.head leafs = List.head tags))
            |> Set.toArray // Hacky way to move redundant filters
            |> (Array.map
                  (fun tags ->
                    let union_name = genName tags
                    let cacheType = SyntaxFactory.ParseTypeName <|sprintf "TypeSpan_%s<TFunction>" union_name
                    let cacheName = sprintf "cache_%s" union_name
                    SyntaxFactory.FieldDeclaration(
                      SyntaxFactory.VariableDeclaration(cacheType
                        ).WithVariables(
                            SyntaxFactory.SingletonSeparatedList<VariableDeclaratorSyntax>(
                                SyntaxFactory.VariableDeclarator(
                                    SyntaxFactory.Identifier(cacheName))))):> MemberDeclarationSyntax
                  )
               )
        // Add subType_filters
        let filters =
          allSubTypeSpans
            |> List.map List.sort
            |> Set.ofList |> Set.toArray // Hacky way to move redundant filters
            |> (Array.map
                  (fun tags ->
                    let union_name = genName tags
                    let cacheName = sprintf "cache_%s" union_name
                    let returnType = SyntaxFactory.ParseTypeName <|sprintf "TypeSpan_%s<TFunction>" union_name
                    let leafs = Seq.collect (fun tag -> ASTUtil.leafTypesOf analyser.DataStructure.subTypes tag) tags
                    let leafs = Seq.sort leafs
                    let args = Seq.map (fun (t : string) -> SyntaxFactory.Argument(SyntaxFactory.IdentifierName(t)) ) leafs
                    SyntaxFactory.MethodDeclaration(
                      returnType,
                      SyntaxFactory.Identifier(sprintf "Filter_%s" union_name)
                      ).WithBody(
                        SyntaxFactory.Block(
                          SyntaxFactory.SingletonList<StatementSyntax>(
                            if Seq.length tags = 1 && Seq.length leafs = 1 && Seq.head leafs = List.head tags
                            then
                              SyntaxFactory.ReturnStatement(
                                  SyntaxFactory.IdentifierName(Seq.head leafs) :> ExpressionSyntax
                                  ) :> StatementSyntax
                            else
                              SyntaxFactory.Block(
                                  [|
                                    SyntaxFactory.IfStatement(
                                      SyntaxFactory.BinaryExpression(
                                          SyntaxKind.NotEqualsExpression,
                                          SyntaxFactory.IdentifierName(cacheName),
                                          SyntaxFactory.LiteralExpression(
                                              SyntaxKind.NullLiteralExpression)),
                                          SyntaxFactory.ReturnStatement(
                                              SyntaxFactory.IdentifierName(cacheName)
                                            )
                                          ) :> StatementSyntax ;
                                          SyntaxFactory.ExpressionStatement(
                                              SyntaxFactory.AssignmentExpression(
                                                  SyntaxKind.SimpleAssignmentExpression,
                                                  SyntaxFactory.IdentifierName(cacheName),
                                                  SyntaxFactory.ObjectCreationExpression(returnType)
                                                        .WithArgumentList(SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList<ArgumentSyntax>(args))) :> ExpressionSyntax)) :> StatementSyntax ;
                                          SyntaxFactory.ReturnStatement(
                                            SyntaxFactory.IdentifierName(cacheName)) :> StatementSyntax
                                  |]) :> StatementSyntax
                            )
                        )
                      ).WithModifiers(SyntaxTokenList.Create(SyntaxFactory.ParseToken("public"))) :> MemberDeclarationSyntax
                    )
                )

        let classNode = classNode.AddMembers(filtersCache)
        let classNode = classNode.AddMembers(filters)

        do! addMembersNSM classNode
        return ()
      }
    mapRS mapper allTypeSpans

