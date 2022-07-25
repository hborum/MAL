module DataflowData

open itu.dk.MAL.ToIntermediate

let largeInit =
  [Assign((TRec ["ExecutorState"], Var "state"),(TRec ["ExecutorState"], Var "_state"));
 Block
 [
  //[Block
  //   [Let
  //      ("tempname_21",
  //       (TList (TRec ["Risk"],None), Init (TList (TRec ["Risk"],None),[])),
  //       false);
  //    For
  //      ("tempname_22",
  //       (TList (TRec ["Group"],None),
  //        Access ((TRec ["ExecutorState"], Var "state"),"Groups")),
  //       Match
  //         ((TRec ["Group"], Var "tempname_22"),
  //          [(TRec ["Interest"], "tempname_23", Block []);
  //           (TRec ["Risk"], "tempname_24",
  //            Expression
  //              (TUnit,
  //               Call
  //                 (Other,
  //                  (TUnit,
  //                   Access
  //                     ((TList (TRec ["Risk"],None), Var "tempname_21"),"Add")),
  //                  [(TRec ["Risk"], Var "tempname_24")])));
  //           (TRec ["Expense"], "tempname_25", Block [])]));
  //    For
  //      ("___ent",(TList (TRec ["Risk"],None), Var "tempname_21"),
  //       Block
  //         [Expression
  //            (TUnit,
  //             Call
  //               (Other,
  //                (TBool,
  //                 Access
  //                   ((TBool,
  //                     Access
  //                       ((TRec ["ExecutorState"], Var "state"),
  //                        "out_Risk_RiskDividend")),"Add")),
  //                [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //                 (TBool,
  //                  Call
  //                    (Other,
  //                     (TBool,
  //                      Access
  //                        ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                         "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]))])];
  // For
  //   ("___ent",
  //    (TList (TRec ["Policy"],None),
  //     Access ((TRec ["ExecutorState"], Var "state"),"Policies")),
  //    Block
  //      [Expression
  //         (TUnit,
  //          Call
  //            (Other,
  //             (TBool,
  //              Access
  //                ((TBool,
  //                  Access
  //                    ((TRec ["ExecutorState"], Var "state"),
  //                     "out_Policy_ScaledCFTechReserve")),"Add")),
  //             [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //              (TBool,
  //               Call
  //                 (Other,
  //                  (TBool,
  //                   Access
  //                     ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                      "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //       Expression
  //         (TUnit,
  //          Call
  //            (Other,
  //             (TBool,
  //              Access
  //                ((TBool,
  //                  Access
  //                    ((TRec ["ExecutorState"], Var "state"),
  //                     "out_Policy_PolicyReserve")),"Add")),
  //             [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //              (TBool,
  //               Call
  //                 (Other,
  //                  (TBool,
  //                   Access
  //                     ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                      "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //       Expression
  //         (TUnit,
  //          Call
  //            (Other,
  //             (TBool,
  //              Access
  //                ((TBool,
  //                  Access
  //                    ((TRec ["ExecutorState"], Var "state"),
  //                     "out_Policy_CFScalingFactor_Scaled")),"Add")),
  //             [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //              (TBool,
  //               Call
  //                 (Other,
  //                  (TBool,
  //                   Access
  //                     ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                      "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //       Expression
  //         (TUnit,
  //          Call
  //            (Other,
  //             (TBool,
  //              Access
  //                ((TBool,
  //                  Access
  //                    ((TRec ["ExecutorState"], Var "state"),
  //                     "out_Policy_BenefitsLessPremiumsReserves")),"Add")),
  //             [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //              (TBool,
  //               Call
  //                 (Other,
  //                  (TBool,
  //                   Access
  //                     ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                      "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //       Expression
  //         (TUnit,
  //          Call
  //            (Other,
  //             (TBool,
  //              Access
  //                ((TBool,
  //                  Access
  //                    ((TRec ["ExecutorState"], Var "state"),
  //                     "out_Policy_BenefitsLessPremiums")),"Add")),
  //             [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //              (TBool,
  //               Call
  //                 (Other,
  //                  (TBool,
  //                   Access
  //                     ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                      "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]))]);
  // Block
  //   [Let
  //      ("tempname_11",
  //       (TList (TRec ["Interest"],None),
  //        Init (TList (TRec ["Interest"],None),[])),false);
  //    For
  //      ("tempname_12",
  //       (TList (TRec ["Group"],None),
  //        Access ((TRec ["ExecutorState"], Var "state"),"Groups")),
  //       Match
  //         ((TRec ["Group"], Var "tempname_12"),
  //          [(TRec ["Interest"], "tempname_13",
  //            Expression
  //              (TUnit,
  //               Call
  //                 (Other,
  //                  (TUnit,
  //                   Access
  //                     ((TList (TRec ["Interest"],None), Var "tempname_11"),
  //                      "Add")),[(TRec ["Interest"], Var "tempname_13")])));
  //           (TRec ["Risk"], "tempname_14", Block []);
  //           (TRec ["Expense"], "tempname_15", Block [])]));
  //    For
  //      ("___ent",(TList (TRec ["Interest"],None), Var "tempname_11"),
  //       Block
  //         [Expression
  //            (TUnit,
  //             Call
  //               (Other,
  //                (TBool,
  //                 Access
  //                   ((TBool,
  //                     Access
  //                       ((TRec ["ExecutorState"], Var "state"),
  //                        "out_Interest_RiskMargin")),"Add")),
  //                [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //                 (TBool,
  //                  Call
  //                    (Other,
  //                     (TBool,
  //                      Access
  //                        ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                         "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //          Expression
  //            (TUnit,
  //             Call
  //               (Other,
  //                (TBool,
  //                 Access
  //                   ((TBool,
  //                     Access
  //                       ((TRec ["ExecutorState"], Var "state"),
  //                        "out_Interest_Reimbursement")),"Add")),
  //                [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //                 (TBool,
  //                  Call
  //                    (Other,
  //                     (TBool,
  //                      Access
  //                        ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                         "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //          Expression
  //            (TUnit,
  //             Call
  //               (Other,
  //                (TBool,
  //                 Access
  //                   ((TBool,
  //                     Access
  //                       ((TRec ["ExecutorState"], Var "state"),
  //                        "out_Interest_ProfitMargin")),"Add")),
  //                [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //                 (TBool,
  //                  Call
  //                    (Other,
  //                     (TBool,
  //                      Access
  //                        ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                         "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //          Expression
  //            (TUnit,
  //             Call
  //               (Other,
  //                (TBool,
  //                 Access
  //                   ((TBool,
  //                     Access
  //                       ((TRec ["ExecutorState"], Var "state"),
  //                        "out_Interest_IndividualBonusPotential")),"Add")),
  //                [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //                 (TBool,
  //                  Call
  //                    (Other,
  //                     (TBool,
  //                      Access
  //                        ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                         "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //          Expression
  //            (TUnit,
  //             Call
  //               (Other,
  //                (TBool,
  //                 Access
  //                   ((TBool,
  //                     Access
  //                       ((TRec ["ExecutorState"], Var "state"),
  //                        "out_Interest_GY")),"Add")),
  //                [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //                 (TBool,
  //                  Call
  //                    (Other,
  //                     (TBool,
  //                      Access
  //                        ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                         "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //          Expression
  //            (TUnit,
  //             Call
  //               (Other,
  //                (TBool,
  //                 Access
  //                   ((TBool,
  //                     Access
  //                       ((TRec ["ExecutorState"], Var "state"),
  //                        "out_Interest_DepositRate")),"Add")),
  //                [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //                 (TBool,
  //                  Call
  //                    (Other,
  //                     (TBool,
  //                      Access
  //                        ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                         "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //          Expression
  //            (TUnit,
  //             Call
  //               (Other,
  //                (TBool,
  //                 Access
  //                   ((TBool,
  //                     Access
  //                       ((TRec ["ExecutorState"], Var "state"),
  //                        "out_Interest_CollectiveBonusPotential")),"Add")),
  //                [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //                 (TBool,
  //                  Call
  //                    (Other,
  //                     (TBool,
  //                      Access
  //                        ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                         "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]))])];
  // For
  //   ("___ent",
  //    (TList (TRec ["Group"],None),
  //     Access ((TRec ["ExecutorState"], Var "state"),"Groups")),
  //    Block
  //      [Expression
  //         (TUnit,
  //          Call
  //            (Other,
  //             (TBool,
  //              Access
  //                ((TBool,
  //                  Access
  //                    ((TRec ["ExecutorState"], Var "state"),
  //                     "out_Group_TransferToCapitalBaseReserves")),"Add")),
  //             [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //              (TBool,
  //               Call
  //                 (Other,
  //                  (TBool,
  //                   Access
  //                     ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                      "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //       Expression
  //         (TUnit,
  //          Call
  //            (Other,
  //             (TBool,
  //              Access
  //                ((TBool,
  //                  Access
  //                    ((TRec ["ExecutorState"], Var "state"),
  //                     "out_Group_TransferToCapitalBase")),"Add")),
  //             [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //              (TBool,
  //               Call
  //                 (Other,
  //                  (TBool,
  //                   Access
  //                     ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                      "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //       Expression
  //         (TUnit,
  //          Call
  //            (Other,
  //             (TBool,
  //              Access
  //                ((TBool,
  //                  Access
  //                    ((TRec ["ExecutorState"], Var "state"),
  //                     "out_Group_TransferFromCapitalBaseReserves")),"Add")),
  //             [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //              (TBool,
  //               Call
  //                 (Other,
  //                  (TBool,
  //                   Access
  //                     ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                      "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //       Expression
  //         (TUnit,
  //          Call
  //            (Other,
  //             (TBool,
  //              Access
  //                ((TBool,
  //                  Access
  //                    ((TRec ["ExecutorState"], Var "state"),
  //                     "out_Group_TransferFromCapitalBase")),"Add")),
  //             [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //              (TBool,
  //               Call
  //                 (Other,
  //                  (TBool,
  //                   Access
  //                     ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                      "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
  //       Expression
  //         (TUnit,
  //          Call
  //            (Other,
  //             (TBool,
  //              Access
  //                ((TBool,
  //                  Access
  //                    ((TRec ["ExecutorState"], Var "state"),
  //                     "out_Group_GroupReserve")),"Add")),
  //             [(TBool, Access ((TBool, Var "___ent"),"Name"));
  //              (TBool,
  //               Call
  //                 (Other,
  //                  (TBool,
  //                   Access
  //                     ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
  //                      "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]))]);
   Block
     [Let
        ("___ent",
         (TRec ["Global"],
          Access ((TRec ["ExecutorState"], Var "state"),"Global")),false);
      Block
        [Assign
           ((TUnit,
             Access
               ((TRec ["ExecutorState"], Var "state"),
                "out_Global_TechnicalReserves")),
            (TBool,
             Call
               (Other,
                (TBool,
                 Access
                   ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),"___NewArr")),
                [(TDouble, Var "___NumberOfProjs")])))]];
   Block
     [Let
        ("tempname_1",
         (TArray (TRec ["Expense"]),
          Init (TArray (TRec ["Expense"]),[])),false);
      For
        ("tempname_2",
         ["iterName_2",
          (TArray (TRec ["Group"]),
           Access ((TRec ["ExecutorState"], Var "state"),"Groups"))
         ],
         Match
           ((TRec ["Group"], Var "tempname_2"),
            [(TRec ["Interest"], "tempname_3", Block []);
             (TRec ["Risk"], "tempname_4", Block []);
             (TRec ["Expense"], "tempname_5",
              Expression
                (TUnit,
                 Call
                   (Other,
                    (TUnit,
                     Access
                       ((TArray (TRec ["Expense"]), Var "tempname_1"),"Add")),
                    [(TRec ["Expense"], Var "tempname_5")])))]), None);
      For
        ("___ent", ["iterName_2",(TArray (TRec ["Expense"]), Var "tempname_1")],
         Block
           [Expression
              (TUnit,
               Call
                 (Other,
                  (TBool,
                   Access
                     ((TBool,
                       Access
                         ((TRec ["ExecutorState"], Var "state"),
                          "out_Expense_ResExpenseDividend")),"Add")),
                  [(TBool, Access ((TBool, Var "___ent"),"Name"));
                   (TBool,
                    Call
                      (Other,
                       (TBool,
                        Access
                          ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
                           "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
            Expression
              (TUnit,
               Call
                 (Other,
                  (TBool,
                   Access
                     ((TBool,
                       Access
                         ((TRec ["ExecutorState"], Var "state"),
                          "out_Expense_CFExpenseDividend")),"Add")),
                  [(TBool, Access ((TBool, Var "___ent"),"Name"));
                   (TBool,
                    Call
                      (Other,
                       (TBool,
                        Access
                          ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
                           "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]))], None)]]]

let troublesomeLoop =
  For
    ("___ent",["iterName_2",(TArray (TRec ["Expense"]), Var "tempname_1")],
     Block
       [Expression
          (TUnit,
           Call
             (Other,
              (TBool,
               Access
                 ((TBool,
                   Access
                     ((TRec ["ExecutorState"], Var "state"),
                      "out_Expense_ResExpenseDividend")),"Add")),
              [(TBool, Access ((TBool, Var "___ent"),"Name"));
               (TBool,
                Call
                  (Other,
                   (TBool,
                    Access
                      ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
                       "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]));
        Expression
          (TUnit,
           Call
             (Other,
              (TBool,
               Access
                 ((TBool,
                   Access
                     ((TRec ["ExecutorState"], Var "state"),
                      "out_Expense_CFExpenseDividend")),"Add")),
              [(TBool, Access ((TBool, Var "___ent"),"Name"));
               (TBool,
                Call
                  (Other,
                   (TBool,
                    Access
                      ((TRec ["BuiltIn"], Var "BuiltIns<TFunction>"),
                       "___NewArr")),[(TDouble, Var "___NumberOfProjs")]))]))], None)