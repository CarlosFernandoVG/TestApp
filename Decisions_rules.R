#TTest-------------------------------------------------------------------------------------------------------
TTestHyp <- conditionalPanel(condition = "input.ParametricTest == 'T-test'",
                             conditionalPanel(condition = "input.TTestKind == '1 muestra'",
                                              conditionalPanel(condition = "input.TTestKindOfTest1 == 'two.sided'",
                                                               align = "center",
                                                               "$T\\leq t_1\\ o\\ T\\geq t_2$",
                                                               "|",
                                                               "$|T|\\geq t$"
                                              ),
                                              conditionalPanel(condition = "input.TTestKindOfTest1 == 'less'",
                                                               align = "center",
                                                               "$T\\leq t$"),
                                              conditionalPanel(condition = "input.TTestKindOfTest1 == 'greater'",
                                                               align = "center",
                                                               "$T\\geq t$")
                             ),
                             conditionalPanel(condition = "input.TTestKind == '2 muestras'",
                                              conditionalPanel(condition = "input.TTestKindOfTest2 == 'two.sided'",
                                                               align = "center",
                                                               "$T\\leq t_1\\ o\\ T\\geq t_2$",
                                                               "|",
                                                               "$|T|\\geq t$"
                                              ),
                                              conditionalPanel(condition = "input.TTestKindOfTest2 == 'less'",
                                                               align = "center",
                                                               "$T\\leq t$"),
                                              conditionalPanel(condition = "input.TTestKindOfTest2 == 'greater'",
                                                               align = "center",
                                                               "$T\\geq t$")
                             ))
#ZTest-------------------------------------------------------------------------------------------------------
ZTestHyp <- conditionalPanel(condition = "input.ParametricTest == 'Z-test'",
                             conditionalPanel(condition = "input.ZTestKind == '1 muestra'",
                                              conditionalPanel(condition = "input.ZTest1Input == 'Datos'",
                                                               conditionalPanel(condition = "input.ZTestKindOfTest1 == 'two.sided'",
                                                                                align = "center",
                                                                                "$T\\leq t_1\\ o\\ T\\geq t_2$",
                                                                                "|",
                                                                                "$|T|\\geq t$"
                                                               ),
                                                               conditionalPanel(condition = "input.ZTestKindOfTest1 == 'less'",
                                                                                align = "center",
                                                                                "$T\\leq t$"),
                                                               conditionalPanel(condition = "input.ZTestKindOfTest1 == 'greater'",
                                                                                align = "center",
                                                                                "$T\\geq t$")
                                              ),
                                              conditionalPanel(condition = "input.ZTest1Input == 'Manual'",
                                                               conditionalPanel(condition = "input.ZTestKindOfTest1M == 'two.sided'",
                                                                                align = "center",
                                                                                "$T\\leq t_1\\ o\\ T\\geq t_2$",
                                                                                "|",
                                                                                "$|T|\\geq t$"
                                                               ),
                                                               conditionalPanel(condition = "input.ZTestKindOfTest1M == 'less'",
                                                                                align = "center",
                                                                                "$T\\leq t$"),
                                                               conditionalPanel(condition = "input.ZTestKindOfTest1M == 'greater'",
                                                                                align = "center",
                                                                                "$T\\geq t$")
                                              )
                             ),
                             conditionalPanel(condition = "input.ZTestKind == '2 muestras'",
                                              conditionalPanel(condition = "input.ZTest2Input == 'Datos'",
                                                               conditionalPanel(condition = "input.ZTestKindOfTest2 == 'two.sided'",
                                                                                align = "center",
                                                                                "$T\\leq t_1\\ o\\ T\\geq t_2$",
                                                                                "|",
                                                                                "$|T|\\geq t$"
                                                               ),
                                                               conditionalPanel(condition = "input.ZTestKindOfTest2 == 'less'",
                                                                                align = "center",
                                                                                "$T\\leq t$"),
                                                               conditionalPanel(condition = "input.ZTestKindOfTest2 == 'greater'",
                                                                                align = "center",
                                                                                "$T\\geq t$")
                                              ),
                                              conditionalPanel(condition = "input.ZTest2Input == 'Manual'",
                                                               conditionalPanel(condition = "input.ZTestKindOfTest2M == 'two.sided'",
                                                                                align = "center",
                                                                                "$T\\leq t_1\\ o\\ T\\geq t_2$",
                                                                                "|",
                                                                                "$|T|\\geq t$"
                                                               ),
                                                               conditionalPanel(condition = "input.ZTestKindOfTest2M == 'less'",
                                                                                align = "center",
                                                                                "$T\\leq t$"),
                                                               conditionalPanel(condition = "input.ZTestKindOfTest2M == 'greater'",
                                                                                align = "center",
                                                                                "$T\\geq t$")
                                              )
                             )
)
#ShapiroTest-------------------------------------------------------------------------------------------------------
ShapiroTestHyp <- conditionalPanel(condition = "input.ParametricTest == 'Shapiro-Wilks'",
                                   "$T<t$")
#BinomialTest-------------------------------------------------------------------------------------------------------
BinomialTestHyp <- conditionalPanel(condition = "input.NParametricTest == 'Binomial'",
                                    conditionalPanel(condition = "input.BinomialTest == 'Proporciones'",
                                                     conditionalPanel(condition = "input.BinomialTestInput == 'Manual' & !input.BTPNormal",
                                                                      conditionalPanel(condition = "input.BinomialTestKindOfTestM == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T\\leq t_1\\ o\\ T>t_2$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.BinomialTestKindOfTestM == 'less'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$"),
                                                                      conditionalPanel(condition = "input.BinomialTestKindOfTestM == 'greater'",
                                                                                       align = "center",
                                                                                       "$T>t$")
                                                     ),
                                                     conditionalPanel(condition = "input.BinomialTestInput == 'Manual' & input.BTPNormal",
                                                                      conditionalPanel(condition = "input.BinomialTestKindOfTestM == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T\\geq t$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.BinomialTestKindOfTestM == 'less'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$"),
                                                                      conditionalPanel(condition = "input.BinomialTestKindOfTestM == 'greater'",
                                                                                       align = "center",
                                                                                       "$T\\geq t$")
                                                     ),
                                                     conditionalPanel(condition = "input.BinomialTestInput == 'Datos' & !input.BTPNormal",
                                                                      conditionalPanel(condition = "input.BinomialTestKindOfTestD == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T\\leq t_1\\ o\\ T>t_2$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.BinomialTestKindOfTestD == 'less'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$"),
                                                                      conditionalPanel(condition = "input.BinomialTestKindOfTestD == 'greater'",
                                                                                       align = "center",
                                                                                       "$T>t$")
                                                     ),
                                                     conditionalPanel(condition = "input.BinomialTestInput == 'Datos' & input.BTPNormal",
                                                                      conditionalPanel(condition = "input.BinomialTestKindOfTestD == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T\\geq t$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.BinomialTestKindOfTestD == 'less'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$"),
                                                                      conditionalPanel(condition = "input.BinomialTestKindOfTestD == 'greater'",
                                                                                       align = "center",
                                                                                       "$T\\geq t$")
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.BinomialTest == 'Cuantiles'",
                                                     conditionalPanel(condition = "input.CuantilTestInput == 'Manual' & !input.CuanTPNormal",
                                                                      conditionalPanel(condition = "input.CuantilTestKindOfTestM== 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T_1\\leq t_1\\ o\\ T_2>t_2$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.CuantilTestKindOfTestM == 'less'",
                                                                                       align = "center",
                                                                                       "$T_1\\leq t_1$"),
                                                                      conditionalPanel(condition = "input.CuantilTestKindOfTestM == 'greater'",
                                                                                       align = "center",
                                                                                       "$T_2> t_2$")
                                                     ),
                                                     conditionalPanel(condition = "input.CuantilTestInput == 'Manual' & input.CuanTPNormal",
                                                                      conditionalPanel(condition = "input.CuantilTestKindOfTestM == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T_1\\leq t_1\\ o\\ T_2\\geq t_2$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.CuantilTestKindOfTestM == 'less'",
                                                                                       align = "center",
                                                                                       "$T_1\\leq t_1$"),
                                                                      conditionalPanel(condition = "input.CuantilTestKindOfTestM == 'greater'",
                                                                                       align = "center",
                                                                                       "$T_2\\geq t_2$")
                                                     ),
                                                     conditionalPanel(condition = "input.CuantilTestInput == 'Datos' & !input.CuanTPNormal",
                                                                      conditionalPanel(condition = "input.CuantilTestKindOfTestD == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T_1\\leq t_1\\ o\\ T_2>t_2$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.CuantilTestKindOfTestD == 'less'",
                                                                                       align = "center",
                                                                                       "$T_1\\leq t_1$"),
                                                                      conditionalPanel(condition = "input.CuantilTestKindOfTestD == 'greater'",
                                                                                       align = "center",
                                                                                       "$T_2> t_2$")
                                                     ),
                                                     conditionalPanel(condition = "input.CuantilTestInput == 'Datos' & input.CuanTPNormal",
                                                                      conditionalPanel(condition = "input.CuantilTestKindOfTestD == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T_1\\leq t_1\\ o\\ T_2\\geq t_2$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.CuantilTestKindOfTestD == 'less'",
                                                                                       align = "center",
                                                                                       "$T_1\\leq t_1$"),
                                                                      conditionalPanel(condition = "input.CuantilTestKindOfTestD == 'greater'",
                                                                                       align = "center",
                                                                                       "$T_2\\geq t_2$")
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.BinomialTest == 'Signos'",
                                                     conditionalPanel(condition = "input.SignosTestInput == 'Manual' & !input.BTPNormal",
                                                                      conditionalPanel(condition = "input.SignosTestKindOfTestM == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T\\leq t\\ o\\ T>n-t$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.SignosTestKindOfTestM == 'less'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$"),
                                                                      conditionalPanel(condition = "input.SignosTestKindOfTestM == 'greater'",
                                                                                       align = "center",
                                                                                       "$T> n-t$")
                                                     ),
                                                     conditionalPanel(condition = "input.SignosTestInput == 'Manual' & input.BTPNormal",
                                                                      conditionalPanel(condition = "input.SignosTestKindOfTestM == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T\\geq t$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.SignosTestKindOfTestM == 'less'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$"),
                                                                      conditionalPanel(condition = "input.SignosTestKindOfTestM == 'greater'",
                                                                                       align = "center",
                                                                                       "$T\\geq t$")
                                                     ),
                                                     conditionalPanel(condition = "input.SignosTestInput == 'Datos' & !input.BTPNormal",
                                                                      conditionalPanel(condition = "input.SignosTestKindOfTestD == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T\\leq t\\ o\\ T>n-t$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.SignosTestKindOfTestD == 'less'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$"),
                                                                      conditionalPanel(condition = "input.SignosTestKindOfTestD == 'greater'",
                                                                                       align = "center",
                                                                                       "$T>t$")
                                                     ),
                                                     conditionalPanel(condition = "input.SignosTestInput == 'Datos' & input.BTPNormal",
                                                                      conditionalPanel(condition = "input.SignosTestKindOfTestD == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T\\geq t$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.SignosTestKindOfTestD == 'less'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$"),
                                                                      conditionalPanel(condition = "input.SignosTestKindOfTestD == 'greater'",
                                                                                       align = "center",
                                                                                       "$T\\geq t$")
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.BinomialTest == 'McNemar'",
                                                     conditionalPanel(condition = "input.MCNemarTestInput == 'Manual'",
                                                                      conditionalPanel(condition = "input.MCNemarTPCorrection & input.MCNCorrection == 'MCNCorrectionCont'",
                                                                                       align = "center",
                                                                                       "$T> t_{1-\\alpha}$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.MCNemarTPCorrection & input.MCNCorrection == 'MCNCorrectionExact'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$ o $T<n-t$"),
                                                                      conditionalPanel(condition = "input.MCNemarTPCorrection & input.MCNCorrection == 'MCNCorrectionBinom'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$ o $T<n-t$"),
                                                                      conditionalPanel(condition = "!input.MCNemarTPCorrection",
                                                                                       align = "center",
                                                                                       "$T> t_{1-\\alpha}$")
                                                     ),
                                                     conditionalPanel(condition = "input.MCNemarTestInput == 'Datos'",
                                                                      conditionalPanel(condition = "input.MCNemarTPCorrection & input.MCNCorrection == 'MCNCorrectionCont'",
                                                                                       align = "center",
                                                                                       "$T> t_{1-\\alpha}$"),
                                                                      conditionalPanel(condition = "input.MCNemarTPCorrection & input.MCNCorrection == 'MCNCorrectionExact'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$ o $T<n-t$"),
                                                                      conditionalPanel(condition = "input.MCNemarTPCorrection & input.MCNCorrection == 'MCNCorrectionBinom'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$ o $T<n-t$"),
                                                                      conditionalPanel(condition = "!input.MCNemarTPCorrection",
                                                                                       align = "center",
                                                                                       "$T> t_{1-\\alpha}$")
                                                     )
                                                     
                                    ),
                                    conditionalPanel(condition = "input.BinomialTest == 'Cox Stuart'",
                                                     conditionalPanel(condition = "input.CSTestInput == 'Manual' & !input.BTPNormal",
                                                                      conditionalPanel(condition = "input.CSTestKindOfTestM == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T\\leq t\\ o\\ T>n-t$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.CSTestKindOfTestM == 'less'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$"),
                                                                      conditionalPanel(condition = "input.CSTestKindOfTestM == 'greater'",
                                                                                       align = "center",
                                                                                       "$T> n-t$")
                                                     ),
                                                     conditionalPanel(condition = "input.CSTestInput == 'Manual' & input.BTPNormal",
                                                                      conditionalPanel(condition = "input.CSTestKindOfTestM == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T\\geq t$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.CSTestKindOfTestM == 'less'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$"),
                                                                      conditionalPanel(condition = "input.CSTestKindOfTestM == 'greater'",
                                                                                       align = "center",
                                                                                       "$T\\geq t$")
                                                     ),
                                                     conditionalPanel(condition = "input.CSTestInput == 'Datos' & !input.BTPNormal",
                                                                      conditionalPanel(condition = "input.CSTestKindOfTestD == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T\\leq t\\ o\\ T>n-t$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.CSTestKindOfTestD == 'less'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$"),
                                                                      conditionalPanel(condition = "input.CSTestKindOfTestD == 'greater'",
                                                                                       align = "center",
                                                                                       "$T>t$")
                                                     ),
                                                     conditionalPanel(condition = "input.CSTestInput == 'Datos' & input.BTPNormal",
                                                                      conditionalPanel(condition = "input.CSTestKindOfTestD == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T\\geq t$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.CSTestKindOfTestD == 'less'",
                                                                                       align = "center",
                                                                                       "$T\\leq t$"),
                                                                      conditionalPanel(condition = "input.CSTestKindOfTestD == 'greater'",
                                                                                       align = "center",
                                                                                       "$T\\geq t$")
                                                     )
                                    )
)
#RangoTest-------------------------------------------------------------------------------------------------------
RangoTestHyp <- conditionalPanel(condition = "input.NParametricTest == 'Rango'",
                                    conditionalPanel(condition = "input.RangoTest == 'U-Mann-Whitney'",
                                                     conditionalPanel(condition = "input.U_Mann_WhitneyTestInput == 'Manual'",
                                                                      conditionalPanel(condition = "input.U_Mann_WhitneyTestKindOfTestM == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T<t_1\\ o\\ T>t_2$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.U_Mann_WhitneyTestKindOfTestM == 'less'",
                                                                                       align = "center",
                                                                                       "$T< t$"),
                                                                      conditionalPanel(condition = "input.U_Mann_WhitneyTestKindOfTestM == 'greater'",
                                                                                       align = "center",
                                                                                       "$T>t$")
                                                     ),
                                                     conditionalPanel(condition = "input.U_Mann_WhitneyTestInput == 'Datos'",
                                                                      conditionalPanel(condition = "input.U_Mann_WhitneyTestKindOfTestD == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T<t_1\\ o\\ T>t_2$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.U_Mann_WhitneyTestKindOfTestD == 'less'",
                                                                                       align = "center",
                                                                                       "$T< t$"),
                                                                      conditionalPanel(condition = "input.U_Mann_WhitneyTestKindOfTestD == 'greater'",
                                                                                       align = "center",
                                                                                       "$T>t$")
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.RangoTest == 'Signed-Rank'",
                                                     conditionalPanel(condition = "input.Signed_RankTestInput == 'Manual'",
                                                                      conditionalPanel(condition = "input.Signed_RankTestKindOfTestM == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T<t_1\\ o\\ T>t_2$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.Signed_RankTestKindOfTestM == 'less'",
                                                                                       align = "center",
                                                                                       "$T< t$"),
                                                                      conditionalPanel(condition = "input.Signed_RankTestKindOfTestM == 'greater'",
                                                                                       align = "center",
                                                                                       "$T>t$")
                                                     ),
                                                     conditionalPanel(condition = "input.Signed_RankTestInput == 'Datos'",
                                                                      conditionalPanel(condition = "input.Signed_RankTestKindOfTestD == 'two.sided'",
                                                                                       align = "center",
                                                                                       "$T<t_1\\ o\\ T>t_2$"
                                                                      ),
                                                                      conditionalPanel(condition = "input.Signed_RankTestKindOfTestD == 'less'",
                                                                                       align = "center",
                                                                                       "$T< t$"),
                                                                      conditionalPanel(condition = "input.Signed_RankTestKindOfTestD == 'greater'",
                                                                                       align = "center",
                                                                                       "$T>t$")
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.RangoTest == 'Kruskal-Wallis'",
                                                     conditionalPanel(condition = "input.Kruskal_WallisTestInput == 'Manual'",
                                                                      align = "center",
                                                                      "$T>t$"
                                                     ),
                                                     conditionalPanel(condition = "input.Kruskal_WallisTestInput == 'Datos'",
                                                                      align = "center",
                                                                      "$T>t$"
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.RangoTest == 'Friedman'",
                                                     conditionalPanel(condition = "input.FriedmanTestInput == 'Manual'",
                                                                      align = "center",
                                                                      "$T>t$"
                                                     ),
                                                     conditionalPanel(condition = "input.FriedmanTestInput == 'Datos'",
                                                                      align = "center",
                                                                      "$T>t$"
                                                     )
                                                     
                                    )
)
#VarianceRankTest-------------------------------------------------------------------------------------------------------
VarianceRankTestHyp <- conditionalPanel(condition = "input.NParametricTest == 'Varianzas'",
                                 conditionalPanel(condition = "input.VarianzasTest == '2 Poblaciones'",
                                                  conditionalPanel(condition = "input.SRTFV2TestInput == 'Manual'",
                                                                   conditionalPanel(condition = "input.SRTFV2TestKindOfTestM == 'two.sided'",
                                                                                    align = "center",
                                                                                    "$T<t_1\\ o\\ T>t_2$"
                                                                   ),
                                                                   conditionalPanel(condition = "input.SRTFV2TestKindOfTestM == 'less'",
                                                                                    align = "center",
                                                                                    "$T< t$"),
                                                                   conditionalPanel(condition = "input.SRTFV2TestKindOfTestM == 'greater'",
                                                                                    align = "center",
                                                                                    "$T>t$")
                                                  ),
                                                  conditionalPanel(condition = "input.SRTFV2TestInput == 'Datos'",
                                                                   conditionalPanel(condition = "input.SRTFV2TestKindOfTestD == 'two.sided'",
                                                                                    align = "center",
                                                                                    "$T<t_1\\ o\\ T>t_2$"
                                                                   ),
                                                                   conditionalPanel(condition = "input.SRTFV2TestKindOfTestD == 'less'",
                                                                                    align = "center",
                                                                                    "$T< t$"),
                                                                   conditionalPanel(condition = "input.SRTFV2TestKindOfTestD == 'greater'",
                                                                                    align = "center",
                                                                                    "$T>t$")
                                                  )
                                 ),
                                 conditionalPanel(condition = "input.VarianzasTest == '>2 Poblaciones'",
                                                  align = "center",
                                                  "$T>t$"
                                 )
)
#ContingencyTablesTest-------------------------------------------------------------------------------------------------------
ContingencyTablesTestHyp <- conditionalPanel(condition = "input.NParametricTest == 'Tablas de Contingencia'",
                                        conditionalPanel(condition = "input.ContingencyTest == '2X2'",
                                                         conditionalPanel(condition = "input.CT2X2TestInput == 'Manual'",
                                                                          conditionalPanel(condition = "input.CT2X2TestKindOfTestM == 'two.sided'",
                                                                                           align = "center",
                                                                                           "$T<t_1\\ o\\ T>t_2$"
                                                                          ),
                                                                          conditionalPanel(condition = "input.CT2X2TestKindOfTestM == 'less'",
                                                                                           align = "center",
                                                                                           "$T< t$"),
                                                                          conditionalPanel(condition = "input.CT2X2TestKindOfTestM == 'greater'",
                                                                                           align = "center",
                                                                                           "$T>t$")
                                                         ),
                                                         conditionalPanel(condition = "input.CT2X2TestInput == 'Datos'",
                                                                          conditionalPanel(condition = "input.CT2X2TestKindOfTestD == 'two.sided'",
                                                                                           align = "center",
                                                                                           "$T<t_1\\ o\\ T>t_2$"
                                                                          ),
                                                                          conditionalPanel(condition = "input.CT2X2TestKindOfTestD == 'less'",
                                                                                           align = "center",
                                                                                           "$T< t$"),
                                                                          conditionalPanel(condition = "input.CT2X2TestKindOfTestD == 'greater'",
                                                                                           align = "center",
                                                                                           "$T>t$")
                                                         )
                                        ),
                                        conditionalPanel(condition = "input.ContingencyTest == 'Independencia'",
                                                         align = "center",
                                                         "$T>t$"
                                        ),
                                        conditionalPanel(condition = "input.ContingencyTest == 'RXC'",
                                                         align = "center",
                                                         "$T>t$"
                                        ),
                                        conditionalPanel(condition = "input.ContingencyTest == 'Mediana'",
                                                         align = "center",
                                                         "$T>t$"
                                        )
)
#Attempts----------------------------------------------------------------------------------------------------

# HypoKind <- reactiveValues(two = FALSE, right = FALSE, left = FALSE)

# output$HytestTT <- reactive({
#   isolate(HypoKind$two) == TRUE
# })
# 
# output$veamo1 <- renderText(isolate(HypoKind$two))
# output$veamo2 <- renderText(isolate(HypoKind$right))
# output$veamo3 <- renderText(isolate(HypoKind$left))
# 
# output$HytestLT <- reactive({
#   isolate(HypoKind$right) == TRUE
# })
# output$HytestRT <- reactive({
#   isolate(HypoKind$left) == TRUE
# })
# observeEvent(input$TTestKindOfTest1 == "two.sided", {
#   # if(input$TTestKindOfTest1 == "two.sided"){
#     HypoKind$two = TRUE
#     HypoKind$right = FALSE
#     HypoKind$left = FALSE
# }
# if(input$TTestKindOfTest1 == "greater"){
#   HypoKind$two = FALSE
#   HypoKind$right = TRUE
#   HypoKind$left = FALSE
# }
# if(input$TTestKindOfTest1 == "less"){
#   HypoKind$two = FALSE
#   HypoKind$right = FALSE
#   HypoKind$left = TRUE
# }
# })
# observeEvent(input$TTestKindOfTest2, {
#   output$veamo4 <- renderText("entré")
#   if(input$TTestKindOfTest2 == "two.sided"){
#     output$veamo4 <- renderText("entré")
#     HypoKind$two = TRUE
#     HypoKind$right = FALSE
#     HypoKind$left = FALSE
#   }
#   if(input$TTestKindOfTest2 == "greater"){
#     HypoKind$two = FALSE
#     HypoKind$right = TRUE
#     HypoKind$left = FALSE
#   }
#   if(input$TTestKindOfTest2 == "less"){
#     HypoKind$two = FALSE
#     HypoKind$right = FALSE
#     HypoKind$left = TRUE
#   }
# })
# observeEvent(input$TTestKindOfTest1,{
#   updateSelectInput(session = session, inputId = "TTestKindOfTest2", selected = NA)
# })
# observeEvent(input$TTestKindOfTest2,{
#   updateSelectInput(session = session, inputId = "TTestKindOfTest1", selected = NA)
# })
# outputOptions(output, "HytestTT", suspendWhenHidden = FALSE)
# outputOptions(output, "HytestLT", suspendWhenHidden = FALSE)
# outputOptions(output, "HytestRT", suspendWhenHidden = FALSE)

# https://stackoverflow.com/questions/33519816/shiny-what-is-the-difference-between-observeevent-and-eventreactive
# https://shiny.rstudio.com/reference/shiny/1.0.3/observeEvent.html
# https://shiny.rstudio.com/reference/shiny/0.11/outputOptions.html
# https://stackoverflow.com/questions/38895710/passing-reactive-values-to-conditionalpanel-condition/38899895