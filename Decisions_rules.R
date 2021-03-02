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
                             conditionalPanel(condition = "input.ZTestKind == '2 muestras'",
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
                             ))
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