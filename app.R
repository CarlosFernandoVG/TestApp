library(plotly)
library(tidyverse)
library(shinydashboard)
library(DT)
library(latex2exp)
library(shinyWidgets)
library(BSDA) #Para pruebas Z
source("functions.R")
source("Decisions_rules.R")
source("DescriptionsTests.R")
library(ggpubr)
#ui-----------------------------------------------------------------------------------------------------------
ui <- navbarPage(title = "TestApp", 
                 theme = "styles.css",
                 #Panel de pruebas-----------------------------------------------------------------------------------
                 tabPanel("Pruebas", 
                          icon = icon("vials"),
                          HTML("<span style='font-size: 32px;'>TestApp: </span> <span style='font-size: 18.72px;'>Pruebas paramétricas y no paramétricas</span>"),
                          #Sidebar-----------------------------------------------------------------------------------
                          sidebarLayout(
                            sidebarPanel(
                              style = "height:1000px;",
                              h4("Selecciona las mejores opciones para tu prueba"),
                              #Entrada de datos----------------------------------------------------------------------
                              fileInput("file", label = "Aquí puedes agregar tu archivo",  buttonLabel = "Selecciona tu archivo..."),
                              # hr(),
                              #Tipo de Prueba------------------------------------------------------------------------
                              fluidRow(
                                column(width = 8,
                                       radioGroupButtons(
                                         selected = NA,
                                         inputId = "KindOfTest",
                                         label = "Tipo de Prueba", 
                                         choices = c("Paramétrica", "No paramétrica"),
                                         status = "primary"
                                       )),
                                column(width = 4,
                                       numericInput("alphaTest", "Significancia", value = 0.05, step = 0.01))
                              ),
                              #Panel condicional para el tipo de prueba Paramétrica----------------------------------
                              conditionalPanel(
                                condition = "input.KindOfTest == 'Paramétrica'",
                                radioGroupButtons(
                                  selected = NA,
                                  inputId = "ParametricTest",
                                  label = "Prueba", 
                                  choices = c("T-test", "Z-test", "Shapiro-Wilks"),
                                  direction = "vertical",
                                  status = "btn btn-info"
                                ),
                                #Prueba T-Student-------------------------------------------------------------------
                                conditionalPanel(
                                  condition = "input.ParametricTest == 'T-test'",
                                  radioGroupButtons(
                                    selected = NA,
                                    inputId = "TTestKind",
                                    label = NULL, 
                                    choices = c("1 muestra", "2 muestras"),
                                    status = "btn btn-info"
                                  ),
                                  # Prueba T-Student 1 muestra-------------------------------------------------------
                                  conditionalPanel(
                                    condition = "input.TTestKind == '1 muestra'",
                                    uiOutput("TTest1"),
                                    numericInput(inputId = "TTest1Mu", "Agrega la media", value = 0, step = 1),
                                    selectInput(inputId = "TTestKindOfTest1", "Hipótesis alternativa", choices = c("two.sided", "greater", "less"), selected = 'two.sided')
                                  ),
                                  # Prueba T-Student 2 muestras------------------------------------------------------
                                  conditionalPanel(
                                    condition = "input.TTestKind == '2 muestras'",
                                    uiOutput("TTest2_1"),
                                    uiOutput("TTest2_2"),
                                    checkboxInput("TTest2Par", "¿Los datos son pareados?", value = F),
                                    checkboxInput("TTest2Var", "¿Las varianzas son iguales?", value = F),
                                    selectInput(inputId = "TTestKindOfTest2", "Hipótesis alternativa", choices = c("two.sided", "greater", "less"), selected = 'two.sided')
                                  )
                                ),
                                #Prueba Z---------------------------------------------------------------------------
                                conditionalPanel(
                                  condition = "input.ParametricTest == 'Z-test'",
                                  radioGroupButtons(
                                    selected = NA,
                                    inputId = "ZTestKind",
                                    label = NULL, 
                                    choices = c("1 muestra", "2 muestras"),
                                    status = "btn btn-info"
                                  ),
                                  # Prueba Z 1 muestra-------------------------------------------------------
                                  conditionalPanel(
                                    condition = "input.ZTestKind == '1 muestra'",
                                    uiOutput("ZTest1"),
                                    numericInput(inputId = "ZTest1Mu", "Agrega la media", value = 0, step = 1),
                                    numericInput(inputId = "ZTest1Sigma", "Agrega la desviación estandar", step = 1, value = NA),
                                    selectInput(inputId = "ZTestKindOfTest1", "Hipótesis alternativa", choices = c("two.sided", "greater", "less"), selected = 'two.sided')
                                  ),
                                  # Prueba Z 2 muestras------------------------------------------------------
                                  conditionalPanel(
                                    condition = "input.ZTestKind == '2 muestras'",
                                    uiOutput("ZTest2_1"),
                                    uiOutput("ZTest2_2"),
                                    numericInput(inputId = "ZTest2_1Sigma", "Agrega la desviación estandar", step = 1, value = NA),
                                    numericInput(inputId = "ZTest2_2Sigma", "Agrega la desviación estandar", step = 1, value = NA),
                                    selectInput(inputId = "ZTestKindOfTest2", "Hipótesis alternativa", choices = c("two.sided", "greater", "less"), selected = 'two.sided')
                                  )
                                ),
                                #Prueba Shapiro-Wilks---------------------------------------------------------------
                                conditionalPanel(
                                  "Actualmente sólo para un nivel de cofianza del 95%",
                                  condition = "input.ParametricTest == 'Shapiro-Wilks'",
                                  uiOutput("SWTest1"),
                                  selectInput("ShapiroCoef", label = "Tipo de tabla", choices = c("Cuantiles", "Coeficientes"), selected = "Distribución")
                                )
                              ),
                              #Panel condicional para el tipo de prueba No Paramétrica------------------------------
                              conditionalPanel(
                                condition = "input.KindOfTest == 'No paramétrica'",
                                radioGroupButtons(
                                  selected = NA,
                                  inputId = "NParametricTest",
                                  label = "Prueba", 
                                  choices = c("Binomial", "Rango", "Varianzas"),
                                  status = "primary"
                                ),
                                #Pruebas binomiales-----------------------------------------------------------------
                                conditionalPanel(
                                  condition = "input.NParametricTest == 'Binomial'",
                                  fluidRow(
                                    column(width = 4, 
                                           radioGroupButtons(
                                             selected = NA,
                                             inputId = "BinomialTest",
                                             label = NULL, 
                                             direction = "vertical",
                                             choices = c("Proporciones", "Cuantiles", "Signos", "McNemar","Cox Stuart"),
                                             status = "btn btn-info"
                                           )
                                    ),
                                    column(width = 8, 
                                           # Proporciones-------------------------------------------------------------------
                                           conditionalPanel(
                                             condition = "input.BinomialTest == 'Proporciones'",
                                             radioGroupButtons(
                                               selected = NA,
                                               inputId = "BinomialTestInput",
                                               label = "Elige la forma de aplicar la prueba",
                                               choices = c("Manual", "Datos"),
                                               status = "btn btn-info"
                                             ),
                                             "¿Deseas aplicar una aproximación normal?",
                                             checkboxInput("BTPNormal", label = NULL, value = F)
                                           ),
                                           #Signos-------------------------------------------------------------------------
                                           conditionalPanel(
                                             condition = "input.BinomialTest == 'Signos'",
                                             radioGroupButtons(
                                               selected = NA,
                                               inputId = "SignosTestInput",
                                               label = "Elige la forma de aplicar la prueba",
                                               choices = c("Manual", "Datos"),
                                               status = "btn btn-info"
                                             ),
                                             "¿Deseas aplicar una aproximación normal?",
                                             checkboxInput("SigTPNormal", label = NULL, value = F)
                                           )
                                    )
                                  ),
                                  #Paneles condicionales para cada prueba Binomial---------------------------------------------------------------------------------------------
                                  conditionalPanel(condition = "input.BinomialTest == 'Proporciones'",
                                                   conditionalPanel(
                                                     condition = "input.BinomialTestInput == 'Manual'",
                                                     numericInput(inputId = "BTtrials", label = "Número de éxitos", value = NA),
                                                     numericInput(inputId = "BTn", label = "Tamaño de la muestra", value = NA, min = 0),
                                                     numericInput(inputId = "BTpM", label = "$p$ específica", value = 0.5),
                                                     selectInput(inputId = "BinomialTestKindOfTestM", "Hipótesis alternativa", choices = c("two.sided", "greater", "less"), selected = 'two.sided')
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.BinomialTestInput == 'Datos'",
                                                     "Tu variable debe estar codificada con 0s y 1s (0: Fallos, 1: Éxitos)",
                                                     uiOutput("BinomialTestVar"),
                                                     numericInput(inputId = "BTpD", label = "$p$ específica", value = 0.5),
                                                     selectInput(inputId = "BinomialTestKindOfTestD", "Hipótesis alternativa", choices = c("two.sided", "greater", "less"), selected = 'two.sided')
                                                   )
                                  ),
                                  conditionalPanel(condition = "input.BinomialTest == 'Signos'",
                                                   conditionalPanel(
                                                     condition = "input.SignosTestInput == 'Manual'",
                                                     numericInput(inputId = "SigTGreater", label = "# de casos: $X_i<Y_i$", value = NA, min = 0),
                                                     numericInput(inputId = "SigTLess", label = "# de casos: $X_i>Y_i$", value = NA, min = 0),
                                                     selectInput(inputId = "SignosTestKindOfTestM", "Hipótesis alternativa", choices = c("two.sided", "greater", "less"), selected = 'two.sided')
                                                   )
                                                   ,
                                                   conditionalPanel(
                                                     condition = "input.SignosTestInput == 'Datos'",
                                                     "Tus datos deben ser numéricos",
                                                     uiOutput("SigTPvar_1"),
                                                     uiOutput("SigTPvar_2"),
                                                     selectInput(inputId = "SignosTestKindOfTestD", "Hipótesis alternativa", choices = c("two.sided", "greater", "less"), selected = 'two.sided')
                                                   )
                                  ),
                                  conditionalPanel(condition = "input.BinomialTest == 'Cuantiles'"),
                                  conditionalPanel(condition = "input.BinomialTest == 'McNemar'"),
                                  conditionalPanel(condition = "input.BinomialTest == 'Cox Stuart'")
                                ),
                                #Pruebas de Rango-------------------------------------------------------------------
                                conditionalPanel(
                                  condition = "input.NParametricTest == 'Rango'",
                                  radioGroupButtons(
                                    selected = NA,
                                    inputId = "RangoTest",
                                    label = NULL, 
                                    direction = "vertical",
                                    choices = c("U-Mann-Whitney", "Kruskal-Wallis", "Friedman"),
                                    status = "btn btn-info"
                                  )
                                ),
                                #Pruebas de varianza----------------------------------------------------------------
                                conditionalPanel(
                                  condition = "input.NParametricTest == 'Varianzas'",
                                  radioGroupButtons(
                                    selected = NA,
                                    inputId = "VarianzasTest",
                                    label = NULL, 
                                    direction = "vertical",
                                    choices = c("Fisher", ">2"),
                                    status = "btn btn-info"
                                  )
                                )
                              ),
                              hr(),
                              tags$head(
                                tags$link(rel="stylesheet", 
                                          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css")
                              ),
                              HTML('<p style="text-align: center;">Report a <a href="https://github.com/CarlosFernandoVG/TestApp/issues">bug</a> , give me your ideas or a review.</p>'),
                              HTML('<p style="text-align: center;"><span style="color: #1665AE;">A work by <a href="https://github.com/CarlosFernandoVG" style="color: #1665AE">Carlos Vásquez</a></p>'),
                              HTML('<p style="text-align: center;"><span style="color: #1665AE;"><em>carlosfvasquez@ciencias.unam.mx</em></span></p>'),
                              HTML('<p style="text-align: center;"><a href="https://twitter.com/CarlosFVasquezG?lang=es" class="fab fa-twitter" style="color: #1665AE"></a>
                              <a href="https://github.com/CarlosFernandoVG?lang=es" class="fab fa-github" style="color: #1665AE"></a></p>'),
                              hr(),
                              HTML('<a rel="license" href="https://creativecommons.org/licenses/by-sa/4.0/" target="_blank"><img alt="Licence Creative Commons" style="border-width:0"
        src="https://licensebuttons.net/l/by/4.0/80x15.png"/></a> This work of is made available under the terms of the <a rel="license"
        href="https://creativecommons.org/licenses/by-sa/4.0/" target="_blank">Creative Commons Attribution-ShareAlike 4.0 International License</a>. Source code available on <a href="https://github.com/CarlosFernandoVG/TestApp" target="_blank">GitHub</a>.')
                            ),
                            #Panel principal------------------------------------------------------------------------
                            mainPanel(
                              #Panel superior-----------------------------------------------------------------------
                              fluidRow(
                                #Summary de la prueba---------------------------------------------------------------
                                column(width = 8,
                                       conditionalPanel(
                                         condition = "input.KindOfTest == 'Paramétrica'",
                                         verbatimTextOutput("summaryP")),
                                       conditionalPanel(
                                         condition = "input.KindOfTest == 'No paramétrica'",
                                         verbatimTextOutput("summaryNP"))
                                ), 
                                #Estadísticos rápidos---------------------------------------------------------------
                                column(width = 4,
                                       #Vamos a agregar algo para que permita el diseño latex inline
                                       tags$head(
                                         tags$link(rel="stylesheet", 
                                                   href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
                                                   integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
                                                   crossorigin="anonymous"),
                                         HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
                                         HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
                                         HTML('<script> document.addEventListener("DOMContentLoaded", function(){renderMathInElement(document.body, {delimiters: [{left: "$", right: "$", display: false}]});})</script>')
                                       ),
                                       #Es necesario una vez aplicar esta función
                                       h3("Datos rápidos"),
                                       fluidRow(
                                         column(
                                           h4("Estadístico ($T$)"), 
                                           width = 6,
                                           h4(textOutput("Statistical"))),
                                         column(
                                           h4("Cuantíl ($t$)"), 
                                           width = 6,
                                           h4(textOutput("Quantil")))
                                       ),
                                       fluidRow(
                                         align = "center",
                                         h5("Se rechaza $H_0$ cuando: "),
                                         #Esto se debe mejorar de alguna forma :/
                                         conditionalPanel(
                                           condition = "input.KindOfTest == 'Paramétrica'",
                                           TTestHyp, ZTestHyp, ShapiroTestHyp),
                                         conditionalPanel(
                                           condition = "input.KindOfTest == 'No paramétrica'", 
                                           BinomialTestHyp)
                                       ),
                                       fluidRow(
                                         align = "center",
                                         h5("$P-value$"),
                                         h4(textOutput("P_value"))
                                       )
                                )
                              )
                              ,
                              #Panel inferior-----------------------------------------------------------------------
                              fluidRow(
                                #Gráficas---------------------------------------------------------------------------
                                column(width = 8,
                                       #Gráfica para pruebas Paramétricas-------------------------------------------
                                       conditionalPanel(
                                         condition = "input.KindOfTest == 'Paramétrica'",
                                         #Gráficas------------------------------------------------------------------
                                         conditionalPanel(condition = "input.ParametricTest == 'T-test' | input.ParametricTest == 'Z-test'",
                                                          conditionalPanel(
                                                            condition = "input.TTestKind == '1 muestra' | input.TTestKind == '2 muestras' | input.ZTestKind == '1 muestra' | input.ZTestKind == '2 muestras'",
                                                            column(width = 11,
                                                                   plotOutput("DensityParametricPlots")
                                                            ),
                                                            column(width = 1,
                                                                   dropdownButton(
                                                                     h3("Modificar el rango de tu gráfica"),
                                                                     numericInput(inputId = "RangoX1P", "", value = -5, step = 1),
                                                                     numericInput(inputId = "RangoX2P", "", value = 5, step = 1),
                                                                     circle = FALSE, status = "primary",
                                                                     icon = icon("gear"), width = "100px",
                                                                     
                                                                     tooltip = tooltipOptions(title = "Modifica el rango de la gráfica")
                                                                   )
                                                            )
                                                          )
                                         ),
                                         #Tablas--------------------------------------------------------------------
                                         conditionalPanel(
                                           condition = "input.ParametricTest == 'Shapiro-Wilks'",
                                           DT::DTOutput("Shapiro_table")
                                         )
                                       ),
                                       #Gráfica para pruebas No Paramétricas---------------------------------------
                                       conditionalPanel(condition = "input.KindOfTest == 'No paramétrica'",
                                                        conditionalPanel(
                                                          condition = "input.BinomialTestInput == 'Manual' | input.BinomialTestInput == 'Datos' | input.BinomialTest == 'Cuantiles' | input.BinomialTest == 'Signos' | input.BinomialTest == 'McNemar' | input.BinomialTest =='Cox Stuart' | input.RangoTest == 'U-Mann-Whitney' | input.RangoTest == 'Kruskal-Wallis' | input.RangoTest == 'Friedman' | input.VarianzasTest == 'Fisher' | input.VarianzasTest == '>2'",
                                                          column(width = 11,
                                                                 plotOutput("DensityPlot_NParametric")
                                                          ),
                                                          column(width = 1,
                                                                 dropdownButton(
                                                                   h3("Modificar el rango de tu gráfica"),
                                                                   numericInput(inputId = "RangoX1NP", "", value = -5, step = 1),
                                                                   numericInput(inputId = "RangoX2NP", "", value = 5, step = 1),
                                                                   circle = FALSE, status = "primary",
                                                                   icon = icon("gear"), width = "100px",
                                                                   tooltip = tooltipOptions(title = "Modifica el rango de la gráfica")
                                                                 )
                                                          )
                                                        )
                                       )
                                ), 
                                #Descripción de la prueba----------------------------------------------------------
                                column(width = 4,
                                       conditionalPanel(
                                         condition = "input.KindOfTest == 'Paramétrica'",
                                         h3(textOutput("headerDescriptionPT")),
                                         textOutput("DescriptionPT")
                                       ),
                                       conditionalPanel(
                                         condition = "input.KindOfTest == 'No paramétrica'",
                                         h3(textOutput("headerDescriptionNPT")),
                                         conditionalPanel(
                                           condition = "input.NParametricTest == 'Binomial'",
                                           h4(textOutput("headerDescriptionNPTB")),
                                           textOutput("DescriptionNPTB"),
                                         )
                                       )
                                )
                              )
                            )
                          )
                 ),
                 #Presentación de los datos-------------------------------------------------------------------------
                 tabPanel("Aquí puedes ver tus datos", 
                          style = "height:610px",
                          icon = icon("table"),
                          DT::DTOutput("fileOutput")
                 )
)
#Server-------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  Proves <- reactiveValues(test = NULL, statistical = NULL, p_value = NULL, cuantil = NULL)
  #Datos rápidos-------------------------------------------------------------------------------------------------------
  output$Statistical <- renderText(Proves$statistical)
  output$P_value <- renderText(Proves$p_value)
  output$Quantil <- renderText({
    if(length(Proves$cuantil)  == 2){
      paste(Proves$cuantil[1], ", ", Proves$cuantil[2])
    }else{
      Proves$cuantil
    }
  })
  
  #Descripción de la prueba----------------------------------------------------------------------------------------
  #Encabezado
  output$headerDescriptionPT <- renderText({
    req(input$ParametricTest)
    input$ParametricTest
  })
  
  output$headerDescriptionNPT <- renderText({
    req(input$NParametricTest)
    input$NParametricTest
  })
  output$headerDescriptionNPTB <- renderText({
    req(input$BinomialTest)
    input$BinomialTest
  })
  
  #Descripción
  output$DescriptionPT <- renderText({
    req(input$ParametricTest)
    description_test_NP(input$ParametricTest)
  })
  output$DescriptionNPTB <- renderText({
    req(input$BinomialTest)
    description_test_PB(input$BinomialTest)
  })
  
  
  #Gráficas y tablas-------------------------------------------------------------------------------------------------
  plot_P <- reactive({
    #gráfica base para pruebas Paramétricas--------------------------------------------------------------------------
    #Creamos los datos
    dat <- data_frame(x = c(input$RangoX1P,input$RangoX2P))
    #Una gráfica base
    plot_b <- dat %>% 
      ggplot(aes(x = x)) + 
      guides(colour = guide_legend(label.position = "left", title = NULL)) +
      labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") + 
      general_theme 
    plot_b
  })
  plot_NP <- reactive({
    #gráfica base para pruebas No Paramétricas----------------------------------------------------------------------
    if(input$BinomialTestInput == 'Manual'){
      dat <- data_frame(x = c(input$RangoX1NP,input$RangoX2NP))
      if(input$BinomialTestKindOfTestM == "two.sided"){dat <- data_frame(x = c(0,input$RangoX2NP))}
      #Una gráfica base
      plot_b <- dat %>% 
        ggplot(aes(x = x)) + 
        guides(colour = guide_legend(label.position = "left", title = NULL)) +
        labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") + 
        general_theme 
      return(plot_b)
    }
    if(input$BinomialTestInput == 'Datos'){
      dat <- data_frame(x = c(input$RangoX1NP,input$RangoX2NP))
      if(input$BinomialTestKindOfTestD == "two.sided"){dat <- data_frame(x = c(0,input$RangoX2NP))}
      #Una gráfica base
      plot_b <- dat %>% 
        ggplot(aes(x = x)) + 
        guides(colour = guide_legend(label.position = "left", title = NULL)) +
        labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") + 
        general_theme 
      return(plot_b)
    }
    # if(input$SignosTestInput == 'Manual'){
    #   dat <- data_frame(x = c(input$RangoX1NP,input$RangoX2NP))
    #   # if(input$SignosTestKindOfTestM == "two.sided"){dat <- data_frame(x = c(0,input$RangoX2NP))}
    #   #Una gráfica base
    #   plot_b <- dat %>% 
    #     ggplot(aes(x = x)) + 
    #     guides(colour = guide_legend(label.position = "left", title = NULL)) +
    #     labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") + 
    #     general_theme 
    #   return(plot_b)
    # }
    # if(input$SignosTestInput == 'Datos'){
    #   dat <- data_frame(x = c(input$RangoX1NP,input$RangoX2NP))
    #   # if(input$SignosTestKindOfTestD == "two.sided"){dat <- data_frame(x = c(0,input$RangoX2NP))}
    #   #Una gráfica base
    #   plot_b <- dat %>% 
    #     ggplot(aes(x = x)) + 
    #     guides(colour = guide_legend(label.position = "left", title = NULL)) +
    #     labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") + 
    #     general_theme 
    #   return(plot_b)
    # }
  })
  #gráficas
  plotis <- reactiveValues(plot = NULL)

  output$DensityParametricPlots <- renderPlot({
    #Gráficas paramétricas---------------------------------------------------------------------------------------------
    req(input$file)
    graph <- plot_P()
    if(input$ParametricTest == "T-test"){
      #TTest-----------------------------------------------------------------------------------------------------------
      if(input$TTestKind == "1 muestra"){
        cuantil <- Proves$cuantil
        if(input$TTestKindOfTest1 == "two.sided"){
          cuantil <- c(-Proves$cuantil, Proves$cuantil)
        }
        plotis$plot <- graph +
          stat_function(fun = ~dt(.x, Proves$test$parameter)) + 
          ggtitle(TeX(paste("$Student's T_{", Proves$test$parameter, "}$")))+
          geom_segment(aes(x = Proves$statistical, xend = Proves$statistical, y = 0, yend = dt(Proves$statistical, Proves$test$parameter), colour = "Estadístico: ")) + 
          geom_segment(aes(x = cuantil, xend = cuantil, y = 0, yend = dt(cuantil, Proves$test$parameter), colour = "Cuantíl: "))+
          stat_function(color = NA, fun= ~under_curve(type = input$TTestKindOfTest1, alpha = input$alphaTest, x = .x, fun = dt, fq = qt, df = Proves$test$parameter), 
                        geom = 'area', fill = '#13378f', alpha = 0.2)+
          scale_colour_manual(values = c("#386df2", "black"))
      }else{
        cuantil <- Proves$cuantil
        if(input$TTestKindOfTest2 == "two.sided"){
          cuantil <- c(-Proves$cuantil, Proves$cuantil)
        }
        plotis$plot <- graph +
          stat_function(fun = ~dt(.x, Proves$test$parameter)) + 
          ggtitle(TeX(paste("$Student's T_{", Proves$test$parameter, "}$")))+
          geom_segment(aes(x = Proves$statistical, xend = Proves$statistical, y = 0, yend = dt(Proves$statistical, Proves$test$parameter), colour = "Estadístico: ")) + 
          geom_segment(aes(x = cuantil, xend = cuantil, y = 0, yend = dt(cuantil, Proves$test$parameter), colour = "Cuantíl: "))+
          stat_function(color = NA, fun= ~under_curve(type = input$TTestKindOfTest2, alpha = input$alphaTest, x = .x, fun = dt, fq = qt, df = Proves$test$parameter), 
                        geom = 'area', fill = '#13378f', alpha = 0.2)+
          scale_colour_manual(values = c("#386df2", "black"))
      }
    }
    if(input$ParametricTest == "Z-test"){
      #ZTest------------------------------------------------------------------------------------------------------------
      graph <- plot_P() +
        stat_function(fun = ~dnorm(.x)) + 
        ggtitle(TeX(paste("$N(0,1)$")))
      
      if(input$ZTestKind == "1 muestra"){
        validate(need(!is.na(input$ZTest1Sigma), "Ingresa la desviación estandar"))
        
        cuantil <- Proves$cuantil
        if(input$ZTestKindOfTest1 == "two.sided"){
          cuantil <- c(-Proves$cuantil, Proves$cuantil)
        }
        plotis$plot <- graph + 
          geom_segment(aes(x = Proves$statistical, xend = Proves$statistical, y = 0, yend = dnorm(Proves$statistical), colour = "Estadístico: ")) + 
          geom_segment(aes(x = cuantil, xend = cuantil, y = 0, yend = dnorm(cuantil), colour = "Cuantíl: ")) +
          stat_function(color = NA, fun= ~under_curve(type = input$ZTestKindOfTest1, alpha = input$alphaTest, x = .x, fun = dnorm, fq = qnorm), 
                        geom = 'area', fill = '#13378f', alpha = 0.2)+
          scale_colour_manual(values = c("#386df2", "black"))
        
      }else{
        validate(need(!is.na(input$ZTest2_1Sigma), "Ingresa la desviación estandar"))
        validate(need(!is.na(input$ZTest2_2Sigma), "Ingresa la desviación estandar"))
        
        cuantil <- Proves$cuantil
        if(input$ZTestKindOfTest2 == "two.sided"){
          cuantil <- c(-Proves$cuantil, Proves$cuantil)
        }
        plotis$plot <- graph + 
          geom_segment(aes(x = Proves$statistical, xend = Proves$statistical, y = 0, yend = dnorm(Proves$statistical), colour = "Estadístico: ")) + 
          geom_segment(aes(x = cuantil, xend = cuantil, y = 0, yend = dnorm(cuantil), colour = "Cuantíl: ")) +
          stat_function(color = NA, fun= ~under_curve(type = input$ZTestKindOfTest2, alpha = input$alphaTest, x = .x, fun = dnorm, fq = qnorm), 
                        geom = 'area', fill = '#13378f', alpha = 0.2)+
          scale_colour_manual(values = c("#386df2", "black"))
      }
    }
    isolate(plotis$plot)
  }, bg="transparent")
  
  cuantiles_Shapiro <- reactive({
    read_delim("Shapiro-Wilk.csv", col_names = TRUE, delim = " ")
  })
  output$Shapiro_table <- DT::renderDataTable({
    #Tablas Shapiro--------------------------------------------------------------------------------------------------------
    plotis$plot <- NULL
    req(input$file)
    if(input$ShapiroCoef == "Cuantiles"){
      datatable(cuantiles_Shapiro(), rownames= FALSE,
                options = list(
                  # autowidth = FALSE,
                  # columnDefs = list(list(width = '1200px', targets = c(1, 2))),
                  pageLength = 10,
                  # lengthMenu = c(7, 14, 28, 36),
                  scrollX = TRUE,
                  fixedColumns = TRUE
                ))
    }else{
      datatable(read.csv("Shapiro-WilkCoef.csv", header = T, stringsAsFactors = F, na.strings = "NA", check.names = F), 
                rownames= FALSE,
                options = list(
                  pageLength = 10,
                  scrollX = TRUE,
                  fixedColumns = TRUE
                ))
    }
  })
  plotis_NP <- reactiveValues(plot = NULL)
  output$DensityPlot_NParametric <- renderPlot({
    #Gráficas no Parametricas---------------------------------------------------------------------------------------------
    if(input$NParametricTest == "Binomial"){
      #Gráficas binomiales------------------------------------------------------------------------------------------------
      #Debemos crear funciones para tener un código más limpio
      if(input$BinomialTest == 'Proporciones'){
        if(input$BinomialTestInput == 'Manual'){
          validate(need(input$BTtrials , "Ingresa el número de éxitos"))
          validate(need(input$BTn , "Ingresa el tamaño de la muestra"))
          if(input$BTPNormal){
            if(input$BinomialTestKindOfTestM == "two.sided"){
              plotis_NP$plot <- plot_NP() +
                stat_function(fun = ~dchisq(.x, df = Proves$test$parameter)) +
                ggtitle(TeX(paste("$\\chi_{1}$"))) +
                geom_segment(aes(x = Proves$statistical, xend = Proves$statistical, y = 0,
                                 yend = dchisq(x = Proves$statistical, df = Proves$test$parameter),
                                 colour = "Estadístico: ")) +
                geom_segment(aes(x = Proves$cuantil, xend = Proves$cuantil, y = 0,
                                 yend = dchisq(x = Proves$cuantil, df = Proves$test$parameter),
                                 colour = "Cuantíl: ")) +
                stat_function(color = NA, fun= ~under_curve(type = "greater",
                                                            alpha = input$alphaTest, x = .x,
                                                            fun = dchisq, fq = qchisq,
                                                            df = Proves$test$parameter),
                              geom = 'area', fill = '#13378f', alpha = 0.2)+
                scale_colour_manual(values = c("#386df2", "black"))
            }
            if(input$BinomialTestKindOfTestM != "two.sided"){
              plotis_NP$plot <- plot_NP() +
                stat_function(fun = ~dnorm(.x)) + 
                ggtitle(TeX(paste("$N(0,1)$"))) + 
                geom_segment(aes(x = Proves$statistical, xend = Proves$statistical, y = 0, yend = dnorm(Proves$statistical), colour = "Estadístico: ")) + 
                geom_segment(aes(x = Proves$cuantil, xend = Proves$cuantil, y = 0, yend = dnorm(Proves$cuantil), colour = "Cuantíl: ")) +
                stat_function(color = NA, fun= ~under_curve(type = input$BinomialTestKindOfTestM, alpha = input$alphaTest, x = .x, fun = dnorm, fq = qnorm), 
                              geom = 'area', fill = '#13378f', alpha = 0.2)+
                scale_colour_manual(values = c("#386df2", "black"))
            }
          }else{
            binom_reject <- reject_zone_discrete(type = input$BinomialTestKindOfTestM, n = input$BTn, alpha = input$alphaTest, fun = dbinom, fq = qbinom, prob = input$BTpM, size = input$BTn)
            data <- tibble(x = seq(0, input$BTn)[!(seq(0, input$BTn) %in% binom_reject$x)], y = dbinom(x = x, size = input$BTn, prob = input$BTpM))
            binom_stat <- tibble(x = Proves$statistical, y = dbinom(x, size = input$BTn, prob = input$BTpM))
            binom_cuantil <- cuantil_assing(type = input$BinomialTestKindOfTestM, alpha = input$alphaTest, fun = dbinom, fq = qbinom, prob = input$BTpM, size = input$BTn)
            
            plotis_NP$plot <- data %>% ggplot(aes(x = x, y = y)) + 
              geom_segment(aes(xend=x, yend=0), color = "grey53") +
              geom_point(color = "grey53") +
              #Región de rechazo
              geom_segment(data = binom_reject, aes(xend = x, yend = 0),  color = '#13378f', alpha = 0.4) +
              geom_point(data = binom_reject, color = '#13378f', alpha = 0.4) + 
              #Estadístico
              geom_segment(data = binom_stat, aes(xend  = x, yend = 0, colour = "Estadístico: ")) + 
              geom_point(data = binom_stat, aes(colour = "Estadístico: ")) +
              #Cuantiles
              geom_segment(data = binom_cuantil, aes(xend = x, yend = 0, colour = "Cuantíl: ")) + 
              geom_point(data = binom_cuantil, aes(colour = "Cuantíl: ")) + 
              #Diseño
              scale_colour_manual(values = c("#386df2", "black")) +
              ggtitle(TeX(paste("$Binomial(", input$BTn,", ", input$BTpM, ")$"))) +
              general_theme + 
              guides(colour = guide_legend(label.position = "left", title = NULL)) +
              labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") 
          }
        }else{
          req(input$file)
          if(input$BTPNormal){
            if(input$BinomialTestKindOfTestD == "two.sided"){
              plotis_NP$plot <- plot_NP() +
                stat_function(fun = ~dchisq(.x, df = Proves$test$parameter)) +
                ggtitle(TeX(paste("$\\chi_{1}$"))) +
                geom_segment(aes(x = Proves$statistical, xend = Proves$statistical, y = 0,
                                 yend = dchisq(x = Proves$statistical, df = Proves$test$parameter),
                                 colour = "Estadístico: ")) +
                geom_segment(aes(x = Proves$cuantil, xend = Proves$cuantil, y = 0,
                                 yend = dchisq(x = Proves$cuantil, df = Proves$test$parameter),
                                 colour = "Cuantíl: ")) +
                stat_function(color = NA, fun= ~under_curve(type = "greater",
                                                            alpha = input$alphaTest, x = .x,
                                                            fun = dchisq, fq = qchisq,
                                                            df = Proves$test$parameter),
                              geom = 'area', fill = '#13378f', alpha = 0.2)+
                scale_colour_manual(values = c("#386df2", "black"))
            }
            if(input$BinomialTestKindOfTestD != "two.sided"){
              plotis_NP$plot <- plot_NP() +
                stat_function(fun = ~dnorm(.x)) + 
                ggtitle(TeX(paste("$N(0,1)$"))) + 
                geom_segment(aes(x = Proves$statistical, xend = Proves$statistical, y = 0, yend = dnorm(Proves$statistical), colour = "Estadístico: ")) + 
                geom_segment(aes(x = Proves$cuantil, xend = Proves$cuantil, y = 0, yend = dnorm(Proves$cuantil), colour = "Cuantíl: ")) +
                stat_function(color = NA, fun= ~under_curve(type = input$BinomialTestKindOfTestD, alpha = input$alphaTest, x = .x, fun = dnorm, fq = qnorm), 
                              geom = 'area', fill = '#13378f', alpha = 0.2)+
                scale_colour_manual(values = c("#386df2", "black"))
            }
          }else{
            binom_reject <- reject_zone_discrete(type = input$BinomialTestKindOfTestD, n = Proves$test$parameter, alpha = input$alphaTest, fun = dbinom, fq = qbinom, prob = input$BTpD, size = Proves$test$parameter)
            data <- tibble(x = seq(0, Proves$test$parameter)[!(seq(0, Proves$test$parameter) %in% binom_reject$x)], y = dbinom(x = x, size = Proves$test$parameter, prob = input$BTpD))
            binom_stat <- tibble(x = Proves$statistical, y = dbinom(x, size = Proves$test$parameter, prob = input$BTpD))
            binom_cuantil <- cuantil_assing(type = input$BinomialTestKindOfTestD, alpha = input$alphaTest, fun = dbinom, fq = qbinom, prob = input$BTpD, size = Proves$test$parameter)
            
            plotis_NP$plot <- data %>% ggplot(aes(x = x, y = y)) + 
              geom_segment(aes(xend=x, yend=0), color = "grey53") +
              geom_point(color = "grey53") +
              #Región de rechazo
              geom_segment(data = binom_reject, aes(xend = x, yend = 0),  color = '#13378f', alpha = 0.4) +
              geom_point(data = binom_reject, color = '#13378f', alpha = 0.4) + 
              #Estadístico
              geom_segment(data = binom_stat, aes(xend  = x, yend = 0, colour = "Estadístico: ")) + 
              geom_point(data = binom_stat, aes(colour = "Estadístico: ")) +
              #Cuantiles
              geom_segment(data = binom_cuantil, aes(xend = x, yend = 0, colour = "Cuantíl: ")) + 
              geom_point(data = binom_cuantil, aes(colour = "Cuantíl: ")) + 
              #Diseño
              scale_colour_manual(values = c("#386df2", "black")) +
              ggtitle(TeX(paste("$Binomial(", Proves$test$parameter,", ", input$BTpD, ")$"))) +
              general_theme + 
              guides(colour = guide_legend(label.position = "left", title = NULL)) +
              labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") 
          }
        }
      }
      if(input$BinomialTest == 'Signos'){
        validate(need(input$SignosTestInput, "Selecciona que como deseas realizar tu prueba"))
        if(input$SignosTestInput == 'Manual'){
          validate(need(input$SigTGreater , "Ingresa la información necesaria"))
          validate(need(input$SigTLess , "Ingresa la información necesaria"))
          if(input$SigTPNormal){
            if(input$SignosTestKindOfTestM == "two.sided"){
              plotis_NP$plot <-data_frame(x = c(0,input$RangoX2NP)) %>% 
                ggplot(aes(x = x)) + 
                guides(colour = guide_legend(label.position = "left", title = NULL)) +
                labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") + 
                general_theme +
                stat_function(fun = ~dchisq(.x, df = Proves$test$parameter)) +
                ggtitle(TeX(paste("$\\chi_{1}$"))) +
                geom_segment(aes(x = Proves$statistical, xend = Proves$statistical, y = 0,
                                 yend = dchisq(x = Proves$statistical, df = Proves$test$parameter),
                                 colour = "Estadístico: ")) +
                geom_segment(aes(x = Proves$cuantil, xend = Proves$cuantil, y = 0,
                                 yend = dchisq(x = Proves$cuantil, df = Proves$test$parameter),
                                 colour = "Cuantíl: ")) +
                stat_function(color = NA, fun= ~under_curve(type = "greater",
                                                            alpha = input$alphaTest, x = .x,
                                                            fun = dchisq, fq = qchisq,
                                                            df = Proves$test$parameter),
                              geom = 'area', fill = '#13378f', alpha = 0.2)+
                scale_colour_manual(values = c("#386df2", "black"))
            }
            if(input$SignosTestKindOfTestM != "two.sided"){
              plotis_NP$plot <- data_frame(x = c(input$RangoX1NP,input$RangoX2NP)) %>% 
                ggplot(aes(x = x)) + 
                guides(colour = guide_legend(label.position = "left", title = NULL)) +
                labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") + 
                general_theme +
                stat_function(fun = ~dnorm(.x)) +
                ggtitle(TeX(paste("$N(0,1)$"))) +
                geom_segment(aes(x = Proves$statistical, xend = Proves$statistical, y = 0, yend = dnorm(Proves$statistical), colour = "Estadístico: ")) +
                geom_segment(aes(x = Proves$cuantil, xend = Proves$cuantil, y = 0, yend = dnorm(Proves$cuantil), colour = "Cuantíl: ")) +
                stat_function(color = NA, fun= ~under_curve(type = input$SignosTestKindOfTestM, alpha = input$alphaTest, x = .x, fun = dnorm, fq = qnorm),
                              geom = 'area', fill = '#13378f', alpha = 0.2)+
                scale_colour_manual(values = c("#386df2", "black"))
            }
          }else{
            binom_reject <- reject_zone_discrete(type = input$SignosTestKindOfTestM, n = Proves$test$parameter, alpha = input$alphaTest, fun = dbinom, fq = qbinom, prob = 0.5, size = Proves$test$parameter)
            data <- tibble(x = seq(0, Proves$test$parameter)[!(seq(0, Proves$test$parameter) %in% binom_reject$x)], y = dbinom(x = x, size = Proves$test$parameter, prob = 0.5))
            binom_stat <- tibble(x = Proves$statistical, y = dbinom(x, size = Proves$test$parameter, prob = 0.5))
            binom_cuantil <- cuantil_assing(type = input$SignosTestKindOfTestM, alpha = input$alphaTest, fun = dbinom, fq = qbinom, prob = 0.5, size = Proves$test$parameter)
            
            plotis_NP$plot <- data %>% ggplot(aes(x = x, y = y)) + 
              geom_segment(aes(xend=x, yend=0), color = "grey53") +
              geom_point(color = "grey53") +
              #Región de rechazo
              geom_segment(data = binom_reject, aes(xend = x, yend = 0),  color = '#13378f', alpha = 0.4) +
              geom_point(data = binom_reject, color = '#13378f', alpha = 0.4) + 
              #Estadístico
              geom_segment(data = binom_stat, aes(xend  = x, yend = 0, colour = "Estadístico: ")) + 
              geom_point(data = binom_stat, aes(colour = "Estadístico: ")) +
              #Cuantiles
              geom_segment(data = binom_cuantil, aes(xend = x, yend = 0, colour = "Cuantíl: ")) + 
              geom_point(data = binom_cuantil, aes(colour = "Cuantíl: ")) + 
              #Diseño
              scale_colour_manual(values = c("#386df2", "black")) +
              ggtitle(TeX(paste("$Binomial(", Proves$test$parameter,", ", 0.5, ")$"))) +
              general_theme + 
              guides(colour = guide_legend(label.position = "left", title = NULL)) +
              labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") 
          }
        }else{
          req(input$file)
          if(input$SigTPNormal){
            if(input$SignosTestKindOfTestD == "two.sided"){
              plotis_NP$plot <- data_frame(x = c(0,input$RangoX2NP)) %>% 
                ggplot(aes(x = x)) + 
                guides(colour = guide_legend(label.position = "left", title = NULL)) +
                labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") + 
                general_theme +
                stat_function(fun = ~dchisq(.x, df = Proves$test$parameter)) +
                ggtitle(TeX(paste("$\\chi_{1}$"))) +
                geom_segment(aes(x = Proves$statistical, xend = Proves$statistical, y = 0,
                                 yend = dchisq(x = Proves$statistical, df = Proves$test$parameter),
                                 colour = "Estadístico: ")) +
                geom_segment(aes(x = Proves$cuantil, xend = Proves$cuantil, y = 0,
                                 yend = dchisq(x = Proves$cuantil, df = Proves$test$parameter),
                                 colour = "Cuantíl: ")) +
                stat_function(color = NA, fun= ~under_curve(type = "greater",
                                                            alpha = input$alphaTest, x = .x,
                                                            fun = dchisq, fq = qchisq,
                                                            df = Proves$test$parameter),
                              geom = 'area', fill = '#13378f', alpha = 0.2)+
                scale_colour_manual(values = c("#386df2", "black"))
            }
            if(input$SignosTestKindOfTestD != "two.sided"){
              plotis_NP$plot <- data_frame(x = c(input$RangoX1NP,input$RangoX2NP)) %>% 
                ggplot(aes(x = x)) + 
                guides(colour = guide_legend(label.position = "left", title = NULL)) +
                labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") + 
                general_theme +
                stat_function(fun = ~dnorm(.x)) + 
                ggtitle(TeX(paste("$N(0,1)$"))) + 
                geom_segment(aes(x = Proves$statistical, xend = Proves$statistical, y = 0, yend = dnorm(Proves$statistical), colour = "Estadístico: ")) + 
                geom_segment(aes(x = Proves$cuantil, xend = Proves$cuantil, y = 0, yend = dnorm(Proves$cuantil), colour = "Cuantíl: ")) +
                stat_function(color = NA, fun= ~under_curve(type = input$SignosTestKindOfTestD, alpha = input$alphaTest, x = .x, fun = dnorm, fq = qnorm), 
                              geom = 'area', fill = '#13378f', alpha = 0.2)+
                scale_colour_manual(values = c("#386df2", "black"))
            }
          }else{
            binom_reject <- reject_zone_discrete(type = input$SignosTestKindOfTestD, n = Proves$test$parameter, alpha = input$alphaTest, fun = dbinom, fq = qbinom, prob = 0.5, size = Proves$test$parameter)
            data <- tibble(x = seq(0, Proves$test$parameter)[!(seq(0, Proves$test$parameter) %in% binom_reject$x)], y = dbinom(x = x, size = Proves$test$parameter, prob = 0.5))
            binom_stat <- tibble(x = Proves$statistical, y = dbinom(x, size = Proves$test$parameter, prob = 0.5))
            binom_cuantil <- cuantil_assing(type = input$SignosTestKindOfTestD, alpha = input$alphaTest, fun = dbinom, fq = qbinom, prob = 0.5, size = Proves$test$parameter)
            
            plotis_NP$plot <- data %>% ggplot(aes(x = x, y = y)) + 
              geom_segment(aes(xend=x, yend=0), color = "grey53") +
              geom_point(color = "grey53") +
              #Región de rechazo
              geom_segment(data = binom_reject, aes(xend = x, yend = 0),  color = '#13378f', alpha = 0.4) +
              geom_point(data = binom_reject, color = '#13378f', alpha = 0.4) + 
              #Estadístico
              geom_segment(data = binom_stat, aes(xend  = x, yend = 0, colour = "Estadístico: ")) + 
              geom_point(data = binom_stat, aes(colour = "Estadístico: ")) +
              #Cuantiles
              geom_segment(data = binom_cuantil, aes(xend = x, yend = 0, colour = "Cuantíl: ")) + 
              geom_point(data = binom_cuantil, aes(colour = "Cuantíl: ")) + 
              #Diseño
              scale_colour_manual(values = c("#386df2", "black")) +
              ggtitle(TeX(paste("$Binomial(", Proves$test$parameter,", ", 0.5, ")$"))) +
              general_theme + 
              guides(colour = guide_legend(label.position = "left", title = NULL)) +
              labs(x = NULL, y = "Densidad", caption = "Gráfica de densidad de la distribución del estadístico. También se agregan las regiones de rechazo.") 
          }
        }
      }
    }
    validate(need(!is.null(isolate(plotis_NP$plot)), ""))
    isolate(plotis_NP$plot)
  }, bg="transparent")
  
  #Carga de datos-------------------------------------------------------------------------------------------
  data <- reactive({
    validate(need(input$file, 'Por el momento sólo se permiten archivos .csv'))  
    vroom::vroom(input$file$datapath, delim = ",")
  })
  
  #Visualización de datos-----------------------------------------------------------------------------------
  output$fileOutput <- DT::renderDataTable({
    validate(need(input$file, 'Primero debes elegir unos datos'))  
    data()
  })
  
  #Actualización de los valores en los botones de las pruebas-----------------------------------------------
  #Pruebas generales
  observeEvent(input$KindOfTest, {
    if(input$KindOfTest == "Paramétrica"){
      updateRadioGroupButtons(
        session = session, inputId = "NParametricTest",
        selected = NA)
      updateRadioGroupButtons(
        session = session, inputId = "BinomialTest",
        selected = NA)
      updateRadioGroupButtons(
        session = session, inputId = "RangoTest",
        choices = c("U-Mann-Whitney", "Kruskal-Wallis", "Friedman"),
        selected = NA)
      updateRadioGroupButtons(
        session = session, inputId = "VarianzasTest",
        selected = NA)
    }
    else{
      updateRadioGroupButtons(
        session = session, inputId = "ParametricTest",
        selected = NA)
    }
  })
  #Pruebas específicas Paramétricas
  observeEvent(input$ParametricTest,{
    if(input$ParametricTest == 'T-test'){
      updateRadioGroupButtons(
        session = session, inputId = "ZTestKind",
        selected = NA)
    }else{
      updateRadioGroupButtons(
        session = session, inputId = "TTestKind",
        selected = NA)
    }  
  })
  #Actualización de las pruebas (LaTex)
  
  #Verificación para la selección de las variables para las pruebas paramétricas-------------------------
  
  ##TTest
  output$TTest1 <- renderUI({
    req(input$file)
    if(dim(data())[2] < 1){
      validate("Se necesitan al menos una variable para esta prueba")
    }else{
      selectInput("TTest1_aux",label = "Selecciona tu variable",  choices = names(data()))
    }
  })
  output$TTest2_1 <- renderUI({
    req(input$file)
    if(dim(data())[2] < 2){
      validate("Se necesitan al menos dos variables para esta prueba")
    }else{
      selectInput("TTest2_1_aux",label = "Selecciona tu variable",  choices = names(data()))
    }
  })
  output$TTest2_2 <- renderUI({
    req(input$file)
    if(dim(data())[2] < 2){
      validate()
    }else{
      selectInput("TTest2_2_aux",label = "Selecciona tu variable",  choices = names(data()) %rc% input$TTest2_1_aux)
    }
  })
  ##ZTest
  output$ZTest1 <- renderUI({
    req(input$file)
    if(dim(data())[2] < 1){
      validate("Se necesitan al menos una variable para esta prueba")
    }else{
      selectInput("ZTest1_aux",label = "Selecciona tu variable",  choices = names(data()))
    }
  })
  output$ZTest2_1 <- renderUI({
    req(input$file)
    if(dim(data())[2] < 2){
      validate("Se necesitan al menos dos variables para esta prueba")
    }else{
      selectInput("ZTest2_1_aux",label = "Selecciona tu variable",  choices = names(data()))
    }
  })
  output$ZTest2_2 <- renderUI({
    req(input$file)
    if(dim(data())[2] < 2){
      validate()
    }else{
      selectInput("ZTest2_2_aux",label = "Selecciona tu variable",  choices = names(data()) %rc% input$ZTest2_1_aux)
    }
  })
  ##Shapiro
  output$SWTest1 <- renderUI({
    req(input$file)
    if(dim(data())[2] < 1){
      validate("Se necesitan al menos una variable para esta prueba")
    }else{
      selectInput("SWTest1_aux",label = "Selecciona tu variable",  choices = names(data()))
    }
  })
  #Binomial
  output$BinomialTestVar <- renderUI({
    req(input$file)
    if(dim(data())[2] < 1){
      validate("Se necesitan al menos una variable para esta prueba")
    }else{
      selectInput("BinomialTestVar_aux",label = "Selecciona tu variable",  choices = names(data()))
    }
  })
  #Signos
  output$SigTPvar_1 <- renderUI({
    req(input$file)
    if(dim(data())[2] < 2){
      validate("Se necesitan al menos dos variables para esta prueba")
    }else{
      selectInput("SigTPvar_1_aux",label = "Selecciona tu variable",  choices = names(data()))
    }
  })
  output$SigTPvar_2 <- renderUI({
    req(input$file)
    if(dim(data())[2] < 2){
      validate()
    }else{
      selectInput("SigTPvar_2_aux",label = "Selecciona tu variable",  choices = names(data()) %rc% input$SigTPvar_1_aux)
    }
  })
  
  #Summary de las pruebas----------------------------------------------------------------
  output$summaryP <- renderPrint({
    req(input$file)
    validate(need(input$ParametricTest %in% c("Z-test", "T-test", "Shapiro-Wilks"), "Selecciona un tipo de prueba Paramétrica"))
    if(input$ParametricTest == "Z-test"){
      validate(need(input$ZTestKind %in% c("1 muestra", "2 muestras"), "Selecciona un tipo de prueba Z"))
      if(input$ZTestKind == '1 muestra'){
        validate(need(!is.na(input$ZTest1Sigma), "Ingresa la desviación estandar"))
        var1 <- data()[[input$ZTest1_aux]]
        prueba <- BSDA::z.test(var1, mu = input$ZTest1Mu, sigma.x = input$ZTest1Sigma, alternative = input$ZTestKindOfTest1, conf.level = 1-input$alphaTest)
        Proves$test <- prueba
        Proves$statistical <- prueba$statistic
        Proves$p_value <- prueba$p.value
        if(input$ZTestKindOfTest1 == "greater"){
          Proves$cuantil <- qnorm(p = alpha(input$ZTestKindOfTest1, alpha = input$alphaTest), lower.tail = FALSE)
        }else{
          Proves$cuantil <- qnorm(p = alpha(input$ZTestKindOfTest1, alpha = input$alphaTest))
        }
      }
      if(input$ZTestKind == '2 muestras'){
        validate(need(!is.na(input$ZTest2_1Sigma), "Ingresa la desviación estandar"))
        validate(need(!is.na(input$ZTest2_2Sigma), "Ingresa la desviación estandar"))
        var1 <- data()[[input$ZTest2_1_aux]]
        var2 <- data()[[input$ZTest2_2_aux]]
        prueba <- BSDA::z.test(x = var1, y = var2, sigma.x = input$ZTest2_1Sigma, sigma.y = input$ZTest2_2Sigma, alternative = input$ZTestKindOfTest2, conf.level = 1-input$alphaTest)
        Proves$test <- prueba
        Proves$statistical <- prueba$statistic
        Proves$p_value <- prueba$p.value
        if(input$ZTestKindOfTest2 == "greater"){
          Proves$cuantil <- qnorm(p = alpha(input$ZTestKindOfTest2, alpha = input$alphaTest), lower.tail = FALSE)
        }else{
          Proves$cuantil <- qnorm(p = alpha(input$ZTestKindOfTest2, alpha = input$alphaTest))
        }
      }
    }
    if(input$ParametricTest == "T-test"){
      validate(need(input$TTestKind %in% c("1 muestra", "2 muestras"), "Selecciona un tipo de prueba T"))
      if(input$TTestKind == '1 muestra'){
        var1 <- data()[[input$TTest1_aux]]
        prueba <- stats::t.test(var1, mu = input$TTest1Mu , alternative = input$TTestKindOfTest1, conf.level = 1-input$alphaTest)
        Proves$test <- prueba
        Proves$statistical <- prueba$statistic
        Proves$p_value <- prueba$p.value
        if(input$TTestKindOfTest1 == "greater"){
          Proves$cuantil <- qt(p = alpha(input$TTestKindOfTest1, alpha = input$alphaTest), prueba$parameter, lower.tail = FALSE)
        }else{
          Proves$cuantil <- qt(p = alpha(input$TTestKindOfTest1, alpha = input$alphaTest), prueba$parameter)
        }
      }
      if(input$TTestKind == '2 muestras'){
        var1 <- data()[[input$TTest2_1_aux]]
        var2 <- data()[[input$TTest2_2_aux]]
        prueba <- stats::t.test(var1, var2, alternative = input$TTestKindOfTest2, paired = input$TTest2Par, var.equal = input$TTest2Var, conf.level = 1-input$alphaTest)
        Proves$test <- prueba
        Proves$statistical <- prueba$statistic
        Proves$p_value <- prueba$p.value
        if(input$TTestKindOfTest2 == "greater"){
          Proves$cuantil <- qt(p = alpha(input$TTestKindOfTest2, alpha = input$alphaTest), prueba$parameter, lower.tail = FALSE)
        }else{
          Proves$cuantil <- qt(p = alpha(input$TTestKindOfTest2, alpha = input$alphaTest), prueba$parameter)
        }
      }
    }
    if(input$ParametricTest == "Shapiro-Wilks"){
      var1 <- data()[[input$SWTest1_aux]]
      prueba <- stats::shapiro.test(var1)
      Proves$test <- prueba
      Proves$statistical <- prueba$statistic
      Proves$p_value <- prueba$p.value
      if(dim(data())[1]>50){
        Proves$cuantil <- (sha %>% select("n", "0.05") %>% filter(n == 50))[[1,2]]
      }else{
        Proves$cuantil <- (sha %>% select("n", "0.05") %>% filter(n == dim(data())[1]))[[1,2]]
      }
    }
    isolate(Proves$test)
  })
  output$summaryNP <- renderPrint({
    validate(need(input$NParametricTest %in% c("Binomial", "Rango", "Varianzas"), "Selecciona un tipo de prueba No Paramétrica"))
    if(input$NParametricTest == "Binomial"){
      validate(need(input$BinomialTest %in% c("Proporciones", "Cuantiles", "Signos", "McNemar","Cox Stuart"), "Selecciona un tipo de prueba Binomial"))
      if(input$BinomialTest == "Proporciones"){
        validate(need(input$BinomialTestInput, "Selecciona que como deseas realizar tu prueba"))
        if(input$BinomialTestInput == "Datos"){
          req(input$file)
          var1 <- data()[[input$BinomialTestVar_aux]]
          successes <- sum(var1 == 1)
          n <- length(var1)
          x <- c(successes, n - successes)
          #Prueba
          prueba <- NULL
          if(input$BTPNormal){
            prueba <- prop.test(x = successes, n = n, p = input$BTpD, alternative = input$BinomialTestKindOfTestD, conf.level = 1- input$alphaTest)
          }else{
            prueba <- binom.test(x = x, p = input$BTpD, alternative = input$BinomialTestKindOfTestD, conf.level = 1- input$alphaTest)
          }
          Proves$test <- prueba
          
          #Estadístico
          if(input$BTPNormal & input$BinomialTestKindOfTestD != "two.sided"){
            Proves$statistical <- sqrt(prueba$statistic)
          } else Proves$statistical <- prueba$statistic
          #P-value
          Proves$p_value <- prueba$p.value
          #Cuantil
          if(input$BTPNormal){
            if(input$BinomialTestKindOfTestD == "two.sided"){
              Proves$cuantil <- qchisq(input$alphaTest, df = prueba$parameter, lower.tail = F)
            }else{
              Proves$cuantil <- qnorm(input$alphaTest, lower.tail = (input$BinomialTestKindOfTestD == "less"))
            }
          }else{
            if(input$BinomialTestKindOfTestD == "two.sided"){
              Proves$cuantil <- c(qbinom(p = alpha(input$BinomialTestKindOfTestD, alpha = input$alphaTest), size =  prueba$parameter, prob = input$BTpD), qbinom(p = alpha(input$BinomialTestKindOfTestD, alpha = input$alphaTest), size =  prueba$parameter, prob = input$BTpD, lower.tail = F))
            }
            else{
              Proves$cuantil <- qbinom(p = input$alphaTest, size =  prueba$parameter, prob = input$BTpD, lower.tail = (input$BinomialTestKindOfTestD == "less"))
            }
          }
        }
        if(input$BinomialTestInput == "Manual"){
          validate(need(input$BTtrials , "Ingresa el número de éxitos"))
          validate(need(input$BTn , "Ingresa el tamaño de la muestra"))
          successes <- input$BTtrials
          n <- input$BTn
          x <- c(successes, n - successes)
          prueba <- NULL
          if(input$BTPNormal){
            prueba <- prop.test(x = successes, n = n, p = input$BTpM, alternative = input$BinomialTestKindOfTestM, conf.level = input$alphaTest)
          }else{
            prueba <- binom.test(x = x, p = input$BTpM, alternative = input$BinomialTestKindOfTestM, conf.level = input$alphaTest)
          }
          Proves$test <- prueba
          if(input$BTPNormal & input$BinomialTestKindOfTestM != "two.sided"){
            Proves$statistical <- sqrt(prueba$statistic)
          } else Proves$statistical <- prueba$statistic
          Proves$p_value <- prueba$p.value
          if(input$BTPNormal){
            if(input$BinomialTestKindOfTestM == "two.sided"){
              Proves$cuantil <- qchisq(input$alphaTest, df = prueba$parameter, lower.tail = F)
            }else{
              Proves$cuantil <- qnorm(input$alphaTest, lower.tail = (input$BinomialTestKindOfTestM == "less"))
            }
          }else{
            if(input$BinomialTestKindOfTestM == "two.sided"){
              Proves$cuantil <- c(qbinom(p = alpha(input$BinomialTestKindOfTestM, alpha = input$alphaTest), size =  prueba$parameter, prob = input$BTpM), qbinom(p = alpha(input$BinomialTestKindOfTestM, alpha = input$alphaTest), size =  prueba$parameter, prob = input$BTpM, lower.tail = F))
            }else{
              Proves$cuantil <- qbinom(p = input$alphaTest, size =  prueba$parameter, prob = input$BTpM, lower.tail = (input$BinomialTestKindOfTestM == "less"))
            }
          }
        }
      }
      if(input$BinomialTest == "Cuantiles"){}
      if(input$BinomialTest == "Signos"){
        validate(need(input$SignosTestInput, "Selecciona que como deseas realizar tu prueba"))
        if(input$SignosTestInput == "Datos"){
          req(input$file)
          var1 <- data()[[input$SigTPvar_1_aux]]
          var2 <- data()[[input$SigTPvar_2_aux]]
          table <- tibble(x = var1, y = var2) %>% filter(x!=y)
          n <- dim(table)[1]
          estatistic <- dim(table %>% filter(x>y))[1]
          #Prueba
          prueba <- NULL
          if(input$SigTPNormal){
            prueba <- prop.test(x = estatistic, n = n, p = 0.5, alternative = input$SignosTestKindOfTestD, conf.level = 1 - input$alphaTest)
          }else{
            prueba <- binom.test(x = estatistic, n = n, p = 0.5, alternative = input$SignosTestKindOfTestD, conf.level = 1 - input$alphaTest)
          }
          Proves$test <- prueba
          
          #Estadístico
          if(input$SigTPNormal & input$SignosTestKindOfTestD != "two.sided"){
            Proves$statistical <- sqrt(prueba$statistic)
          } else Proves$statistical <- prueba$statistic
          #P-value
          Proves$p_value <- prueba$p.value
          #Cuantil
          if(input$SigTPNormal){
            if(input$SignosTestKindOfTestD == "two.sided"){
              Proves$cuantil <- qchisq(input$alphaTest, df = prueba$parameter, lower.tail = F)
            }else{
              Proves$cuantil <- qnorm(input$alphaTest, lower.tail = (input$SignosTestKindOfTestD == "less"))
            }
          }else{
            if(input$SignosTestKindOfTestD == "two.sided"){
              Proves$cuantil <- c(qbinom(p = alpha(input$SignosTestKindOfTestD, alpha = input$alphaTest), size =  prueba$parameter, prob = 0.5),qbinom(p = alpha(input$SignosTestKindOfTestD, alpha = input$alphaTest), size =  prueba$parameter, prob = 0.5, lower.tail = FALSE))
            }else{
              Proves$cuantil <- qbinom(p = input$alphaTest, size =  prueba$parameter, prob = 0.5, lower.tail = (input$SignosTestKindOfTestD == "less"))
            }
          }
        }
        if(input$SignosTestInput == "Manual"){
          validate(need(input$SigTGreater , "Ingresa la información necesaria"))
          validate(need(input$SigTLess , "Ingresa la información necesaria"))
          estatistic <- input$SigTGreater
          n <- input$SigTGreater + input$SigTLess
          prueba <- NULL
          if(input$SigTPNormal){
            prueba <- prop.test(x = estatistic, n = n, p = 0.5, alternative = input$SignosTestKindOfTestM, conf.level = 1- input$alphaTest)
          }else{
            prueba <- binom.test(x = estatistic, n = n, p = 0.5, alternative = input$SignosTestKindOfTestM, conf.level = 1- input$alphaTest)
          }
          Proves$test <- prueba
          if(input$SigTPNormal & input$SignosTestKindOfTestM != "two.sided"){
            Proves$statistical <- sqrt(prueba$statistic)
          } else Proves$statistical <- prueba$statistic
          Proves$p_value <- prueba$p.value
          if(input$SigTPNormal){
            if(input$SignosTestKindOfTestM == "two.sided"){
              Proves$cuantil <- qchisq(input$alphaTest, df = prueba$parameter, lower.tail = F)
            }else{
              Proves$cuantil <- qnorm(input$alphaTest, lower.tail = (input$SignosTestKindOfTestM == "less"))
            }
          }else{
            if(input$SignosTestKindOfTestM == "two.sided"){
              Proves$cuantil <- c(qbinom(p = alpha(input$SignosTestKindOfTestM, alpha = input$alphaTest), size =  prueba$parameter, prob = 0.5), qbinom(p = alpha(input$SignosTestKindOfTestM, alpha = input$alphaTest), size =  prueba$parameter, prob = 0.5, lower.tail = FALSE))
            }else{
              Proves$cuantil <- qbinom(p = input$alphaTest, size =  prueba$parameter, prob = 0.5, lower.tail = (input$SignosTestKindOfTestM == "less"))
            }
          }
        }
      }
      if(input$BinomialTest == "McNemar"){}
      if(input$BinomialTest == "Cox"){}
      if(input$BinomialTest == "Stuart"){}
    }
    if(input$NParametricTest == "Rango"){
      validate(need(input$RangoTest %in% c("U-Mann-Whitney", "Kruskal-Wallis", "Friedman"), "Selecciona un tipo de prueba de Rango"))
      if(input$input$RangoTest == "U-Mann-Whitney"){}
      if(input$input$RangoTest == "Kruskal-Wallis"){}
      if(input$input$RangoTest == "Friedman"){}
    }
    if(input$NParametricTest == "Varianzas"){
      validate(need(input$VarianzasTest %in% c("Fisher", ">2"), "Selecciona un tipo de prueba de varianza"))
      if(input$VarianzasTest == "Fisher"){}
      if(input$VarianzasTest == ">2"){} 
    }
    isolate(Proves$test)
  })
}
shinyApp(ui, server)
