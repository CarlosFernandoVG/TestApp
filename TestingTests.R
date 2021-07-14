library(shiny)
"Ingresa tus datos separados por coma para cada muestra"
ui <- fluidPage(
  textInput("a","Muestra 1"),
  textInput("b","Muestra 2"),
  
  verbatimTextOutput("summarya"),
  verbatimTextOutput("summaryb"),
  
  verbatimTextOutput("operations"),
  verbatimTextOutput("Extra1"),
  verbatimTextOutput("Extra2"),
  verbatimTextOutput("Len1"),
  verbatimTextOutput("Len2"),
  
  verbatimTextOutput("T1")
  
)


server <- function(input,output){
  output$summarya <- renderText(print(input$a))
  output$summaryb <- renderText(print(input$b))
  output$operations <- renderPrint({
    x <- as.numeric(str_extract_all(input$a, pattern = "[\\d|\\d\\.]+")[[1]])
    g <- str_extract_all(input$b, pattern = "[^\\s|,]+")[[1]]
    kruskal.test(x = x, g = g)
    })
  #output$operations <- renderText(str_extract_all(input$b, pattern = "[^\\s|,]+")[[1]])
  output$Extra1 <- renderText(is.vector(as.numeric(str_extract_all(input$a, pattern = "[\\d|\\d\\.]+")[[1]])))
  output$Extra2 <- renderText(is.vector(str_extract_all(input$b, pattern = "[^\\s|,]+")[[1]]))
  output$Len1 <- renderText(length(as.numeric(str_extract_all(input$a, pattern = "[\\d|\\d\\.]+")[[1]])))
  output$Len2 <- renderText(length(str_extract_all(input$b, pattern = "[^\\s|,]+")[[1]]))
  output$T1 <- renderText({
    aux_m1 <- str_extract_all(input$a, pattern = "[\\d|\\d\\.]+")[[1]]
    length(aux_m1) != 0
    })
}
shinyApp(ui, server)