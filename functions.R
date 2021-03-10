#Tema general para las gráficas---------------------------------------------
general_theme <-  theme(legend.position = "bottom",
                        legend.margin=margin(t = 0, unit='cm'),
                        legend.key = element_blank(), #To remove background of the key
                        legend.justification='left',
                        legend.spacing.x = unit(0.5, 'cm'),
                        legend.text = element_text(size = 9.6, face = "bold"),
                        legend.title = element_text(size = 9.6, face = "bold"),
                        panel.background = element_rect(fill = "transparent", colour = NA),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        plot.background = element_rect(fill = "transparent", colour = NA),
                        legend.background = element_rect(fill = "transparent", colour = NA),
                        legend.box.background = element_rect(fill = "transparent", colour = NA),
                        axis.title = element_text(size = 12, face = "bold.italic"),
                        axis.text = element_text(size = 10),
                        axis.line = element_line(colour = "white"),
                        # panel.background = element_rect(fill = "gray97"),
                        text = element_text(size=12, family="Dosis"),
                        plot.title = element_text(hjust = 0.5),
                        plot.caption = element_text(color = "black", face = "italic", hjust = 0))
#Operandos------------------------------------------------
#Remove Character
`%rc%` <- function(vector, string) vector[vector!=string]
#Función para rellenar curva----------------------------------------------------------------------------------
under_curve <- function(type = "two.sided", alpha ,x, fun, fq,...){
  if(type == "two.sided"){
    left <- fq(p = alpha/2, ...)
    right <- fq(p = 1-alpha/2, ...)
    y <- fun(x, ...)
    y[x > left & x < right] <- NA
    return(y)
  }
  if(type == "greater"){
    right <- fq(p = 1-alpha, ...)
    y <- fun(x, ...)
    y[x < right] <- NA
    return(y)
  }
  if(type == "less"){
    left <- fq(p = alpha, ...)
    y <- fun(x, ...)
    y[x > left] <- NA
    return(y)
  }
}
#Alpha----------------------------------------------------------------------------------------------------
alpha <-  function(alternative, alpha = 0.05){
  if(alternative == "two.sided"){
    alpha <- alpha/2
  }
  alpha
}
#Función para asignar cuantiles --------------------------------------------------------------------------------
cuantil_assing <- function(type = "two.sided", alpha , fun, fq,...){
  if(type == "two.sided"){
    left <- fq(p = alpha/2, ...)
    right <- fq(p = alpha/2, lower.tail = F, ...)
    data <- tibble(x = c(left, right), y  = fun(x, ...))
    return(data)
  }
  if(type == "greater"){
    right <- fq(p = alpha,lower.tail = F, ...)
    data <- tibble(x = right, y  = fun(x, ...))
    return(data)
  }
  if(type == "less"){
    left <- fq(p = alpha, ...)
    data <- tibble(x = left, y  = fun(x, ...))
    return(data)
  }
}
#Función para la zona de rechazo discreta-----------------------------------------------------------------------
reject_zone_discrete <- function(n, type = "two.sided", alpha , fun, fq,...){
  if(type == "two.sided"){
    left <- fq(p = alpha/2, ...)
    right <- fq(p = alpha/2, lower.tail = F, ...)
    data <- tibble(x = c(seq(from = 0, to = left), seq(from = right + 1, to = n)))
    data <- data %>% mutate(y = fun(x, ...))
    return(data)
  }
  if(type == "greater"){
    right <- fq(p = alpha,lower.tail = F, ...)
    data <- tibble(x = seq(from = right + 1, to = n))
    data <- data %>% mutate(y = fun(x, ...))
    return(data)
  }
  if(type == "less"){
    left <- fq(p = alpha, ...)
    data <- tibble(x = seq(from = 0, to = left))
    data <- data %>% mutate(y = fun(x, ...))
    return(data)
  }
}
#Funciones para la prueba de cuantiles----------------------------------------------------------
#This function is a modified version of the next functions: https://people.stat.sc.edu/hitchcock/Rexamples518section3_2.txt
quantile.test<-function(x, y, n, xstar = 0, quantile = .5, alternative = "two.sided", conf.level = .95, correct = F){
  if(length(x) != 1){
    n <- length(x)
    T1 <- sum(x <= xstar)
    T2 <- sum(x < xstar)
  }else{
    if(missing(y) && missing(n)) stop("y and n must be initialized")
    if(x < y) stop("y must be less or equal to x")
    T1 <- x
    T2 <- y
  }
  if(alternative=="greater"){
    STATISTIC <- T2
    names(STATISTIC) <- "T2"
    p.value <- pbinom(T2, n, quantile, lower.tail = F)
    # 1-pbinom(T2-1,n,quantile)
    if(correct) p.value <- 1-pnorm(q = (T2 - n*quantile - 0.5)/sqrt(n*quantile*(1-quantile)))
  }
  if(alternative=="less"){
    STATISTIC <- T1
    names(STATISTIC) <- "T1"
    p.value <- pbinom(T1,n,quantile)
    if(correct) p.value <- pnorm(q = (T1 - n*quantile + 0.5)/sqrt(n*quantile*(1-quantile)))
  }
  if (alternative=="two.sided"){
    STATISTIC <- c(T1, T2)
    names(STATISTIC) <- c("T1", "T2")
    p.value <- 2*min(pbinom(q = T2, size = n, prob = quantile, lower.tail = F), pbinom(T1, n, quantile))
    if(correct) p.value <- 2*min(c(pnorm(q = (T1 - n*quantile + 0.5)/sqrt(n*quantile*(1-quantile))), 1-pnorm(q = (T2 - n*quantile - 0.5)/sqrt(n*quantile*(1-quantile)))))
    # 2*min(1-pbinom(T2-1,n,p),pbinom(T1,n,quantile))
  }
  CINT <- quantile.interval(x, n, quantile, conf.level, correct)
  attr(CINT, "conf.level") <- conf.level
  DNAME <- deparse(substitute(x))
  names(x) <- "number of successes"
  names(n) <- "size of the sample"
  names(quantile) <- "quantile"
  RVAL <- list(statistic = STATISTIC, parameter = n, 
               p.value = as.numeric(p.value), null.value = quantile, 
               conf.int = CINT, alternative = alternative, method = "Quantil binomial test", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}

quantile.interval <- function(x, n, quantile = .5, conf.level = .95, correct = F){
  if(length(x) != 1) n <- length(x)
  else if(missing(n)) stop("n must be initialized")
  alpha <- 1-conf.level
  if(correct){
    r <- floor(n*quantile+qnorm(1-conf.level/2)*sqrt(n*quantile*(1-quantile)))
    s <- floor(n*quantile+qnorm(conf.level/2)*sqrt(n*quantile*(1-quantile)))
  }else{
    rmin1 <- qbinom(alpha/2,n,quantile)-1
    r <- rmin1+1
    smin1 <- qbinom(1-alpha/2,n,quantile)
    s <- smin1+1
  }
  alpha1 <- pbinom(r-1,n,quantile)
  alpha2 <- 1-pbinom(s-1,n,quantile)
  if(length(x) != 1) x <- seq(0, n)
  clo <- sort(x)[r]
  chi <- sort(x)[s]
  conf.level <- 1-alpha1-alpha2
  list(quantile = quantile, conf.level = conf.level, r = r, s = s, interval = c(clo,chi))
}
#Funciones para la prueba de McNemar
verify_matrix <- function(m){
  if(!is.na(sum(m))) NULL
  else "Necesitas tener una matriz adecuada antes de continuar"
}
check_leves <- function(var1, var2){
  if(length(levels(as.factor(var1))) == 2 && length(levels(as.factor(var2))) == 2 && levels(as.factor(var1)) == levels(as.factor(var2))) NULL
  else FALSE
}




#Attemps to improve my code--------------------------------------------------------------
  ##Cambiar ejes de las gráficas---------------------------------------------------------
# observeEvent(input$RangoX1NP | input$RangoX2NP,{
#   if(input$NParametricTest == "Binomial"){
#     if(input$BinomialTest == 'Proporciones'){
#       if(input$BinomialTestInput == 'Manual'){
#         validate(need(input$BTtrials , "Ingresa el número de éxitos"))
#         validate(need(input$BTn , "Ingresa el tamaño de la muestra"))
#         if(input$BTPNormal){
#           if(input$BinomialTestKindOfTestM == "two.sided"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#           if(input$BinomialTestKindOfTestM != "two.sided"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#         }else{
#           output$DensityPlot_NParametric <- renderPlot({
#             plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#           }, bg="transparent")
#         }
#       }else{
#         req(input$file)
#         if(input$BTPNormal){
#           if(input$BinomialTestKindOfTestD == "two.sided"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#           if(input$BinomialTestKindOfTestD != "two.sided"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#         }else{
#           output$DensityPlot_NParametric <- renderPlot({
#             plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#           }, bg="transparent")
#         }
#       }
#     }
#     if(input$BinomialTest == 'Cuantiles'){
#       validate(need(input$CuantilTestInput, ""))
#       if(input$CuantilTestInput == 'Manual'){
#         validate(need(input$CuantilTT1 , "Ingresa los datos necesarios"))
#         validate(need(input$CuantilTT2 , "Ingresa los datos necesarios"))
#         validate(need(input$CuantilTN , "Ingresa el tamaño de la muestra"))
#         validate(need(input$CuantilTCuantilM , "Ingresa el cuantil al que deseas realizar la prueba"))
#         if(input$CuanTPNormal){
#           output$DensityPlot_NParametric <- renderPlot({
#             plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#           }, bg="transparent")
#         }else{
#           output$DensityPlot_NParametric <- renderPlot({
#             plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#           }, bg="transparent")
#         }
#       }else{
#         req(input$file)
#         if(input$CuanTPNormal){
#           output$DensityPlot_NParametric <- renderPlot({
#             plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#           }, bg="transparent")
#         }else{
#           output$DensityPlot_NParametric <- renderPlot({
#             plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#           }, bg="transparent")
#         }
#       }
#     }
#     if(input$BinomialTest == 'Signos'){
#       validate(need(input$SignosTestInput, ""))
#       if(input$SignosTestInput == 'Manual'){
#         validate(need(input$SigTGreater , "Ingresa la información necesaria"))
#         validate(need(input$SigTLess , "Ingresa la información necesaria"))
#         if(input$SigTPNormal){
#           if(input$SignosTestKindOfTestM == "two.sided"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#           if(input$SignosTestKindOfTestM != "two.sided"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#         }else{
#           output$DensityPlot_NParametric <- renderPlot({
#             plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#           }, bg="transparent")
#         }
#       }else{
#         req(input$file)
#         if(input$SigTPNormal){
#           if(input$SignosTestKindOfTestD == "two.sided"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#           if(input$SignosTestKindOfTestD != "two.sided"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#         }else{
#           output$DensityPlot_NParametric <- renderPlot({
#             plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#           }, bg="transparent")
#         }
#       }
#     }
#     if(input$BinomialTest == "McNemar"){
#       validate(need(input$MCNemarTestInput, ""))
#       if(input$MCNemarTestInput == "Manual"){
#         validate(verify_matrix(input$McNemarInputMatrix))
#         if(input$MCNemarTPCorrection){
#           if(input$MCNCorrection == "MCNCorrectionCont"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#           if(input$MCNCorrection %in% c("MCNCorrectionExact", "MCNCorrectionBinom")){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#         }else{
#           output$DensityPlot_NParametric <- renderPlot({
#             plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#           }, bg="transparent")
#         }
#       }
#       if(input$MCNemarTestInput == "Datos"){
#         validate(verify_matrix(input$McNemarMatrixDatos))
#         if(input$MCNemarTPCorrection){
#           if(input$MCNCorrection == "MCNCorrectionCont"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#           if(input$MCNCorrection %in% c("MCNCorrectionExact", "MCNCorrectionBinom")){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#         }else{
#           output$DensityPlot_NParametric <- renderPlot({
#             plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#           }, bg="transparent")
#         }
#       }
#     }
#     if(input$BinomialTest == 'Cox Stuart'){
#       validate(need(input$CSTestInput, ""))
#       if(input$CSTestInput == 'Manual'){
#         validate(need(input$CSTGreater , "Ingresa la información necesaria"))
#         validate(need(input$CSTLess , "Ingresa la información necesaria"))
#         if(input$CSTPNormal){
#           if(input$CSTestKindOfTestM == "two.sided"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#           if(input$CSTestKindOfTestM != "two.sided"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#         }else{
#           output$DensityPlot_NParametric <- renderPlot({
#             plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#           }, bg="transparent")
#         }
#       }else{
#         req(input$file)
#         if(input$CSTPNormal){
#           if(input$CSTestKindOfTestD == "two.sided"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#           if(input$CSTestKindOfTestD != "two.sided"){
#             output$DensityPlot_NParametric <- renderPlot({
#               plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#             }, bg="transparent")
#           }
#         }else{
#           output$DensityPlot_NParametric <- renderPlot({
#             plotis_NP$plot + xlim(c(input$RangoX1NP, input$RangoX2NP))
#           }, bg="transparent")
#         }
#       }
#     }
#   }
# },ignoreInit = TRUE)