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
    if(missing(y) || missing(n)) stop("y and n must be initialized")
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
#Funciones para la prueba de McNemar---------------------------------------------------------------
verify_matrix <- function(m){
  if(!is.na(sum(m))) NULL
  else "Necesitas tener una matriz adecuada antes de continuar"
}
check_leves <- function(var1, var2){
  if(length(levels(as.factor(var1))) == 2 && length(levels(as.factor(var2))) == 2 && levels(as.factor(var1)) == levels(as.factor(var2))) NULL
  else FALSE
}

#Funciones para prueba de suma de rangos----------------------------------------------------
rank_sum <- function(m1, m2, alternative = "two.sided",	significance = 0.05){
  #Cuando se ingresan datos con muestras de distintos tamaños, pueden existir valores perdidos que alteren los resultados (m y n no serán los correctos)
  m1 <- na.omit(m1)
  m2 <- na.omit(m2)
  if(missing(m1) || missing(m2)) stop("x and y must be initialized")
  if(length(m1) == 0 ||  length(m1) == 0) stop("x and y must have length greater than 0")
  empates <- F
  
  names(m1) <- rep("m1", length(m1))
  names(m2) <- rep("m2", length(m2))
  mt <- c(m1, m2)
  all_ranks <- rank(mt)
  
  est <- sum(all_ranks[which(names(all_ranks)=="m1")])
  n <- length(m1)
  m <- length(m2)
  N <- length(mt)
  #Repeticiones
  if(sum(table(mt)>1)>0){
    #Ahora el estadístico tiene una dist. N(0,1)
    est <- (est- (n*((N+1)/2)))/
      (sqrt((n*m/(N*(N-1)))*(sum(all_ranks^2))-
              ((n*m*(N+1)^2)/(4*(N-1)))))
    empates <- T
  }
  #Aquí se guardará la información sobre los cuantiles
  q1 <- NULL
  q2 <- NULL
  if(alternative == "two.sided"){
    #Rechazamos si T<t_(alpha/2) o T>t_1-(alpha/2)
    if(empates){
      q1 <- qnorm(significance/2)
      q2 <- qnorm(significance/2, lower.tail = F)
      p_val <- 2*min(pnorm(est), pnorm(est, lower.tail = F))
    }else{
      #Para los valores de los cuantiles del estadístico Mann-Whitney Test utilizaremos la siguiente relación
      #T_W = T_U+n(n+1)/2 donde X tiene tamaño n; T_U es calculado por la función qwilcox()
      q1 <- qwilcox(significance/2, n, m) + n*(n+1)/2
      q2 <- n*(n+m+1)-q1
      p_val <- 2*pnorm((est+1/2-n*((N+1)/2))/(sqrt((n*m*(N+1))/(12))))
    }
  }
  if(alternative == "less"){
    if(empates){
      q1 <- qnorm(significance)
      p_val <- pnorm(est)
    }else{
      q1 <- qwilcox(significance, n, m) + n*(n+1)/2
      p_val <- pnorm((est+1/2-n*((N+1)/2))/(sqrt((n*m*(N+1))/(12))))
    }
  }
  if(alternative == "greater"){
    if(empates){
      q2 <- qnorm(significance/2, lower.tail = F)
      p_val <- pnorm(est, lower.tail = F)
    }else{
      q1 <- qwilcox(significance, n, m) + n*(n+1)/2
      q2 <- n*(n+m+1)-q1
      p_val <- 1- pnorm((est+1/2-n*((N+1)/2))/(sqrt((n*m*(N+1))/(12))))
    }
  }
  names(est) <- "T"
  DNAME <- paste(substitute(x), substitute(y), sep = " and ")
  RVAL <- list(statistic = est,
               p.value = as.numeric(p_val), 
               interval = c(q1, q2), 
               alternative = alternative, method = if(empates) "Rank-Sum test with ties" else "Rank-Sum test", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}
#Funciones para prueba de varianzas en poblaciones por rangos de Conover---------------------------------------------------
rank_var_2 <- function(m1, m2, alternative = "two.sided",	significance = 0.05, mu1 = NULL, mu2 = NULL, exact.pval = FALSE){
  #Cuando se ingresan datos con muestras de distintos tamaños, pueden existir valores perdidos que alteren los resultados (m y n no serán los correctos)
  m1 <- na.omit(m1)
  m2 <- na.omit(m2)
  if((length(m1)<3) | (length(m2)<3)) stop("Samples must have at least three elements")
  if(missing(m1) || missing(m2)) stop("x and y must be initialized")
  if(length(m1) == 0 ||  length(m1) == 0) stop("x and y must have length greater than 0")
  empates <- F
  
  n <- length(m1)
  m <- length(m2)
  N <- n+m
  
  #Promedios
  X_barra <- NULL
  Y_barra <- NULL
  
  if(is.null(mu1)) X_barra <- mean(m1) else X_barra <- mu1
  if(is.null(mu2)) Y_barra <- mean(m2) else Y_barra <- mu2
  
  #Modificaciones de las variables
  U <- abs(m1-X_barra)
  V <- abs(m2-Y_barra)
  
  #Asignación de nombres
  names(U) <- rep("U", length(U))
  names(V) <- rep("V", length(V))
  
  #Asignación de rangos
  rangos <- rank(c(U,V))
  
  #Estadístico
  est <- sum(rangos[which(names(rangos)=="U")]^2)
  
  #Empates
  if(sum(table(rangos)>1)>0){
    R4 <- sum(rangos^4)
    R2barra <- mean(rangos^2)
    est <- (est-n*R2barra)/(sqrt((((n*m)/(N*(N-1)))*(R4))-(((n*m)/(N-1))*R2barra^2)))
    empates <- T
  }
  
  #Si no se tienen empates y n<=10 y m<=10
  #low_quanitles <- readxl::read_xls("squared_ranks_critical_values.xls")
  #quantiles <- low_quanitles %>% dplyr::filter(n == length(m1) & p == significance) %>% as_vector()
  #quantil <- quantiles[length(m2)]
  
  #Aquí se guardará la información sobre los cuantiles
  q1 <- NULL
  q2 <- NULL
  if(alternative == "two.sided"){
    #Rechazamos si T<t_(alpha/2) o T>t_1-(alpha/2)
    if(n<=10 & m<=10 & empates == FALSE){
      low_quanitles <- readxl::read_xls("squared_ranks_critical_values.xls")
      #La tabla solo proporciona algunas probabilidades
      unique_probs <- low_quanitles$p %>% unique()
      if(!(significance/2 %in% unique_probs)) stop("The available probabilities are only 0.005 0.010 0.025 0.050 0.100 0.900 0.950 0.975 0.990 0.995")
      q1 <- (low_quanitles %>% dplyr::filter(n == length(m1) & p == significance/2) %>% as_vector())[length(m2)]
      q2 <- (low_quanitles %>% dplyr::filter(n == length(m1) & p == 1-significance/2) %>% as_vector())[length(m2)]
      if(exact.pval){
        #Filtramos la información que requerimos de acuerdo a los parámetros del estadístico (n y m)
        possible_values <- (low_quanitles %>% dplyr::filter(n == length(m1)))[length(m2)]
        #Para facilidad del filtro
        colnames(possible_values) <- "Q"
        p_val <- 2*min(sum(unique_probs[1:(possible_values %>% filter(Q <= est) %>% dim())[1]]), sum(unique_probs[1:(possible_values %>% filter(Q >= est) %>% dim())[1]]))
        
      }else{
        p_val <- 2*min(pnorm((est-n*(N+1)*(2*N+1)/6)/(sqrt(m*n*(N+1)*(2*N+1)*(8*N+11)/180))), pnorm((est-n*(N+1)*(2*N+1)/6)/(sqrt(m*n*(N+1)*(2*N+1)*(8*N+11)/180)), lower.tail = F))
      }
    }else{
      q1 <- qnorm(significance/2)
      q2 <- qnorm(significance/2, lower.tail = F)
      p_val <- 2*min(pnorm(est), pnorm(est, lower.tail = F))
    }
  }
  
  if(alternative == "less"){
    if(n<=10 & m<=10 & empates == FALSE){
      low_quanitles <- readxl::read_xls("squared_ranks_critical_values.xls")
      #La tabla solo proporciona algunas probabilidades
      unique_probs <- low_quanitles$p %>% unique()
      if(!(significance %in% unique_probs)) stop("The available probabilities are only 0.005 0.010 0.025 0.050 0.100 0.900 0.950 0.975 0.990 0.995")
      
      q1 <- (low_quanitles %>% dplyr::filter(n == length(m1) & p == significance) %>% as_vector())[length(m2)]
      if(exact.pval){
        #Filtramos la información que requerimos de acuerdo a los parámetros del estadístico (n y m)
        possible_values <- (low_quanitles %>% dplyr::filter(n == length(m1)))[length(m2)]
        #Para facilidad del filtro
        colnames(possible_values) <- "Q"
        p_val <- sum(unique_probs[1:(possible_values %>% filter(Q <= est) %>% dim())[1]])
      }else{
        p_val<- pnorm((est-n*(N+1)*(2*N+1)/6)/(sqrt(m*n*(N+1)*(2*N+1)*(8*N+11)/180)))
      }
    }else{
      q1 <- qnorm(significance)
      p_val <- pnorm(est)
    }
  }
  
  if(alternative == "greater"){
    if(n<=10 & m<=10 & empates == FALSE){
      low_quanitles <- readxl::read_xls("squared_ranks_critical_values.xls")
      #La tabla solo proporciona algunas probabilidades
      unique_probs <- low_quanitles$p %>% unique()
      if(!(significance/2 %in% unique_probs)) stop("The available probabilities are only 0.005 0.010 0.025 0.050 0.100 0.900 0.950 0.975 0.990 0.995")
      
      q2 <- (low_quanitles %>% dplyr::filter(n == length(m1) & p == 1-significance) %>% as_vector())[length(m2)]
      if(exact.pval){
        #Filtramos la información que requerimos de acuerdo a los parámetros del estadístico (n y m)
        possible_values <- (low_quanitles %>% dplyr::filter(n == length(m1)))[length(m2)]
        #Para facilidad del filtro
        colnames(possible_values) <- "Q"
        p_val <- sum(unique_probs[1:(possible_values %>% filter(Q >= est) %>% dim())[1]])
      }else{
        p_val<- pnorm((est-n*(N+1)*(2*N+1)/6)/(sqrt(m*n*(N+1)*(2*N+1)*(8*N+11)/180)), lower.tail = F)
      }
    }else{
      q2 <- qnorm(significance/2, lower.tail = F)
      p_val <- pnorm(est, lower.tail = F)
    }
  }
  
  names(est) <- "T"
  DNAME <- paste(substitute(x), substitute(y), sep = " and ")
  RVAL <- list(statistic = est,
               p.value = as.numeric(p_val), 
               interval = c(q1, q2), 
               alternative = alternative, method = if(empates) "Conover's Two-Sample Squared Ranks Test for Equality of Variance with ties" else "Conover's Two-Sample Squared Ranks Test for Equality of Variance", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
  
}
rankVar.test <- function(m, g, mu = NULL, significance = 0.05){
  m <- na.omit(m)
  g <- na.omit(g)
  if(missing(m) || missing(g)) stop("x and y must be initialized")
  if(length(m) == 0 ||  length(g) == 0) stop("x and y must have length greater than 0")
  empates <- F
  
  lista_groups <- group_list(m, g)
  
  #Ahora le damos nombres a cada elemento, indicando así su grupo de pertenencia.
  for(i in 1:length(lista_groups)){
    names(lista_groups[[i]]) <- rep(unique(g)[i], length(lista_groups[[i]]))
  }
  
  #Ahora necesitamos obtener nuevas observaciones restando a cada dato su media
  medias <- NULL
  if(is.null(mu)){
    #Calculamos la media
    for(element in lista_groups){
      medias <- c(medias, mean(element))
    }
  }else{
    #Por el momento, si se conoce una media, se deben agregar todas.
    #Debemos verificar que se tengan suficientes medias para cada grupo
    if(length(mu) != unique(g)) stop("m must have enough means for each group")
    medias <- mu
  }
  
  #Restamos a cada grupo su media
  lista_group_means <- map2(lista_groups, medias,  ~abs(.x-.y))
  
  #Guardamos todo en un simple vector para obtener un rankeo total
  U <- rank(unlist(lista_group_means))
  est <- NULL
  N <- length(U)
  
  #Si hay empates...
  if(sum(table(U)>1)>0){
    R4 <- sum(U^4)
    S_barra <- sum(U^2)/N
    D2 <- (R4-N*S_barra^2)/(N-1)
    #Nuevos valores con rankeo por grupos (new data by group)
    ndbg <- group_list(U, names(U))
    est <- (sum(((map(ndbg, ~sum(.x^2)) %>% unlist())^2)/(map(ndbg, ~length(.x)) %>% unlist()))-N*S_barra^2)/D2
    empates <- T
  }else{
    D2 <- N*(N+1)*(2*N+1)*(8*N+11)/180
    S_barra <- (N+1)*(2*N+1)/6
    #Nuevos valores con rankeo por grupos (new data by group)
    ndbg <- group_list(U, names(U))
    est <- ((map(ndbg, ~sum(.x)/length(.x)) %>% unlist() %>% sum())-N*S_barra^2)/D2
  }
  
  #Cuantil
  q <- qchisq(p = significance, df = length(unique(g))-1, lower.tail = F)
  p_val <- pchisq(q = est, df = length(unique(g))-1, lower.tail = F)
  
  names(est) <- "T"
  DNAME <- paste(substitute(x), substitute(y), sep = " by ")
  RVAL <- list(statistic = est,
               p.value = as.numeric(p_val), 
               interval = q, 
               alternative = "Some of the population variances are not equal to each other", method = if(empates) "Conover's +Two-Sample Squared Ranks Test for Equality of Variance with ties" else "Conover's +Two-Sample Squared Ranks Test for Equality of Variance", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
  
}
#Función para prueba Chi-squared test for Differences in probabilities 2X2--------------------------------------------------------------------------------
CSDP2X2.test <- function(matriz,	significance = 0.05){
  matriz <- addmargins(matriz)
  N <- matriz[3,3]
  O1 <- matriz[1,1]
  O2 <- matriz[1,2]
  O3 <- matriz[2,1]
  O4 <- matriz[2,2]
  n1 <- matriz[1,3]
  n2 <- matriz[2,3]
  C1 <- matriz[3,1]
  C2 <- matriz[3,2]
  est <- NULL
  q1 <- NULL
  q2 <- NULL
  if(0 %in% colSums(matriz)){
    est <- 0
  }else{
    est <- (sqrt(N)*(O1*O4-O2*O3))/(sqrt(n1*n2*C1*C2))
  }
  if(alternative == "two.sided"){
    q1 <- qnorm(significance/2)
    q2 <- qnorm(significance/2, lower.tail = F)
    p_val <- 2*min(pnorm(est), pnorm(est, lower.tail = F))
  }
  if(alternative == "less"){
    q1 <- qnorm(significance)
    p_val <- pnorm(est)
  }
  if(alternative == "greater"){
    q2 <- qnorm(significance, lower.tail = F)
    p_val <- pnorm(est, lower.tail = F)
  }
  names(est) <- "T"
  
  DNAME <- paste(substitute(S1), substitute(S2), sep = " and ")
  RVAL <- list(statistic = est,
               p.value = as.numeric(p_val), 
               interval = c(q1, q2), 
               alternative = alternative, method = "Chi-squared test for Differences in probabilities 2X2", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}
#Función para realizar prueba de la mediana---------------------------------------------------------
median.test <- function(matriz, significance = 0.05){
  matriz <- addmargins(matriz)
  shape <- dim(matriz)
  N <- matriz[shape[1],shape[2]]
  a <- matriz[1,shape[2]]
  b <- matriz[2,shape[2]]
  est <- (N^2)/(a*b)*sum(matriz[1,1:(shape[2]-1)]^2/matriz[3,1:(shape[2]-1)]) - N*a/b
  q <- qchisq(significance, lower.tail = F, df = (shape[2]-2))
  p_val <- pchisq(est, lower.tail = F,  df = (shape[2]-2))
  names(est) <- "T"
  
  DNAME <- "Different samples"
  RVAL <- list(statistic = est,
               parameter = (shape[2]-2),
               p.value = as.numeric(p_val), 
               interval = q, 
               alternative = alternative, method = "Chi-squared test for Medians", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
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