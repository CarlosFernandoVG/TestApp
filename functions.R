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