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