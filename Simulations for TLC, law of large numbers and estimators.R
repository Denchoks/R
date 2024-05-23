################################################################################ 
#Regression Models
setwd("C:/Users/56976/Desktop/Ayudantia Econometria I") #Set Working Directory
getwd() #Get Working Directory
################################################################################
rm(list=ls()) 
################################################################################ 
#Librerías 
library(ggplot2) 
library(patchwork) 
################################################################################
####INSESGAMIENTO Y EFICIENCIA####
#Para la media poblacional 
#Fijar el inicio del generador de números aleatorios 
set.seed(1) 

#Parámetros para la población 
mu<- 100
sigma<- 25
n<-3 #Vamos a variarla más adelante

#Tomamos la muestra 
x<-rnorm(n,mu,sigma)

#Se calculan los estadísticos
m1<-mean(x)
m1

m2<-median(x)
m2

#Este proceso anterior lo vamos a replicar varias veces.

#Vectores numéricos vacíos para almacenar los estadísticos.
est1<-numeric()
est2<-numeric()

for (i in 1:10000){
  x<-rnorm(n,mu,sigma)
  
  #Se calculan los estadísticos 
  m1<-mean(x)
  m1
  
  m2<-median(x)
  m2
  
  est1<-c(est1,m1) #agregammos al vector los valores de m1 (medias)
  est2<-c(est2,m2) #agregammos al vector los valores de m2 (medianas)
}

#Estadísticos
str(est1) #str() permite ver cómo se han almacenado los resultados.
str(est2) 

 
#Calculando las estimaciones aproximadas para los estadísticos

mean(est1); var(est1)
mean(est2); var(est2)


#Estudio del sesgo est 1
Sesgo<-mean(est1)- mu
Sesgo

#Estudio del sesgo est 2
Sesgo<-mean(est2)- mu
Sesgo


#Por lo tanto se cumple que:
#i) E(Estimador)=u #Se cumple para ambos aproximadamente.

#ii) 
#Es mejor el estimador de la media muestral, ya que tiene menor 
#varianza.

#Si cambiamos n vamos a seguir obteniendo los mismo resultados, es decir
#estimadores insesgados y que el estimador de la media muestral es más eficiente.
 
####DISTRIBUCIÓN MUESTRAL####
#Y ~ N(5, 25).
#Y_pro ~ N (µ_Y , σ2_Y/n)

#a)
# Establecer tamaño y numero de muestras 
n	<- 10000
reps <- 10000
mu<- 5
sd <- 5
set.seed(1)
samples <- replicate(reps, rnorm(n,mu,sd))

#b)
sample.avgs <- colMeans(samples)
head(sample.avgs)

#c)

ggplot() +
  # Histograma de las estimaciones
  geom_histogram(aes(x = sample.avgs, y = ..density.., fill = "Distribución"), 
                 bins =65,
                 color = "black", alpha = 0.7) +
  # Línea de la función de densidad
  stat_function(aes(color = "Distribución"), fun = dnorm, args = list(mean = mu, 
                                                   sd = sd/sqrt(n)), size = 1) +
  theme_apa() +
  # Etiquetas
  labs(
    title = "Distribución de las Medias Muestrales"~bar(Y),
    subtitle = "Para muestras grandes (Simulación con 100.000.000 de observaciones)",
    x = bquote(bar(Y)),
    y = ""
  ) +
  # Leyenda
  scale_color_manual(name = "Empírico", values = "black",
                     labels = "Función de Densidad Empírica") +
  scale_fill_manual(name = "Estimado", values = "lightblue", 
                    labels = "Histograma de las medias estimadas") +
  theme(legend.position = "bottom")

####CONVERGENCIA DE PROBABILIDAD Y LEY DE LOS GRANDES NÚMEROS####
# lanzamiento de un dado de 8 lados, con un dado justo.
# Fijar semilla
set.seed(2)

# Número de lanzamientos del dado
N <- 10000
resultados <- sample(1:8, N, replace = TRUE)

# Calcular R_n (proporción de caras) para 1:N
S <- cumsum(resultados == 8)
R <- S/(1:N) #Probabilidad de que salga un 8 a medida que aumentan los 
#lanzamientos de la moneda
0.125-mean(R)
# Graficar el camino o trayectoria.
plot(R,type = "l",ylim = c(0, 0.2),
     col = "#556B2F", lwd = 2,
     xlab = "n", ylab = "R_n",
     main = " Convergencia a 1/8 de Lanzamientos Repetidos de 
     un Dado de 8 caras")

# Agregar una linea para R_n = 0.125
lines(c(0, N), c(1/8, 1/8),
      col = "darkred", lty = 2, lwd = 1)

#Graficar el camino o trayectoria con ggplot
 Data <- data.frame(n = 1:N, R = R)

 ggplot(Data, aes(x = n, y = R)) +
  geom_line(color = "midnightblue", size = 0.8) +
 geom_hline(yintercept = 1/8, linetype = "dashed", color = "darkred",
            size = 1) +
 ylim(0, 0.2) +theme_classic()+
 labs( title = "Convergencia a la probabilidad de 0.125 cuando n aumenta",
    subtitle = "10.000 lanzamientos repetidos de un dado de 8 caras",
    x ="n",
    y ="Probabilidad de que salga un 8")+
   theme(plot.title = element_text(face = "bold"))
 
####TEOREMA DEL LÍMITE CENTRAL####
#Area de los graficos (4)
par(mfrow = c(2, 2))

# Establecer el nunmero de repeticiones y el tamaño de la muestra

reps <- 10000
sample.sizes	<- c(5, 20, 75, 200)

# Establecer la semilla para reproducir el ejercicio
set.seed(10)

#  loop externo (loop para los tamaños de la muestra)

for (n in sample.sizes) {
  
  samplemean <- rep(0, reps) # inicializar el vector de medias #muestrales 
  
  stdsamplemean <- rep(0, reps) # inicializar el vector de las #medias 
                                  #muestrales estandarizadas  
  
  # loop interno (loop para las repeticiones)   
  for (i in 1:reps) {
    x <- rpois(n,0.5)
    samplemean[i] <- mean(x)
    stdsamplemean[i] <- sqrt(n)*(mean(x) - 0.5)/sqrt(0.5)
  }
  
 #graficar el histograma y superponer la densidad N(0,1) en cada iteración 
  
  hist(stdsamplemean, 
       col = "steelblue", 
       freq = FALSE, 
       breaks = 40,
       xlim = c(-3, 3), 
       ylim = c(0, 0.8), 
       xlab = "", 
       main = paste("n =", n))
  
  curve(dnorm(x), 
        lwd = 2, 
        col = "darkred", 
        add = TRUE)
}  

  

####DISTRIBUCIONES MUESTRALES DE B0 y B1####
set.seed(123)
par(mfrow = c(1, 2))
 
#Datos simulados
N <- 100000
x <- runif(N, min = 0, max = 20)
u <- rnorm(N, sd = 10)

# Regresion Poblacional
y <- -2.2  +  4.4 * x + u

#Simulación para ver si los betas tienden al beta poblacional
Datos <- data.frame(x, y)

#trabajando con el siguiente tamaño de muestra

n <- 10 #ir cambiando en la ayudantía
reps <- 100#ir cambiando en la ayudantía

# Inicializar la matriz de resultados
betas_0_1<- matrix(ncol = 2, nrow = reps)

# loop muestreo y estimacion de los coeficientes
for (i in 1:reps){
  
  sample <- Datos[sample(1:N, n,replace = FALSE), ]
  
  betas_0_1[i, ] <- lm(y ~ x, data = sample)$coefficients
}
mean(betas_0_1[,1]);mean(betas_0_1[,2])

sqrt(var(betas_0_1[,1]));sqrt(var(betas_0_1[,1]))

#Calculo de las varianzas
#Si no se entienden las ecuaciones, entonces debe de revisar el concepto clave 
#4.4 Introducción a la Econometria - Stock & Watson  

#Calcular la varianza de B0 

#Donde Hi=1- (u_{x}/u_{x^2})*x
Hi <- 1 - ((mean(x) / (mean(x^2)))*x)
varB0<- var(Hi * u) / (n* mean(Hi^2)^2 )

# Calcular la varianza de B1
varB1 <- var( ( x - mean(x) ) * u ) / (n * var(x)^2)

 
p1 <-  ggplot() +
  # Histograma de las estimaciones
  geom_histogram(aes(x = betas_0_1[,1], y = ..density..), bins = 20,
                 fill = "lightblue", color = "black", alpha = 0.7) +
   
  stat_function(aes(color = "darkred"),fun = dnorm, args = list(mean =-2.2,
                                         sd = sqrt(varB0)), size = 1) +
   
  stat_function(aes(color = "black"),fun = dnorm, 
                args = list(mean =mean(betas_0_1[,1]),
                 sd = sqrt(var(betas_0_1[,1]))) , size = 1 )+
   
  theme_apa()+
  labs(subtitle = "Para n=100.000",
    title = bquote(Distribución ~ para ~ hat(beta)[.(0)] ~ Estimado),
    x = bquote(hat(beta)[.(0)]),
    y = ""
  ) + 
   scale_color_manual(name = bquote(Para~hat(beta)[.(0)] ),
                      values = c("darkred", "black"),
                      labels = c("Original","Estimada"))+  
  theme(legend.position = "bottom")
p2 <-  ggplot() +
  # Agregar el histograma
  geom_histogram(aes(x = betas_0_1[,2], y = ..density..), bins = 20,
                 fill = "lightblue", color = "black", alpha = 0.7) +
  
  stat_function(aes(color = "darkred"),fun = dnorm, args = list(mean = 4.4,
                                         sd = sqrt(varB1)), size = 1.5) +
  
  stat_function(aes(color = "black"),fun = dnorm,
        args = list(mean = mean(betas_0_1[,2]), sd = sqrt(var(betas_0_1[,2]))),
                                                         size = 1.5)+
  theme_apa() +
   labs(subtitle = "Para n=100.000",
 title = bquote(Distribución ~ para ~ hat(beta)[.(1)] ~ Estimado),
    x = bquote(hat(beta)[.(1)]),
    y = ""
  )+
  scale_color_manual( name=bquote(Para~hat(beta)[.(1)] ),
                      values = c("darkred", "black"),
      labels = c("Original","Estimada"))+  theme(legend.position = "bottom")


wrap_plots(p1, p2, nrow = 1,ncol = 2)
 
#Para diferentes tamaños demuestra 
# establecer semilla para reproducibilidad
set.seed(123)

# repeticiones y el vector de los tamaños de muestra
reps <- 1500

n <- c(10, 20, 50, 100) #c(100, 250, 1000, 5000)

# inicializar la matriz de resultados
fit <- matrix(ncol = 2, nrow = reps)
mean_betas0 <- numeric(length(n))
mean_betas1 <- numeric(length(n))
ds_betas0 <-numeric(length(n))
  ds_betas1 <-numeric(length(n))
# panel de graficos
par(mfrow = c(2, 2))

for (j in 1:length(n)) {
  
  for (i in 1:reps){
    
    sample <- Datos[sample(1:N, n[j]), ]
    
    fit[i,] <- lm(y ~ x, data = sample)$coefficients
 
  }
  mean_betas0[j] <- mean(fit[, 1])
  mean_betas1[j] <- mean(fit[, 2])
  ds_betas0[j] <- sqrt(var((fit[, 1])))
  ds_betas1[j] <- sqrt(var((fit[, 2])))
  
        hist(fit[ ,2],
             col = "lightblue" , breaks = 30,
             main = paste("n=", n[j]),
             xlab = bquote(hat(beta)[1]),probability = T)
  
  lines(density(fit[ ,2]) ,
       col = j,add = T)
}