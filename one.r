install.packages("tidyverse")
library(tidyverse) #ya tiene forecast
#library(forecast) si cargamos da error ¡ ojo
#antes de hacer correr un modelo debes reiniciar R ctrl+shif+f10
X <- c(1,2,3,4,5)
Y <- c(30,35,40,45,50)

model <- lm(Y ~ X)
#view(model)
summary(model)

#inferencia estadistica
set.seed(123)

#distribucion normal
datos <- rnorm(100,mean = 50, sd = 10)

#estadistico esenciales
media <- mean(datos)
desvicionestandar = sd(datos)
leng = length(datos)
#intervalo de confianza
er_estandar = desvicionestandar / sqrt(leng)

z = qnorm(0.95) # valor critico z para 95%
LI = media-z * er_estandar #limite inferior
LS = media+z * er_estandar

cat("intervalo de confianza: [",LI,",",LS,"]\n")

#ejercicio de correlacion

X = rnorm(1000,mean= 100,sd=15)  #esto remplaza a la anterior variable 'X'
#error norm
e = rnorm(1000,mean= 0,sd=5)

Y = 2 * X + e
correlacin = cor(X,Y)
cat("coeficiente de correlacion :",correlacin,"\n")

#ejemplo : analisis de residuales

#creamos un nuevo modelo
modelo2 = lm(Y ~ X)
residuos = residuals(modelo2)

#graficamos ahora 
plot(X, residuos, main = "grafico de residuos", xlab = "X",ylab = "residuos")
abline(h=0, col= "red", lwd=2)

#test de shapiro , normalida de residuos
shapiro.test(residuos)
