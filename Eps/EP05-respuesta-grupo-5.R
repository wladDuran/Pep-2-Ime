library(dplyr)
library(ggpubr)
library(pwr)
library(ggplot2)
options(scipen = 100)

# Enunciado:
# Se sabe que una máquina que envasa detergentes industriales llena bidones con un volumen de producto que 
# sigue una distribución normal con desviación estándar de 1 litro. Usando una muestra aleatoria de 100 botellas, 
# el ingeniero a cargo de la planta requiere determinar si la máquina está llenando los bidones con una media de 
# 10 litros.


# GRUPO 5:
# WLADIMIR DURÁN
# RODRIGO HERNÁNDEZ
# MANUEL vILLAR

#------------------------------------------PREGUNTA 1:-----------------------------------------

#   1. Si el ingeniero está seguro de que el verdadero volumen medio no puede ser inferior a 10 litros y piensa 
# rechazar la hipótesis nula cuando la muestra presente una media mayor a 10,5 litros, ¿cuál es la probabilidad 
# de que cometa un error de tipo I?

desvEst <- 1
n <- 100
mediaNula <- 10
z1 <- 10.5  #zona crítica superior

#Hipótesis:
#H0: La máquina llena los bidones con una media de 10 [litro]
#H0: mu = 10 litros
#Ha: La máquina llena los bidones con una media de más de 10 [litro]
#Ha: mu > 10 litros

SE  <- desvEst / sqrt(n)

#x <- seq( 5 * SE, 15 * SE, 0.01)
x <- seq( 9, 11, 1)
y <- dnorm(x, mean = mediaNula, sd = SE)

g1 <- ggplot(data = data.frame(x,y), aes(x))

#Se agrega la distribución normal
g1 <- g1 + stat_function(fun = dnorm,
                         args = list(mean = mediaNula,sd = SE), 
                         colour = "red")

g1 <- g1 + scale_x_continuous(name = "Media de llenado [litro]", breaks = seq(8,15,1))

g1 <- g1 + theme_pubr()

g1 <- g1 + geom_vline(xintercept = mediaNula, colour = "red", linetype = "longdash")

g1 <- g1 + ggtitle("Distribución normal de las medias")

print(g1)

#Ahora calculamos la probabilidad de cometer un error de tipo I, enfocándonos en la zona de rechazo que sería
# mu > 10.5

#Graficamos con la distribución anterior.

g2 <- g1 + geom_area(data = subset(data.frame(x,y), x > z1),
                     aes(y = y),
                     colour = "blue",
                     fill = "blue",
                     alpha = 0.5)

g2 <- g2 + ggtitle("Zona de rechazo para la hipótesis")

print(g2)

#Ahora calculamos la probabilidad de que se cometa un error de tipo I calculando alfa:
prob <- pnorm(z1, mean = mediaNula, sd = SE, lower.tail = FALSE)
cat("Pregunta 1: La probabilidad de que cometa un error de tipo I es: ", prob,sep = "\n")



#------------------------------------------PREGUNTA 2:-----------------------------------------

# 2. Si el verdadero volumen medio de los bidones fuera de 10,3 litros, ¿cuál sería la probabilidad de que el 
# ingeniero, que obviamente no conoce este dato, cometa un error de tipo II?

mediaVerdadera <- 10.3
x1 <- seq(mediaVerdadera - 5 * SE, mediaVerdadera + 5 * SE, 1)
y1 <- dnorm(x1, mean = mediaVerdadera, sd = SE)

g3 <- g2 + stat_function(fun = dnorm,
                         args = list(mean = mediaVerdadera,sd = SE),
                         colour = "blue")

g3 <- g3 + geom_vline(xintercept = mediaVerdadera,
                      colour = "blue",
                      linetype = "longdash")


g3 <- g3 + ggtitle("Zona superior")

print(g3)

#Para calcular la probabilidad de que se cometa un error de tipo II necesitamos sacar la probabilidad de B:
probBeta <- pnorm(z1, mean = mediaVerdadera, sd = SE, lower.tail = TRUE)
cat("Pregunta 2: Probabilidad de que el ingeniero cometa un error de tipo II es: ", probBeta,sep = "\n")


 
#------------------------------------------PREGUNTA 3:-----------------------------------------
 
# 3. Como no se conoce el verdadero volumen medio, genere un gráfico del poder estadístico con las condiciones 
# anteriores, pero suponiendo que el verdadero volumen medio podría variar de 10 a 10,7 litros.

mediaNula <- 10
desvEst <- 1
n <- 100
errorStd <- desv_est/sqrt(n)

#Superior en inferior son los extremos 10, 10.3

qInferior <- 10
qSuperior <- 10.7


funcionPoder <- function(mediaNula, errorEstandar, inferior, superior) {
  poderTeorico1 <- pnorm(inferior, mean = mediaNula, sd = errorEstandar, lower.tail = TRUE)
  
  poderTeorico2 <- pnorm(superior, mean = mediaNula, sd = errorEstandar, lower.tail = FALSE)
  
  poderTeoricoTotal <- poderTeorico1 + poderTeorico2
  
  return(poderTeoricoTotal)
}

x3 <- seq(10, 10.7, 0.01)

y3 <- sapply(x3, funcionPoder, errorEstandar = errorStd, inferior = qInferior, superior = qSuperior)


#Se imprime el gr�fico generado con el poder estad�stico.

g4 <- ggplot(data.frame(x3, y3), aes(x3, y3))

g4 <- g4 + geom_line(colour = "green")

g4 <- g4 + ylab("Poder")

g4 <- g4 + xlab("Litros")

g4 <- g4 + theme_pubr()

g4 <- g4 + ggtitle("Pregunta 3 - Poder estad�stico te�rico")

cat("Pregunta 3: Gr�fico de poder estad�stico te�rico generado.\n")
print(g4)



#------------------------------------------PREGUNTA 4:-----------------------------------------

# 4. Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse para conseguir un poder 
# estadístico de 0,8 y un nivel de significación de 0,05?
 
alfaPreg4 <- 0.05
poderEst <- 0.8
volMedioVerdadero <- 10.3
volHipNula <- 10

# Se calcula el tamaño del efecto, D de Cohen
tamanoEfecto <- ( volMedioVerdadero - volHipNula )/ desvEst

# Se encuentra la "n" mediante la prueba Z
poderZ <- pwr.norm.test(d = tamanoEfecto, sig.level = 0.05, power = 0.8, alternative = "greater")
#print(poderZ)
nZ <- ceiling(poderZ[["n"]])
cat("Pregunta 4: La cantidad de bidones que se deberían revisar para conseguir un poder de 0,8 y nivel de significación de 0,05 es: ", nZ,sep = "\n")



#------------------------------------------PREGUNTA 5:-----------------------------------------

# 5. ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de tipo I a un 1% 
# solamente?

# Se encuentra la "n" mediante la prueba Z, ahora con un nivel de significación del 0,01
poderZ01 <- pwr.norm.test(d = tamanoEfecto, sig.level = 0.01, power = 0.8, alternative = "greater")
#print(poderZ01)
nZ01 <- ceiling(poderZ01[["n"]])
cat("Pregunta 5: La cantidad de bidones que se deberían revisar para conseguir un poder de 0,8, pero ahora con un nivel de significación de 0,01 es: ", nZ01,sep = "\n")

