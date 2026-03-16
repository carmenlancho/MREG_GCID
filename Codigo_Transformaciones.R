# Creamos funciones con los escalados
min_max <- function(x){
  x_transf <- (x-min(x))/(max(x)-min(x))
  return(x_transf)
}

robust_scale <- function(x){
  x_trans <- (x-median(x))/(quantile(x,0.75) - quantile(x,0.25))
  return(x_trans)
}


## Efecto de la estandarización
x1 <- rnorm(600)
hist(x1)
x1_estand <- scale(x1)
hist(x1_estand)
hist(min_max(x1))
hist(robust_scale(x1))


x2 <- rnorm(600, mean = 85, sd= 50)

hist(x2)
x2_estand <- scale(x2)
hist(x2_estand)
hist(min_max(x2))
hist(robust_scale(x2))


x3 <- runif(600, 10,50)
hist(x3)
x3_est <-scale(x3) 
hist(x3_est)
hist(min_max(x3))
hist(robust_scale(x3))


x4 <- rexp(600,rate=5)
hist(x4)

x4_est <- scale(x4)
hist(x4_est)
hist(min_max(x4))
hist(robust_scale(x4))




######################

# boxcox(object,    # lm 
#        lambda = seq(-2, 2, 1/10), # Vector of values of lambda
#        plotit = TRUE,  # Create a plot or not
#        eps = 1/50,     # Tolerance for lambda. Defaults to 0.02.
#        xlab = expression(lambda), # X-axis title
#        ylab = "log-Likelihood")  # Y-axis title


x <- c(0.103, 0.528, 0.221, 0.260, 0.091,
       1.314, 1.732, 0.244, 1.981, 0.273,
       0.461, 0.366, 1.407, 0.079, 2.266)

# Histogram of the data
hist(x)


library(MASS)

bc <- boxcox(lm(x ~ 1))
bc$x # los valores de lambda
bc$y # log verosimilitud

lambda <- bc$x[which.max(bc$y)] ## --> recomienda logaritmo

modelo <- lm(log(x) ~ 1)
summary(modelo)
hist(log(x))

modelo2 <- lm(x~ 1)
summary(modelo2)

## Codificación categórica
# Crear datos de ejemplo
datos <- data.frame(color = c("rojo", "verde", "azul", "rojo"))

# Crear variables dummy (one-hot)
# El '-1' elimina la columna de intercepto si no se desea
matriz_onehot <- model.matrix(~ color-1, data = datos)
print(matriz_onehot)


library(mltools)
library(data.table)

datos <- data.frame(color = c("rojo", "verde", "azul", "rojo"))
# Convertir a factor primero
datos$color <- as.factor(datos$color)

# Aplicar one_hot
encoded_data <- one_hot(as.data.table(datos))
print(encoded_data)


library(caret)

datos <- data.frame(color = c("rojo", "verde", "azul", "rojo"))
dummies <- dummyVars(~ ., data = datos)
predict(dummies, newdata = datos)


## Caso ordinal
# Crear datos de ejemplo
datos <- data.frame(
  ID = 1:5,
  Satisfaccion = c("Muy Insatisfecho", "Insatisfecho", "Neutral", "Satisfecho", "Muy Satisfecho")
)

# Convertir en factor ordenado
datos$Satisfaccion_factor <- factor(datos$Satisfaccion, 
                                    levels = c("Muy Insatisfecho", "Insatisfecho", "Neutral", "Satisfecho", "Muy Satisfecho"), 
                                    ordered = TRUE)

# Codificación ordinal manual
datos$Satisfaccion_codificada <- as.numeric(datos$Satisfaccion_factor)

# Verificar la codificación
datos


# Simular variable respuesta correlacionada con el orden
set.seed(123)
datos$Puntuacion <- c(2, 4, 6, 8, 10) + rnorm(5, mean = 0, sd = 0.5)

# Modelo de regresión
modelo <- lm(Puntuacion ~ Satisfaccion_factor, data = datos)
summary(modelo)


modelo <- lm(Puntuacion ~ Satisfaccion_codificada, data = datos)
summary(modelo)



