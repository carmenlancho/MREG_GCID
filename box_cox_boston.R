# =========================================================
# EJEMPLO: Box-Cox con datos reales en R
# Dataset: Boston (MASS)
# =========================================================

# Paquetes
library(MASS)      # dataset Boston + función boxcox()
library(car)       # ncvTest(), qqPlot(), durbinWatsonTest()
library(lmtest)    # bptest()
library(ggplot2)

# ---------------------------------------------------------
# 1. Cargar datos
# ---------------------------------------------------------
data("Boston")
datos <- Boston

# Ver estructura
str(datos)
summary(datos)

# ---------------------------------------------------------
# 2. Modelo lineal inicial
# Variable dependiente: medv
# ---------------------------------------------------------
modelo_original <- lm(medv ~ lstat + rm + ptratio + crim, data = datos)

#   
# medv: valor mediano de la vivienda
# lstat: porcentaje de población de bajo nivel socioeconómico
# rm: número medio de habitaciones
# ptratio: ratio alumno/profesor
# crim: tasa de criminalidad

summary(modelo_original)

# ---------------------------------------------------------
# 3. Verificación de hipótesis / supuestos ANTES de Box-Cox
# ---------------------------------------------------------

# 3.1 Linealidad y patrones en residuos
par(mfrow = c(2, 2))
plot(modelo_original)
par(mfrow = c(1, 1))

# Gráfico residuos vs ajustados
plot(fitted(modelo_original), resid(modelo_original),
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Ajustados (modelo original)")
abline(h = 0, col = "red", lwd = 2)

# 3.2 Normalidad de residuos
hist(resid(modelo_original),
     main = "Histograma de residuos (modelo original)",
     xlab = "Residuos",
     col = "lightgray", border = "white")

qqnorm(resid(modelo_original),
       main = "QQ-plot residuos (modelo original)")
qqline(resid(modelo_original), col = "red", lwd = 2)

shapiro.test(resid(modelo_original))

# 3.3 Homocedasticidad
bptest(modelo_original)      # Breusch-Pagan
ncvTest(modelo_original)     # Non-constant variance test (car)

# 3.4 Independencia de errores
durbinWatsonTest(modelo_original)

# 3.5 Multicolinealidad
vif(modelo_original)

# ---------------------------------------------------------
# 4. Transformación Box-Cox
# ---------------------------------------------------------
# Box-Cox requiere respuesta positiva; medv > 0, así que vale.

bc <- boxcox(modelo_original, lambda = seq(-2, 2, by = 0.1))

# Lambda óptimo
lambda_opt <- bc$x[which.max(bc$y)]
lambda_opt

# ---------------------------------------------------------
# 5. Crear variable transformada
# ---------------------------------------------------------
# Fórmula Box-Cox:
# y(lambda) = (y^lambda - 1)/lambda, si lambda != 0
# y(lambda) = log(y), si lambda = 0

if (abs(lambda_opt) < 0.1) {
  datos$medv_bc <- log(datos$medv)
} else {
  datos$medv_bc <- (datos$medv^lambda_opt - 1) / lambda_opt
}

# ---------------------------------------------------------
# 6. Ajustar modelo con respuesta transformada
# ---------------------------------------------------------
modelo_boxcox <- lm(medv_bc ~ lstat + rm + ptratio + crim, data = datos)

summary(modelo_boxcox)

# ---------------------------------------------------------
# 7. Verificación de hipótesis / supuestos DESPUÉS de Box-Cox
# ---------------------------------------------------------

par(mfrow = c(2, 2))
plot(modelo_boxcox)
par(mfrow = c(1, 1))

# Gráfico residuos vs ajustados
plot(fitted(modelo_boxcox), resid(modelo_boxcox),
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Ajustados (modelo Box-Cox)")
abline(h = 0, col = "blue", lwd = 2)

# Normalidad
hist(resid(modelo_boxcox),
     main = "Histograma de residuos (modelo Box-Cox)",
     xlab = "Residuos",
     col = "lightblue", border = "white")

qqnorm(resid(modelo_boxcox),
       main = "QQ-plot residuos (modelo Box-Cox)")
qqline(resid(modelo_boxcox), col = "blue", lwd = 2)

shapiro.test(resid(modelo_boxcox))

# Homocedasticidad
bptest(modelo_boxcox)
ncvTest(modelo_boxcox)

# Independencia
durbinWatsonTest(modelo_boxcox)

# Multicolinealidad
vif(modelo_boxcox)

# ---------------------------------------------------------
# 8. Comparación simple de ajuste
# ---------------------------------------------------------
cat("\nAIC modelo original:", AIC(modelo_original), "\n")
cat("AIC modelo Box-Cox :", AIC(modelo_boxcox), "\n")

cat("\nR2 ajustado modelo original:", summary(modelo_original)$adj.r.squared, "\n")
cat("R2 ajustado modelo Box-Cox :", summary(modelo_boxcox)$adj.r.squared, "\n")

# ---------------------------------------------------------
# 9. Comparación visual de residuos
# ---------------------------------------------------------
resumen_residuos <- data.frame(
  residuos = c(resid(modelo_original), resid(modelo_boxcox)),
  modelo = rep(c("Original", "Box-Cox"),
               times = c(length(resid(modelo_original)),
                         length(resid(modelo_boxcox))))
)

ggplot(resumen_residuos, aes(x = residuos, fill = modelo)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ modelo, scales = "free") +
  theme_minimal() +
  labs(title = "Comparación de residuos",
       x = "Residuos", y = "Frecuencia")

# ---------------------------------------------------------
# 10. Predicción en escala original (opcional)
# ---------------------------------------------------------
# Si quieres volver a la escala original, hay que invertir la transformación.

pred_bc <- predict(modelo_boxcox)

if (abs(lambda_opt) < 0.1) {
  pred_original_scale <- exp(pred_bc)
} else {
  pred_original_scale <- (lambda_opt * pred_bc + 1)^(1 / lambda_opt)
}

head(pred_original_scale)



#### Versión compacta
library(MASS)
library(lmtest)
library(car)

data("Boston")

m1 <- lm(medv ~ lstat + rm + ptratio + crim, data = Boston)
summary(m1)

plot(m1)
shapiro.test(resid(m1))
bptest(m1)
ncvTest(m1)

bc <- boxcox(m1, lambda = seq(-2, 2, 0.1))
lambda <- bc$x[which.max(bc$y)]
lambda

if (abs(lambda) < 0.1) {
  Boston$medv_bc <- log(Boston$medv)
} else {
  Boston$medv_bc <- (Boston$medv^lambda - 1) / lambda
}

m2 <- lm(medv_bc ~ lstat + rm + ptratio + crim, data = Boston)
summary(m2)

plot(m2)
shapiro.test(resid(m2))
bptest(m2)
ncvTest(m2)


# Se aplicó una transformación Box-Cox con el objetivo de mejorar el 
# cumplimiento de los supuestos del modelo lineal. Sin embargo, aunque 
# se observó cierta mejora en la distribución de los residuos y/o en la 
# estabilidad de la varianza, los problemas no desaparecieron completamente. 
# Esto sugiere que la transformación, por sí sola, no resulta suficiente y que
# podrían requerirse otras modificaciones en la especificación del modelo.
