set.seed(123)

# -----------------------------
# 1. Simular datos
# -----------------------------
n <- 400

edad <- rnorm(n, mean = 40, sd = 12)
ingresos <- rnorm(n, mean = 2500, sd = 600)
sexo <- factor(sample(c("Mujer", "Hombre"), n, replace = TRUE))

# Predictor lineal verdadero
eta <- -6 + 0.08*edad + 0.0012*ingresos + 0.7*(sexo == "Hombre")

# Probabilidad verdadera
p <- 1 / (1 + exp(-eta))

# Respuesta binaria
y <- rbinom(n, size = 1, prob = p)

datos_logit <- data.frame(y, edad, ingresos, sexo)

# -----------------------------
# 2. Ajustar modelo logístico
# -----------------------------
mod_logit <- glm(y ~ edad + ingresos + sexo,
                 family = binomial(link = "logit"),
                 data = datos_logit)

summary(mod_logit)

# Hay que verificar que la especificación funcional sea razonable.
# En logística no se exige que Y sea lineal en X, sino que el logit sea lineal.
# Entonces hay que revisar si edad e ingresos entran bien de forma lineal o 
# si convendrían transformaciones, polinomios o splines.
# Comparar con términos no lineales
mod_logit_quad <- glm(y ~ edad + I(edad^2) + ingresos + I(ingresos^2) + sexo,
                      family = binomial,
                      data = datos_logit)

anova(mod_logit, mod_logit_quad, test = "Chisq")
# Si los términos cuadrados mejoran mucho el ajuste, sospechamos no linealidad.

## Gráfico de residuos parciales aproximado
# No existe el mismo “partial residual plot” clásico que en lineal, 
# pero una forma útil es comparar con suavizados.

library(ggplot2)

datos_logit$pred <- predict(mod_logit, type = "response")
datos_logit$eta_hat <- predict(mod_logit, type = "link")
datos_logit$resp_dev <- residuals(mod_logit, type = "deviance")

ggplot(datos_logit, aes(x = edad, y = resp_dev)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  labs(title = "Residuos deviance vs edad")

ggplot(datos_logit, aes(x = ingresos, y = resp_dev)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  labs(title = "Residuos deviance vs ingresos")
# No buscamoss “nube normal”, sino patrones fuertes.
# Si el suavizado muestra curvatura clara, 
# el predictor puede estar mal especificado.


## Observaciones influyentes y leverage
hat_vals <- hatvalues(mod_logit)
cook_vals <- cooks.distance(mod_logit)
dfb <- dfbetas(mod_logit)

summary(hat_vals)
summary(cook_vals)

par(mfrow = c(1, 2))
plot(hat_vals, type = "h", main = "Leverage", ylab = "hat values")
abline(h = 2*mean(hat_vals), lty = 2, col = 2)

plot(cook_vals, type = "h", main = "Cook's distance", ylab = "Cook")
abline(h = 4/nrow(datos_logit), lty = 2, col = 2)
par(mfrow = c(1, 1))

# Mostramos las problemáticas
which(hat_vals > 2*mean(hat_vals))
which(cook_vals > 4/nrow(datos_logit))

## Residuos
# En GLM residuos deviance y Pearson
res_dev <- residuals(mod_logit, type = "deviance")
res_pear <- residuals(mod_logit, type = "pearson")
ajustados <- fitted(mod_logit)

par(mfrow = c(1, 2))
plot(ajustados, res_dev,
     xlab = "Valores ajustados", ylab = "Residuos deviance",
     main = "Deviance residuals vs fitted")
abline(h = 0, lty = 2, col = 2)

plot(ajustados, res_pear,
     xlab = "Valores ajustados", ylab = "Residuos Pearson",
     main = "Pearson residuals vs fitted")
abline(h = 0, lty = 2, col = 2)
par(mfrow = c(1, 1))
# No comprobamos homocedasticidad como en lineal.
# Buscamos son patrones raros, grupos sistemáticos o puntos extremos.
# Por ejemplo, ese punto muy abajo a la derecha en Pearson residuals.
# Eso significa: p_i alta pero y_i=0
# El modelo estaba muy confiado en que sería 1, y falló.
# Ese punto puede ser: una observación rara, una posible influyente
# un indicio de mala especificación, o simplemente un caso poco probable pero posible
#  revisar leverage y Cook.


## Separación 
# En logística puede pasar que una covariable separe casi perfectamente los 0 y 1.
# Síntomas: coeficientes enormes, errores estándar enormes, warnings de convergencia
summary(mod_logit)

## Bondad de ajuste global
# Deviance y comparación con modelo nulo
mod_nulo <- glm(y ~ 1, family = binomial, data = datos_logit)
anova(mod_nulo, mod_logit, test = "Chisq")

# pseudo r2
library(pscl)
pR2(mod_logit)

##  Capacidad predictiva y calibración
prob_hat <- predict(mod_logit, type = "response")
pred_clase <- ifelse(prob_hat > 0.5, 1, 0)

table(Predicho = pred_clase, Observado = datos_logit$y)
mean(pred_clase == datos_logit$y)

library(pROC)

roc_obj <- roc(datos_logit$y, prob_hat)
plot(roc_obj, main = "Curva ROC")
auc(roc_obj)

## Calibración simple por grupos
datos_logit$grupo <- cut(prob_hat,
                         breaks = quantile(prob_hat, probs = seq(0, 1, 0.1)),
                         include.lowest = TRUE)

calib <- aggregate(cbind(obs = y, pred = prob_hat) ~ grupo,
                   data = datos_logit,
                   FUN = mean)

print(calib)

plot(calib$pred, calib$obs,
     xlab = "Probabilidad predicha media",
     ylab = "Proporción observada",
     main = "Calibración")
abline(0, 1, col = 2, lty = 2)
# Si los puntos quedan cerca de la diagonal, la calibración va bien.
library(car)
residualPlots(mod_logit)


## Resumen logística:
# Ajuste
mod <- glm(y ~ x1 + x2, family = binomial, data = datos)

# Residuos
plot(fitted(mod), residuals(mod, type="deviance"))
plot(fitted(mod), residuals(mod, type="pearson"))

# Influencia
plot(hatvalues(mod), type="h")
plot(cooks.distance(mod), type="h")

# No linealidad
mod2 <- glm(y ~ x1 + I(x1^2) + x2, family = binomial, data = datos)
anova(mod, mod2, test = "Chisq")



