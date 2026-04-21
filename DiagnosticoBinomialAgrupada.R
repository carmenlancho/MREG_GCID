# regresión binomial agrupada
# No tenemos una observación 0/1 por fila, tenemos
# en cada grupo hubo m_i ensayos y, de esos, y_i éxitos
# Modelamos Y_i ~ Binomial(m_i,p_i)
# glm(cbind(exitos, fracasos) ~ x1 + x2, family = binomial)

set.seed(456)

# -----------------------------
# Simular datos agrupados
# -----------------------------
g <- 120

tratamiento <- factor(sample(c("Control", "Tratado"), g, replace = TRUE))
dosis <- runif(g, 0, 10)
ensayos <- sample(20:60, g, replace = TRUE)

eta <- -2 + 0.35*dosis + 0.8*(tratamiento == "Tratado")
p <- 1 / (1 + exp(-eta))

exitos <- rbinom(g, size = ensayos, prob = p)
fracasos <- ensayos - exitos

datos_binom <- data.frame(exitos, fracasos, ensayos, tratamiento, dosis)

# -----------------------------
# Ajustar modelo binomial agrupado
# -----------------------------
mod_binom <- glm(cbind(exitos, fracasos) ~ dosis + tratamiento,
                 family = binomial(link = "logit"),
                 data = datos_binom)

summary(mod_binom)

# Hay que verificar lo mismo quee en reglog
# pero es más importantee el ajuste de proporciones observadas
# frente a esperadas

## Forma funcional
mod_binom_quad <- glm(cbind(exitos, fracasos) ~ dosis + I(dosis^2) + tratamiento,
                      family = binomial,
                      data = datos_binom)

anova(mod_binom, mod_binom_quad, test = "Chisq")

## Residuos deviance y Pearson
res_dev_b <- residuals(mod_binom, type = "deviance")
res_pear_b <- residuals(mod_binom, type = "pearson")
fit_b <- fitted(mod_binom)

par(mfrow = c(1, 2))
plot(fit_b, res_dev_b,
     xlab = "Ajustados", ylab = "Residuos deviance",
     main = "Binomial agrupada: deviance residuals")
abline(h = 0, lty = 2, col = 2)

plot(fit_b, res_pear_b,
     xlab = "Ajustados", ylab = "Residuos Pearson",
     main = "Binomial agrupada: Pearson residuals")
abline(h = 0, lty = 2, col = 2)
par(mfrow = c(1, 1))

## Sobredispersión
# El modelo supone Var(Y_i|X)=m_i p_i (1-p_i)
# peero a veces la variabilidad real es mayor
# y entonces los errores estándar salen demasiado optimistas
dispersion <- sum(residuals(mod_binom, type = "pearson")^2) / mod_binom$df.residual
dispersion
# Si está cerca de 1 es razonable
# Si >1 --> sospechamos sobredispersión y usamos quasibinomial
mod_quasi <- glm(cbind(exitos, fracasos) ~ dosis + tratamiento,
                 family = quasibinomial(link = "logit"),
                 data = datos_binom)

summary(mod_quasi)
# coefs paarecidos, cambian errores estándar

## Observado vs predicho
datos_binom$prop_obs <- datos_binom$exitos / datos_binom$ensayos
datos_binom$prop_pred <- fitted(mod_binom)

plot(datos_binom$prop_pred, datos_binom$prop_obs,
     xlab = "Proporción predicha",
     ylab = "Proporción observada",
     main = "Observado vs predicho")
abline(0, 1, col = 2, lty = 2)

# añadimos tamaño proporcional al número de ensayos
plot(datos_binom$prop_pred, datos_binom$prop_obs,
     cex = sqrt(datos_binom$ensayos) / 4,
     xlab = "Proporción predicha",
     ylab = "Proporción observada",
     main = "Observado vs predicho (tamaño ~ ensayos)")
abline(0, 1, col = 2, lty = 2)

## Influencia
hat_b <- hatvalues(mod_binom)
cook_b <- cooks.distance(mod_binom)

par(mfrow = c(1, 2))
plot(hat_b, type = "h", main = "Leverage", ylab = "hat values")
abline(h = 2*mean(hat_b), lty = 2, col = 2)

plot(cook_b, type = "h", main = "Cook's distance", ylab = "Cook")
abline(h = 4/nrow(datos_binom), lty = 2, col = 2)
par(mfrow = c(1, 1))

## Comparación con modelo nulo
mod_binom_nulo <- glm(cbind(exitos, fracasos) ~ 1,
                      family = binomial,
                      data = datos_binom)

anova(mod_binom_nulo, mod_binom, test = "Chisq")


