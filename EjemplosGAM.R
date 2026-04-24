library(ggplot2)
library(mgcv)

set.seed(2025)

n <- 90
x <- sort(runif(n, 0, 10))

# Relación verdadera suave
y_true <- 1.2 * sin(1.1 * x) + 0.25 * x

# Más ruido para que el polinomio sobreajuste
y <- y_true + rnorm(n, sd = 0.85)

datos <- data.frame(x = x, y = y)

# Modelos
mod_lineal <- lm(y ~ x, data = datos)

# Polinomio alto: intencionadamente inestable
mod_poly <- lm(y ~ poly(x, 15), data = datos)

# GAM con penalización algo más fuerte
mod_gam <- gam(y ~ s(x, k = 10), data = datos, gamma = 1.4)

# Predicciones
grid <- data.frame(x = seq(min(x), max(x), length.out = 500))

grid$lineal <- predict(mod_lineal, newdata = grid)
grid$poly   <- predict(mod_poly, newdata = grid)
grid$gam    <- predict(mod_gam, newdata = grid)

preds <- data.frame(
  x = rep(grid$x, 3),
  y = c(grid$gam, grid$poly, grid$lineal),
  modelo = rep(
    c("GAM con spline", "Polinomio grado 15", "Regresión lineal"),
    each = nrow(grid)
  )
)

g1 <- ggplot(datos, aes(x, y)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(
    data = preds,
    aes(x = x, y = y, linetype = modelo),
    linewidth = 1.1
  ) +
  labs(
    title = "Tres formas de modelar una relación no lineal",
    x = "x",
    y = "y",
    linetype = "Modelo"
  ) +
  theme_minimal(base_size = 14)

g1
 # La regresión lineal es rígida, el polinomio alto puede oscilar, el GAM suaviza la tendencia

#######################################################################################
# Gráfico 2: qué es un spline, con nudos

knots <- c(2.5, 5, 7.5)

g2 <- ggplot(datos, aes(x, y)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = y_true), linewidth = 1.1) +
  geom_vline(xintercept = knots, linetype = "dashed") +
  annotate("text", x = knots, y = min(datos$y), 
           label = "nudo", angle = 90, vjust = -0.5) +
  labs(
    title = "Idea intuitiva de un spline",
    subtitle = "Curva suave construida por tramos que se unen en ciertos puntos llamados nudos",
    x = "x",
    y = "y"
  ) +
  theme_minimal(base_size = 14)

g2
########################################################################################
# Gráfico 3: poco, mucho y buen suavizado

mod_poco_suave <- gam(y ~ s(x, k = 30), data = datos, gamma = 0.5)
mod_buen_suave <- gam(y ~ s(x, k = 10), data = datos)
mod_muy_suave  <- gam(y ~ s(x, k = 4), data = datos, gamma = 2)

grid$poco_suave <- predict(mod_poco_suave, newdata = grid)
grid$buen_suave <- predict(mod_buen_suave, newdata = grid)
grid$muy_suave  <- predict(mod_muy_suave, newdata = grid)

preds_suavizado <- data.frame(
  x = rep(grid$x, 3),
  y = c(grid$muy_suave, grid$buen_suave, grid$poco_suave),
  suavizado = rep(c("Demasiado suave: subajuste",
                    "Suavizado razonable",
                    "Poco suave: posible sobreajuste"),
                  each = nrow(grid))
)

g3 <- ggplot(datos, aes(x, y)) +
  geom_point(alpha = 0.5) +
  geom_line(data = preds_suavizado, 
            aes(x, y, linetype = suavizado), linewidth = 1) +
  labs(
    title = "Distintos suavizados en un GAM",
    x = "x",
    y = "y",
    linetype = "Nivel de suavizado"
  ) +
  theme_minimal(base_size = 14)

# ¡OJO! El objetivo no es seguir todos los puntos, sino capturar la tendencia

g3
##########################################################################
# Gráfico 4: interpretación de efectos suavizados

plot(mod_gam, shade = TRUE, seWithMean = TRUE,
     main = "Efecto suavizado estimado: s(x)",
     xlab = "x",
     ylab = "Contribución de s(x)")
# Aquí estamos visualizando el efecto de cada suavizado
# No pintamos y vs ajustado
# Con esto es como estudiamos cada GAM de forma separada
# El gráfico 4 muestra solo la parte s(x): es decir, cuánto aporta x a la predicción, no la predicción final de y.
# Si el gráfico de s(x):
# sube --> al aumentar x, la predicción aumenta
# baja --> al aumentar x, la predicción disminuye
# es casi plano --> x apenas afecta
# es casi una recta --> el efecto de x es aproximadamente lineal
# tiene curva --> el efecto de x es no lineal.
# 
# La banda sombreada es la incertidumbre del efecto estimado.
# Este gráfico nos enseña la forma del efecto de x, quitando el nivel medio del modelo.


# En un GAM no decidimos “la función es cuadrática” o “la función es logarítmica”. 
#Decidimos qué variables pueden tener efectos suaves. 
# El modelo aprende la forma de esos efectos y penaliza la complejidad para evitar sobreajuste.