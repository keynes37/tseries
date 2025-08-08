# ===========================================
# Econometría II: Tendencia
# Script para clase
# ===========================================

# --- Instalación y carga de paquetes ---
# Instalar si no se tienen:
# install.packages(c("pacman", "fpp2", "dynlm", "huxtable", "readxl", "tidyverse"))

library(pacman)
p_load(fpp2, dynlm, huxtable, readxl, tidyverse)

# --- Lectura de datos ---
datos <- read_excel("cartera.xlsx")
serie <- ts(datos$Cartera, start = c(2016, 1), frequency = 12)

# --- Gráfico de la serie ---
autoplot(serie) +
  ylab("$ Miles de Millones") +
  xlab("Año") +
  ggtitle("Cartera Comercial de Bancos Colombianos 2016-2023")

# --- Gráfico estacional ---
ggseasonplot(serie, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("$ Miles de Millones") +
  ggtitle("Gráfico Estacional: Cartera comercial")

# --- Ajuste estacional con STL ---
sea <- stl(serie, s.window = "per")
autoplot(serie, series = "Data") +
  autolayer(seasadj(sea), series = "Ajuste Estacional")

# --- Ciclo y tendencia ---
autoplot(serie, series = "Data") +
  autolayer(trendcycle(sea), series = "Tendencia") +
  autolayer(seasadj(sea), series = "Ajuste Estacional") +
  xlab("Fecha") + ylab("$ Miles de Millones") +
  ggtitle("Cartera de Bancos comerciales") +
  scale_colour_manual(values = c("gray", "blue", "red"),
                      breaks = c("Data", "Ajuste Estacional", "Tendencia"))

# --- Modelos de tendencia ---
m1 <- dynlm(Cartera ~ trend(serie), data = serie)
m2 <- dynlm(Cartera ~ trend(serie) + I(trend(serie)^2), data = serie)
m3 <- dynlm(Cartera ~ trend(serie) + I(trend(serie)^2) + I(trend(serie)^3), data = serie)
m4 <- dynlm(Cartera ~ log(trend(serie)), data = serie)
m5 <- dynlm(Cartera ~ I(1/trend(serie)), data = serie)

huxreg(m1, m2, m3, m4, m5,
       statistics = c("Observaciones" = "nobs", "R2 Ajustado" = "adj.r.squared"))

# --- Pronósticos ---
# Tendencia lineal
m.lin <- tslm(serie ~ trend)
p.lin <- forecast(m.lin, h = 10)

# Tendencia exponencial
m.exp <- tslm(serie ~ trend, lambda = 0)
p.exp <- forecast(m.exp, h = 10)

# Tendencia cúbica
m.cub <- tslm(serie ~ trend + trend^2 + trend^3)
p.cub <- forecast(m.cub, h = 10)

autoplot(serie) +
  autolayer(fitted(m.lin), series = "Lineal") +
  autolayer(fitted(m.exp), series = "Exponencial") +
  autolayer(fitted(m.cub), series = "Cúbico") +
  autolayer(p.lin$mean, series = "Lineal") +
  autolayer(p.exp$mean, series = "Exponencial") +
  autolayer(p.cub$mean, series = "Cúbico") +
  xlab("Año") + ylab("$ Miles de Millones") +
  ggtitle("Predicción simple") +
  guides(colour = guide_legend(title = " "))

# --- Otros métodos de pronóstico ---
autoplot(serie) +
  autolayer(meanf(serie, h = 11), PI = FALSE, series = "Promedio") +
  autolayer(naive(serie, h = 11), PI = FALSE, series = "Naïve") +
  autolayer(snaive(serie, h = 11), PI = FALSE, series = "Naïve Estacional") +
  ggtitle("Pronósticos de Cartera") +
  xlab("Fecha") + ylab("$ Miles de Millones") +
  guides(colour = guide_legend(title = "Pronóstico por"))

# --- Box-Cox ---
# Lambda específico
autoplot(BoxCox(serie, lambda = 1/3))

# Lambda óptimo
lambda_opt <- BoxCox.lambda(serie)
print(lambda_opt)

# Pronóstico con Box-Cox
niv1 <- snaive(serie, lambda = 1/3)
autoplot(niv1)

niv2 <- snaive(serie, lambda = lambda_opt)
autoplot(niv2)