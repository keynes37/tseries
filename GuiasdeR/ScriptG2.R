# ===========================================
# Econometría II: Medias Móviles y Holt-Winters
# Script optimizado para clase
# ===========================================

# Instalar los paquetes necesarios (si no están instalados)
# install.packages(c("pacman", "fpp2", "forecast", "zoo", "TSstudio", "readxl", "tidyverse"))

# Cargar los paquetes necesarios
library(pacman)
p_load(fpp2, forecast, zoo, TSstudio, readxl, tidyverse)

# ===========================================
# Cargar y preparar los datos
# ===========================================

# Cargar los datos de la cartera comercial de bancos
datos <- read_excel("cartera.xlsx")
serie <- ts(datos$Cartera, start = c(2016, 1), frequency = 12)

# Gráfico de la serie original
autoplot(serie) +
  ylab("$ Miles de Millones") + 
  xlab("Año") +
  ggtitle("Cartera Comercial de Bancos Colombianos 2016-2023")

# ===========================================
# Modelos de Medias Móviles
# ===========================================

## Media Móvil Simple (SMA)
# Establecemos n = 2 (2-periodos) para la media móvil simple
n <- 2
sma <- stats::filter(serie, rep(1 / n, n), sides = 1)

# Media móvil ponderada (WMA) con ponderación [0.25, 0.5, 0.25]
wma <- stats::filter(serie, c(0.25, 0.5, 0.25), sides = 1)

# Media móvil exponencial (EMA) usando suavizado exponencial
ema <- ses(serie, alpha = 0.2)

# Gráfico de los pronósticos de medias móviles
autoplot(serie) +
  autolayer(sma, series = "SMA") +
  autolayer(wma, series = "WMA") +
  autolayer(ema$mean, series = "EMA") +
  xlab("Año") + 
  ylab("$ Miles de Millones") +
  ggtitle("Pronósticos con Modelos de Medias Móviles") +
  guides(colour = guide_legend(title = "Modelos"))

# ===========================================
# Holt-Winters (Triple Exponential Smoothing)
# ===========================================

## Ajuste del modelo Holt-Winters (estacional multiplicativo)
hw_model <- hw(serie, seasonal = "multiplicative", h = 12)

# Gráfico de los pronósticos de Holt-Winters
autoplot(serie) +
  autolayer(hw_model$mean, series = "Holt-Winters") +
  xlab("Año") + 
  ylab("$ Miles de Millones") +
  ggtitle("Pronóstico con Holt-Winters Estacional Multiplicativo") +
  guides(colour = guide_legend(title = "Modelos"))

## Holt-Winters con tendencia amortiguada (Damped)
hw_damped <- holt(serie, damped = TRUE, phi = 0.9, h = 12)

# Gráfico de los pronósticos Holt-Winters Damped
autoplot(serie) +
  autolayer(hw_damped$mean, series = "Holt-Winters Damped") +
  xlab("Año") + 
  ylab("$ Miles de Millones") +
  ggtitle("Pronóstico con Holt-Winters Damped") +
  guides(colour = guide_legend(title = "Modelos"))

# ===========================================
# Comparación de Modelos de Pronóstico
# ===========================================

## Comparación de los modelos de medias móviles y Holt-Winters
autoplot(serie) +
  autolayer(sma, series = "SMA") +
  autolayer(wma, series = "WMA") +
  autolayer(ema$mean, series = "EMA") +
  autolayer(hw_model$mean, series = "Holt-Winters Multiplicativo") +
  autolayer(hw_damped$mean, series = "Holt-Winters Damped") +
  xlab("Año") + 
  ylab("$ Miles de Millones") +
  ggtitle("Comparación de Modelos de Pronóstico") +
  guides(colour = guide_legend(title = "Modelos"))

# ===========================================
# Resultados de precisión del modelo
# ===========================================

## Precisión de Holt-Winters Multiplicativo
print(round(accuracy(hw_model), 2))

## Precisión de Holt-Winters Damped
print(round(accuracy(hw_damped), 2))

## Precisión de la Media Móvil Exponencial
print(round(accuracy(ema), 2))