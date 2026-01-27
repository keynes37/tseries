# Econometría II
library(tidyverse)   
library(data.table)  # opcional, también sirve para tablas rápidas
library(ggplot2)     # en R se usa para gráficos
library(CVXR) # paquete en R para optimización convexa


# Leer archivo Excel
library(readxl)
mp <- read_excel("monthly_prices.xlsx")
head(mp)

# Creamos una columna de Mes (1 a 24)
mp$Mes <- 1:24  

# Pasamos de formato ancho (wide) a largo (long) para graficar fácilmente
mp_long <- mp %>%
  pivot_longer(cols = c(CRWD, NU, NFLX), names_to = "Stock", values_to = "Precio")

# Graficamos
ggplot(mp_long, aes(x = Mes, y = Precio, color = Stock, shape = Stock)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  labs(x = "Meses", y = "Stock price (Monthly average)") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_blank()
  )

### Vamos a ver los retornos

# Suponemos que mp tiene una columna de fechas como rownames o una columna "Date"
mr <- mp %>%
  mutate(across(everything(), ~ (.-lag(.)) / lag(.)))
head(mr)

symbols <- colnames(mr)
return_data <- t(as.matrix(mr))

library(tidyr)
library(dplyr)
library(ggplot2)

# Quitamos la primera fila (NA en retornos)
mr <- mr[-1, ]
mr$Mes <- 1:nrow(mr)

mr_long <- mr %>%
  select(Mes, CRWD, NFLX, NU) %>%
  pivot_longer(cols = -Mes, names_to = "Stock", values_to = "Retorno")

ggplot(mr_long, aes(x = Mes, y = 100 * Retorno, color = Stock, shape = Stock)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  labs(x = "Meses", y = "Monthly return (%)") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_blank()
  )

# Extraer solo los retornos
retornos <- mr %>% select(-Mes)
# Convertir a matriz numérica
retornos_d <- t(as.matrix(retornos))

# Promedios de retorno
r <- rowMeans(retornos_d)
# Matriz de covarianzas
C <- cov(t(retornos_d))
C

### Vamos a cálcular el riesgo de acciones
symbols <- colnames(mr %>% select(-Mes))
for (j in seq_along(symbols)) {
  cat(sprintf("%s: Exp ret = %f, Risk = %f\n",
              symbols[j], r[j], sqrt(C[j, j])))
}

#------------ (Parte avanzada)----------#
library(CVXR)

# Número de activos que tenemos
n <- length(symbols)
# Vector de pesos (variable de decisión)
x <- Variable(n)
# Retorno mínimo requerido (esto lo debe ajustar usted mismo/a)
req_return <- -0.03
# Retorno esperado del portafolio
ret <- t(r) %*% x
# Riesgo del portafolio (varianza)
risk <- quad_form(x, C)

# Definir problema de optimización
prob <- Problem(
  Minimize(risk),
  constraints = list(sum(x) == 1, ret >= req_return, x >= 0)
)

### Parte final

result <- try(solve(prob), silent = TRUE)

if (!inherits(result, "try-error")) {
  cat("Portafolio óptimo\n")
  cat("----------------------\n")
  
  # Pesos óptimos
  weights <- result$getValue(x)
  
  for (s in seq_along(symbols)) {
    cat(sprintf(" Inversión en %s : %.2f%% del portafolio\n",
                symbols[s], 100 * weights[s]))
  }
  
  cat("----------------------\n")
  cat(sprintf("Exp ret = %.2f%%\n", 100 * result$getValue(ret)))
  cat(sprintf("Expected risk = %.2f%%\n", 100 * sqrt(result$getValue(risk))))
  
} else {
  cat("Error en la optimización\n")
}