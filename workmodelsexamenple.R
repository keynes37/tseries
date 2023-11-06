# Paquetes
library(pacman)
p_load(readxl, TSstudio, tidyverse, stats, urca, forecast, ggfortify, ggplot2, tseries, fpp2, fontawesome)

# Serie importada
pib <- read_excel("datospib2015.xlsx")
pib <- pib |> select(PIB_Cons)
pibn <- ts(pib, frequency=4, start=c(2005,1))


# Grafico 1
pibn %>% log() %>% diff(lag=4) %>% diff() %>%
  ggtsdisplay()

cbind("PIB ($Miles de millones 2015)" = pibn,
      "PIB trimestral en logaritmo" = log(pibn),
      "Cambio anual logaritmico" = diff(log(pibn),4)) %>%
  autoplot(facets=TRUE) +
  xlab("Trimestres") + ylab("") +
  ggtitle("PIB precios constantes 2015")


bpinl<-pibn %>% log() %>% diff(lag=4) %>% diff()

# Estacionariedad
testkp<-ur.kpss(bpinl, type = "tau", lags = "short")
summary(testkp)

# Numero de diferencias (por si acaso)

pibn %>% log() %>% nsdiffs()
pibn %>% log() %>% diff(lag=4) %>% nsdiffs()

sarima_dpibc<-Arima(bpinl, order=c(1,0,1), seasonal=c(1,0,0))

ggplot2::autoplot(sarima_dpibc)
??autoplot
# Pronostico
autoplot(forecast(sarima_dpibc, h=4))

# Objeto
pron<-forecast(sarima_dpibc, h=4)

# Datos solicitados
pron

plot.armaroots(sarima_dpibc)

# Compute raices del AR 
arroots <- function(object)
{
  if(!("Arima" %in% class(object)) &
     !("ar" %in% class(object)))
    stop("object must be of class Arima or ar")
  if("Arima" %in% class(object))
    parvec <- object$model$phi
  else
    parvec <- object$ar
  if(length(parvec) > 0)
  {
    last.nonzero <- max(which(abs(parvec) > 1e-08))
    if (last.nonzero > 0)
      return(structure(list(
        roots=polyroot(c(1,-parvec[1:last.nonzero])),
        type="AR"),
        class='armaroots'))
  }
  return(structure(list(roots=numeric(0), type="AR"),
                   class='armaroots'))
}

# Computar raices MA
maroots <- function(object)
{
  if(!("Arima" %in% class(object)))
    stop("object must be of class Arima")
  parvec <- object$model$theta
  if(length(parvec) > 0)
  {
    last.nonzero <- max(which(abs(parvec) > 1e-08))
    if (last.nonzero > 0)
      return(structure(list(
        roots=polyroot(c(1,parvec[1:last.nonzero])),
        type="MA"),
        class='armaroots'))
  }
  return(structure(list(roots=numeric(0), type="MA"),
                   class='armaroots'))
}

plot.armaroots <- function(x, xlab="Real", ylab="Imaginaria",
                           main=paste("Raiz inversa", x$type,
                                      "Polinomio Característico"),
                           ...)
{
  oldpar <- par(pty='s')
  on.exit(par(oldpar))
  plot(c(-1,1), c(-1,1), xlab=xlab, ylab=ylab,
       type="n", bty="n", xaxt="n", yaxt="n", main=main, ...)
  axis(1, at=c(-1,0,1), line=0.5, tck=-0.025)
  axis(2, at=c(-1,0,1), label=c("-i","0","i"),
       line=0.5, tck=-0.025)
  circx <- seq(-1,1,l=501)
  circy <- sqrt(1-circx^2)
  lines(c(circx,circx), c(circy,-circy), col='gray')
  lines(c(-2,2), c(0,0), col='gray')
  lines(c(0,0), c(-2,2), col='gray')
  if(length(x$roots) > 0)
  {
    inside <- abs(x$roots) > 1
    points(1/x$roots[inside], pch=19, col='black')
    if(sum(!inside) > 0)
      points(1/x$roots[!inside], pch=19, col='red')
  }
}

par(mfrow=c(1,2))
plot(arroots(sarima_dpibc),main="Raices inversas del AR")
plot(maroots(sarima_dpibc),main="Raices inversas del MA")

library(feasts)
feasts::gg_arma(sarima_dpibc)


# Crear dos series aleatorias con diferentes medias
set.seed(123)  # Establecer una semilla para la reproducibilidad
serie1 <- 200+rnorm(100, mean = 50, sd = 10)  # Serie 1 con media 50 y desviación estándar 10
serie2 <- 1000+rnorm(100, mean = 200, sd = 10)  # Serie 2 con media 200 y desviación estándar 20

# Crear un dataframe con las dos series
df <- data.frame(Serie1 = serie1, Serie2 = serie2)

# Realizar una regresión lineal de Serie2 en función de Serie1
regresion <- lm(Serie2 ~ Serie1, data = df)
summary(regresion)
# Graficar las dos series y la línea de regresión en un solo gráfico
library(ggplot2)

ggplot(df, aes(x = 1:100)) +
  geom_line(aes(y = Serie1, color = "Serie 1"), size = 1) +
  geom_line(aes(y = Serie2, color = "Serie 2"), size = 1) +
  geom_abline(intercept = coef(regresion)[1], slope = coef(regresion)[2], color = "red", linetype = "dashed") +
  labs(x = "Tiempo", y = "Valores", color = "Series") +
  scale_color_manual(values = c("Serie 1" = "blue", "Serie 2" = "green")) +
  theme_minimal()

# Cargar la librería stats
library(stats)
set.seed(1234)
# Establecer el número de observaciones
n <- 1000
# Crear una serie de tiempo x
x <- ts(data = rep(NA, n), start = 1, frequency = 12)
# Establecer el primer valor
x[1] <- 100
# Generar valores siguiendo un proceso AR(1)
for (t in 2:n) {
  x[t] <- x[t - 1] + qnorm(runif(1))
}

# Lo mismo para y
y <- ts(data = rep(NA, n), start = 1, frequency = 12)
# Establecer el primer valor
y[1] <- 0.5
# Generar valores siguiendo un proceso AR(1)
for (t in 2:n) {
  y[t] <- y[t - 1] + qnorm(runif(1))
}

par(mfrow=c(1,2))
plot(x,main="Serie X")
plot(y,main="Serie Y")

mi_ols<-lm(y~x)
summary(mi_ols)

library(tseries)
library(TSstudio)
library(tidyverse)
library(fpp2)
mi_ols$residuals %>% ggtsdisplay()


library(urca)
ser=serial.test(mi_ols, lags.pt=16, type= "PT.asymptotic")

mi_ols2<-lm(diff(y)~diff(x))
summary(mi_ols2)



library(dymlm)
