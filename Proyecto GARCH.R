rm(list=ls()) # Eliminar variables
cat("\014") # Vaciar consola
graphics.off() # Eliminar gráficas

#### LIBRERÍAS ####
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(data.table,fGarch,rugarch,forecast,TSA,MTS,fDMA,lubridate,ggplot2)

#### HECHOS ESTILIZADOS ####
setwd('') # Elegir directorio

SerieHist<-read.csv("NVDA.csv", header=TRUE) ## Leamos la base de datos
SerieHist$time<-as.Date(SerieHist$time,format("%d/%m/%Y")) #Convertimos la columna Fecha en tipo Fecha

# Gráfico de la Evolución del Precio de Cierre
plot(SerieHist, type="l", col="black", 
     xlab="Años", ylab="Precio en Dólares",
     main="Precio de cierre de NVIDIA 2020 a 2025" ,xaxt="n")

axis(1,at = seq(SerieHist[1,1], SerieHist[1356,1], by = 365),label=2020:2025)
abline(v=SerieHist[506,], col="blue",lty=3) # 2022
abline(v=SerieHist[757,], col="red",lty=3) # 2023
abline(v=SerieHist[1007,], col="blue",lty=3) # 2024
abline(v=SerieHist[1259,], col="red",lty=3) # 2025

# Gráfico Logaritmo Precio de Cierre

FechaHist<-SerieHist[,1] #Tomamos la columna de las fechas
head(FechaHist) #Revisamos los primeros valores de esta columna
class(FechaHist) # Revisamos sus características

FechaHist<-as.Date(FechaHist,format("%d/%m/%Y")) # Reafirmamos su formato de fecha
class(FechaHist) # Comprobamos su formato
length(FechaHist) # Medimos cuantos datos tiene
LogHist<-log(SerieHist[,2]) # Creamos el vector con el logaritmos de los tipos de Cambio
class(LogHist) # Revisamos la clase de los valores de este vector
length(LogHist) # Medimos los datos que tiene, los mismos que el vector Fecha
LogSerieHist<-data.frame(FechaHist,LogHist) # Creamos un date frame con esos 2 vectores
head(LogSerieHist) # Revisamos los valores
names(LogSerieHist)
class(LogSerieHist)

plot(LogSerieHist, type="l", col="black", 
     xlab="Años", ylab="Ln Precio", 
     main="Logaritmo Precio de Cierre de NVIDIA 2020-2025")

# Creación de Matriz de Diferenciales
# Vector Logaritmo
RendHist<-diff(LogSerieHist$LogHist) #Creamos los Rendimientos, es un vector num?rico
pacf(as.vector(RendHist))
head(RendHist)
class(RendHist)
length(RendHist)
RendHist_Absoluto<-abs(RendHist)
head(RendHist_Absoluto)
which(RendHist>0)
RendHist_positivo<-(RendHist+RendHist_Absoluto)/2
head(RendHist_positivo)
RendHist_negativo<-abs((RendHist-RendHist_Absoluto)/2)
head(RendHist_negativo)

# Vector Fechas
FechaRendHist<-SerieHist[2:length(SerieHist$time),1]
head(FechaRendHist)
FechaRendHist<-as.Date(FechaRendHist,format("%d/%m/%Y")) #convertimos la columna en formato fecha
class(FechaRendHist)
length(FechaRendHist)

# Creación Log-Rendimientos
SerieRendHist<-data.frame(FechaRendHist,RendHist)
names(SerieRendHist)<-c("Fecha","Rendimientos") #Renombramos los nombres de las columnas
head(SerieRendHist)#Revisamos los primeros valores
tail(SerieRendHist)
class(SerieRendHist) #Revisamos la clase de este objeto
View(SerieRendHist)
plot(SerieRendHist, type="l", col="black", 
     xlab="Años", ylab="Rendimientos ", 
     main="Rendimientos Precio de Cierre de NVIDIA 2020-2025",xaxt="n")

axis(1,at = seq(SerieRendHist[1,1], SerieRendHist[1355,1], by = 365),label=2020:2025)
abline(v=SerieRendHist[30,], col="blue",lty=3)
abline(v=SerieRendHist[70,], col="blue",lty=3)
abline(v=SerieRendHist[835,], col="red",lty=3)
abline(v=SerieRendHist[875,], col="red",lty=3)
abline(v=SerieRendHist[1253,], col="blue",lty=3)
abline(v=SerieRendHist[1293,], col="blue",lty=3)

# ACF Log-Rendimientos
par(mfrow = c(1,2))
acf(SerieRendHist$Rendimientos, main="ACF Rendimientos Precio de Cierre",xlab="Retardo")
pacf(SerieRendHist$Rendimientos, main="PACF Rendimientos Precio de Cierre", xlab="Retardo")

# Histograma Log-Rendimientos
par(mfrow = c(2,1))

boxplot(SerieRendHist$Rendimientos, horizontal = TRUE, boxwex=1,xlab = "Rendimientos NVIDIA 2020-2025",main="Diagrama de cajas y bigotes")##, ylim = c(2, 4.5))
abline(v=mean(SerieRendHist$Rendimientos), col="blue")
hist(SerieRendHist$Rendimientos, breaks =100,xlab = "Rendimientos ",ylab = "Frecuencia",main="Histograma de Rendimientos P. de C. NVIDIA 2020-2025")
abline(v=mean(SerieRendHist$Rendimientos), col="blue")

skewness(SerieRendHist$Rendimientos) # Sesgo
kurtosis(SerieRendHist$Rendimientos)   # Kurtosis

# Gráfica para Observar las Colas Pesadas de la Serie
par(mfrow = c(1,1))
plot(density(SerieRendHist$Rendimientos),col="blue",xlab="Rendimientos", main="Densidad estimada, método Kernel")
curve(dnorm(x,mean=mean(SerieRendHist$Rendimientos),sd= sd(SerieRendHist$Rendimientos)), add = TRUE, col='red')
legend(x = "topright", legend = c("Densidad Rendimientos P. de C.", "Densidad Normal"), col = c("blue", "red"),lty = 1:1,cex=0.6)

#### AJUSTE DE MODELO ####

# Efecto ARCH en rendimientos
archtest(as.vector(SerieRendHist$Rendimientos), lag=10)

# ACF, PACF y EACF del cuadrado de los rendimientos
par(mfrow=c(1,2))
DifCuadradas<-SerieRendHist$Rendimientos^2
acf(DifCuadradas, main="ACF Cuadrados de Rendimientos Precio de Cierre", xlab="Retardo")
pacf(DifCuadradas, main="PACF Cuadrados de Rendimientos Precio de Cierre", xlab="Retardo")
eacf(DifCuadradas) # ARMA(1,2) y ARMA(2,2)

# 1. GARCH(2,2) con errores normales ----
garch22 <- ugarchspec(variance.model=list(model = "sGARCH",  garchOrder = c(2,2), 
                                          submodel = NULL,  external.regressors = NULL,    variance.targeting = FALSE), 
                      mean.model=list(armaOrder = c(0, 0),  include.mean = F, external.regressors = NULL),  distribution.model = "norm")

garchfit22 <- ugarchfit(spec = garch22, out.sample=23, data = SerieRendHist)
print(garchfit22)

# Validación de supuestos ----
stdret22 <- residuals(garchfit22, standardize = TRUE)

par(mfrow = c(1, 2))
acf(as.vector(stdret22^2), main = "ACF")
pacf(as.vector(stdret22^2), main = "PACF")

ks.test(stdret22, "pnorm", 0, 1)

# Boxplot
boxplot(stdret22, 
        main = "Boxplot de Residuos de GARCH(2,2)",
        ylab = "Valor del Residuo",
        col = "lightblue",
        border = "darkblue",
        notch = TRUE)
abline(h = 0, col = "red", lty = 2)

# Histograma con densidad y curva normal
hist(stdret22, 
     freq = FALSE,
     main = "Histograma de Residuos de GARCH(2,2)",
     xlab = "Residuos",
     col = "lightgreen",
     border = "darkgreen",
     breaks = 30)

lines(density(stdret22, na.rm = TRUE), col = "blue", lwd = 2) # Añadir curva de densidad estimada

curve(dnorm(x, mean = mean(stdret22, na.rm = TRUE), sd = sd(stdret22, na.rm = TRUE)), 
      col = "red", lwd = 2, lty = 2, add = TRUE) # Añadir curva normal teórica

legend("topright", 
       legend = c("Densidad", "Normal Teórica"),
       col = c("blue", "red"),
       lwd = 2, lty = c(1, 2)) # Leyenda

# 2. GARCH(1,2) con errores normales ----
garch12 <- ugarchspec(variance.model=list(model = "sGARCH",  garchOrder = c(2,1), 
                                          submodel = NULL,  external.regressors = NULL,    variance.targeting = FALSE), 
                      mean.model=list(armaOrder = c(0, 0),  include.mean = F, external.regressors = NULL),  distribution.model = "norm")

garchfit12 <- ugarchfit(spec = garch12, out.sample=23, data = SerieRendHist)
print(garchfit12)

# Validación de supuestos ----
stdret12 <- residuals(garchfit12, standardize = TRUE)

par(mfrow = c(1, 2))
acf(as.vector(stdret12^2), main = "ACF")
pacf(as.vector(stdret12^2), main = "PACF")

ks.test(stdret12, "pnorm", 0, 1)

# Boxplot
boxplot(stdret12, 
        main = "Boxplot de Residuos de GARCH(1,2)",
        ylab = "Valor del Residuo",
        col = "lightblue",
        border = "darkblue",
        notch = TRUE)
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1,1))
# Histograma con densidad y curva normal
hist(stdret12, 
     freq = FALSE,
     main = "Histograma de Residuos de GARCH(1,2)",
     xlab = "Residuos",
     col = "lightgreen",
     border = "darkgreen",
     breaks = 30)

lines(density(stdret12, na.rm = TRUE), col = "blue", lwd = 2) # Añadir curva de densidad estimada

curve(dnorm(x, mean = mean(stdret12, na.rm = TRUE), sd = sd(stdret12, na.rm = TRUE)), 
      col = "red", lwd = 2, lty = 2, add = TRUE) # Añadir curva normal teórica

legend("topright", 
       legend = c("Densidad", "Normal Teórica"),
       col = c("blue", "red"),
       lwd = 2, lty = c(1, 2)) # Leyenda

# 3. GARCH(1,1) con errores normales ----
garch11 <- ugarchspec(variance.model=list(model = "sGARCH",  garchOrder = c(1,1), 
                                          submodel = NULL,  external.regressors = NULL,    variance.targeting = FALSE), 
                      mean.model=list(armaOrder = c(0, 0),  include.mean = F, external.regressors = NULL),  distribution.model = "norm")

garchfit11 <- ugarchfit(spec = garch11, out.sample=23, data = SerieRendHist)
print(garchfit11)

# Validación de supuestos ----
stdret11 <- residuals(garchfit11, standardize = TRUE)

par(mfrow = c(1, 2))
acf(as.vector(stdret11^2), main = "ACF")
pacf(as.vector(stdret11^2), main = "PACF")

ks.test(stdret11, "pnorm", 0, 1)

# Boxplot
par(mfrow = c(1, 1))
boxplot(stdret11, 
        main = "Boxplot de Residuos de GARCH(1,1)",
        ylab = "Valor del Residuo",
        col = "lightblue",
        border = "darkblue",
        notch = TRUE)
abline(h = 0, col = "red", lty = 2)

# Histograma con densidad y curva normal
hist(stdret11, 
     freq = FALSE,
     main = "Histograma de Residuos de GARCH(1,1)",
     xlab = "Residuos",
     col = "lightgreen",
     border = "darkgreen",
     breaks = 30)

lines(density(stdret11, na.rm = TRUE), col = "blue", lwd = 2) # Añadir curva de densidad estimada

curve(dnorm(x, mean = mean(stdret11, na.rm = TRUE), sd = sd(stdret11, na.rm = TRUE)), 
      col = "red", lwd = 2, lty = 2, add = TRUE) # Añadir curva normal teórica

legend("topright", 
       legend = c("Densidad", "Normal Teórica"),
       col = c("blue", "red"),
       lwd = 2, lty = c(1, 2)) # Leyenda

#### PRONÓSTICOS ####
garch11 <- ugarchspec(variance.model=list(model = "sGARCH",  garchOrder = c(1,1), 
                                          submodel = NULL,  external.regressors = NULL,    variance.targeting = FALSE), 
                      mean.model=list(armaOrder = c(0, 0),  include.mean = F, external.regressors = NULL),  distribution.model = "norm")

garchfit11 <- ugarchfit(spec = garch11 , out.sample=23 , data = SerieRendHist)

model.forecast <- ugarchforecast(garchfit11 , n.ahead= 23 , n.roll =23, out.sample=23)

model.forecast

#### GRAFICAS
# Obtener rendimientos y fechas
rend <- SerieRendHist$Rendimientos
fechas <- SerieRendHist$Fecha

# Define modelo GARCH(1,1) sin media
garch11 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)

# Ajuste de modelo usando todas las observaciones excepto las últimas 23
garchfit11 <- ugarchfit(spec = garch11, out.sample = 23, data = rend)

# Pronóstico de 5 periodos
model.forecast <- ugarchforecast(garchfit11, n.ahead = 5, n.roll = 0, out.sample = 5)

# Extraer estimaciones y pronósticos
fitted_vals <- as.numeric(fitted(garchfit11))            # Estimacion rendimientos
forecast_vals <- as.numeric(fitted(model.forecast))      # Pronósticos rendimientos
sigma_vals <- as.numeric(sigma(garchfit11))              # Estimación volatilidad
forecast_sigma <- as.numeric(sigma(model.forecast))      # Pronósticos volatilidad

# Comprobar longitud de los índices
n_total <- length(rend)
n_out <- 23
n_fit <- n_total - n_out
n_forecast <- length(forecast_vals)

# Definir índices usando fechas
fechas_fit <- fechas[1:n_fit]
fechas_forecast <- fechas[(n_total - n_out + 1):(n_total - n_out + n_forecast)]

# Serie de rendimientos
plot(fechas, rend, type = "l", col = "black", lwd = 1.5,
     main = "Rendimientos observados, estimados y pronosticados",
     xlab = "Fecha", ylab = "Rendimientos", xaxt = "n")
lines(fechas_fit, fitted_vals, col = "blue", lwd = 2)
lines(fechas_forecast, forecast_vals, col = "red", lwd = 3)
points(fechas_forecast, forecast_vals, col = "red", pch = 16)
abline(v = fechas[n_fit], col = "gray", lty = 2)

axis.Date(1, at = seq(min(fechas), max(fechas), by = "year"), format = "%Y")

legend("topleft",
       legend = c("Observado", "Estimado", "Pronóstico (5 pasos)"),
       col = c("black", "blue", "red"), lty = 1, lwd = 2)

# Serie de volatilidad
plot(fechas_fit, sigma_vals, type = "l", col = "blue", lwd = 2,
     ylim = range(c(sigma_vals, forecast_sigma)),
     main = "Volatilidad Estocástica Estimada y Pronosticada",
     xlab = "Fecha", ylab = "Volatilidad", xaxt = "n")
lines(fechas_forecast, forecast_sigma, col = "red", lwd = 2)
abline(v = fechas[n_fit], col = "gray", lty = 2)

axis.Date(1, at = seq(min(fechas), max(fechas), by = "year"), format = "%Y")

legend("topleft",
       legend = c("Volatilidad Estimada", "Pronóstico de Volatilidad"),
       col = c("blue", "red"), lty = 1, lwd = 2)

# Pronósticos
cat("\nPronóstico de Rendimientos (5 pasos):\n")
print(round(forecast_vals, 6))

cat("\nPronóstico de Volatilidad (5 pasos):\n")
print(round(forecast_sigma, 6))