# ------------------------------- Script ----------------------------------
#                     Universidad Nacional de Colombia
#                     Facultad de Ciencias Económicas
#                              Econometría I
# ------------------------------ Monitoria --------------------------------
#                       Julián David Rojas Aguilar

# ---------------------------- Instrucciones ------------------------------
# 1. Cada línea en el script se ejecuta con Ctrl + Enter o con el botón run de 
#    esta ventana.
# 2. Todo el texto que se encuentre después del numeral (#) es un comentario.
# 3. A la hora de ejecutar cada línea, R no tendrá en cuenta los comentarios.
# 4. Todos los resultados de comandos ejecutados se muestran en la consola.
# 5. Es posible ejecutar comandos directamente desde la consola con la tecla 
#    Enter.

# 1| Preparación ----------------------------------------------------------
rm(list = ls())
library(readr)     # Lectura de los .csv.
library(readxl)    # Lectura de los .xlsx.
library(here)      # Direcciones relativas.
library(tidyverse) # Manipulación de datos y generación de gráficas.
library(reshape2)  # Manipulación de datos: Formato de lectura en ggplot2.
library(lubridate) # Manipulación de datos: Fechas.
library(seasonal)  # Componente estacional.
library(forecast)  # Empleado como complemento de 'seasonal'.
library(uroot)     # Pruebas de raíz unitaria.
library(seastests) # Preba OCSB.

# 2| Bases de datos -------------------------------------------------------
# 2.1| Importación --------------------------------------------------------
# - Encuesta Mensual de Comercio al por Menor y Vehículos - EMCM.
data <- read_excel(here('Input', 'EMCM (Febrero 2020).xls'), 
                   range = 'B7:T211',
                   sheet = 3)

# 2.2| Manipulación -------------------------------------------------------
# 2.2.1| Creación de fechas -----------------------------------------------
inicialDate     = ymd('2003-02-01')
numberOfMonths  = nrow(data)
dates           = as.data.frame(inicialDate + months(0:(numberOfMonths-1)) - days(1))
colnames(dates) = 'Fecha'

# 2.2.2| Consolidación ----------------------------------------------------
data            = cbind(dates, data)
data            = data %>% select(-c('Año', 'Mes'))

dataGgplot = data %>% melt(id = c('Fecha')) # ¿Cómo se comportan las variables?
colnames(dataGgplot) = c('Fecha', 'Variable', 'Valor')

# 2.2.3| Visualización ----------------------------------------------------
# A| Todas las series originales ------------------------------------------
dataGgplot %>% ggplot(aes(x = Fecha, y = Valor)) +
  geom_line() +
  facet_wrap(. ~ Variable, scales = 'free') +
  labs(title    = 'Encuesta Mensual de Comercio al por Menor y Vehículos', 
       subtitle = 'Análisis desglosado por índice',
       caption  = 'Elaboración propia. \n Información extraida de: Departamento Administrativo Nacional de Estadística',
       x        = 'Fecha',
       y        = 'Índice') + 
  scale_x_date(date_labels = '%b %Y') +
  theme_bw() +
  theme(plot.title            = element_text(size = 12, face = 'bold'),
        plot.subtitle         = element_text(face = 'italic'),
        axis.text.x           = element_text(angle = 45, hjust = 1),
        legend.position       = 'bottom',
        legend.box.background = element_rect(),
        legend.box.margin     = margin(6, 6, 6, 6))

# B| Comportamientos estacionales -----------------------------------------
data[, 2] %>% ts(start = c(2003, 1), frequency = 12) %>% ggseasonplot(main = 'Gráfica estacional: Niveles por años \nDiscriminando por meses',
                                                                      xlab = 'Meses',
                                                                      year.labels = T,
                                                                      year.labels.left = T)

data[, 2] %>% ts(start = c(2003, 1), frequency = 12) %>% ggseasonplot(main = 'Gráfica estacional: Diseño polar \nDiscriminando por años',
                                                                      xlab = 'Meses',
                                                                      polar = T)

data[, 2] %>% ts(start = c(2003, 1), frequency = 12) %>% ggsubseriesplot(main = 'Gráfica estacional: Distribución de los valores \nDiscriminado por meses',
                                                                         xlab = 'Meses')

# B1| X11 -----------------------------------------------------------------
data[, 2] %>% ts(start = c(2003, 1), frequency = 12) %>% seas(x11='') -> SA

autoplot(SA) + ggtitle(paste0('Descomposición vía x11 de ', colnames(data)[2])) +
  theme_bw()

data[, 2] %>% ts(start = c(2003, 1), frequency = 12) %>% 
  autoplot(series = 'Original') +
  autolayer(trendcycle(SA), series = 'Ciclo-Tendencia') +
  autolayer(seasadj(SA), series = 'Ajustada estacionalmente') +
  scale_colour_manual(values = c('gray', 'blue', 'red'),
                      breaks = c('Original', 'Ajustada estacionalmente', 'Ciclo-Tendencia')) +
  labs(title    = 'Encuesta Mensual de Comercio al por Menor y Vehículos', 
       subtitle = paste0('Resultados para ', colnames(data)[2]),
       caption  = 'Elaboración propia. \n Información extraida de: Departamento Administrativo Nacional de Estadística',
       x        = 'Fecha',
       y        = 'Índice') + 
  theme_bw() +
  theme(plot.title            = element_text(size = 12, face = 'bold'),
        plot.subtitle         = element_text(face = 'italic'),
        axis.text.x           = element_text(angle = 45, hjust = 1),
        legend.position       = 'bottom',
        legend.box.background = element_rect(),
        legend.box.margin     = margin(6, 6, 6, 6))

# B2| X13-ARIMA-SEATS -----------------------------------------------------
serieSA <- seas(data[, 2] %>% ts(start = c(2003, 1), frequency = 12), 
                regression.aictest = c('easter'),
                transform.function = 'none')
names(serieSA)
serieSA$data
attributes(serieSA$data)

original(serieSA)
final(serieSA)
resid(serieSA) 
coef(serieSA)
fivebestmdl(serieSA)
out(serieSA, browser = getOption('Firefox'))
spc(serieSA)

plot(serieSA,
     main = 'Serie original y ajustada',
     sub  = 'Elaboración propia',
     xlab = 'Tiempo',
     ylab = colnames(data)[2])
summary(serieSA)

# Se exporta la serie desestacionalizada para aplicar el filtro mecánico:
serieSA = final(serieSA)
write.csv(serieSA, file = paste0('Output/Data/', 'Serie ajustada estacionalmente.csv'), 
          row.names = FALSE)
serieSA = serieSA %>% ts(start = c(2003, 1), frequency = 12)

# 3| Modelación -----------------------------------------------------------
# 3.1| Evaluación del comportamiento --------------------------------------
data[, 2] %>% ts(start = c(2003, 1), frequency = 12) %>% 
  ggtsdisplay(main = 'Serie original', xlab = 'Fecha', ylab = 'Índice', 
              plot.type = 'partial', theme = theme_bw())

data[, 2] %>% ts(start = c(2003, 1), frequency = 12) %>% diff() %>% 
  ggtsdisplay(main = 'Diferencia regular', xlab = 'Fecha', 
              ylab = 'Índice diferenciado regularmente',
              plot.type = 'partial', theme = theme_bw())

data[, 2] %>% ts(start = c(2003, 1), frequency = 12) %>% diff(lag = 12) %>% 
  ggtsdisplay(main = 'Diferencia estacional', xlab = 'Fecha', 
              ylab = 'Índice diferenciado estacionalmente',
              plot.type = 'partial', theme = theme_bw())

data[, 2] %>% ts(start = c(2003, 1), frequency = 12) %>% 
  diff() %>% diff(lag = 12) %>% 
  ggtsdisplay(main='Diferencia regular y estacional', xlab = 'Fecha', 
              ylab = 'Índice diferenciado regular \n y estacionalmente', 
              plot.type = 'partial', theme = theme_bw())

# 3.2| Pruebas de raíz unitaria estacionales ------------------------------
serie = data[, 2] %>% ts(start = c(2003, 1), frequency = 12)

# 3.2.1| HEGY -------------------------------------------------------------
# Modelo con constante, tendencia y dummies estacionales:
HEGY <- hegy.test(serie, deterministic = c(1,1,1), lag.method = 'AIC', maxlag = 4)
summary(HEGY)
hegy.boot.pval(serie, HEGY$fit, HEGY$stat, deterministic = c(1,1,1), 
               lag.method = 'AIC', nb = 1000)
# Constante y tendencia:
HEGY <- hegy.test(serie, deterministic = c(1,1,0), lag.method = 'AIC', maxlag = 4)
summary(HEGY)
hegy.boot.pval(serie, HEGY$fit, HEGY$stat, deterministic = c(1,1,0), maxlag = 4,
               lag.method = 'AIC', nb = 1000)
# Constante:
HEGY <- hegy.test(serie, deterministic = c(1,0,0), lag.method = 'AIC', maxlag = 4)
summary(HEGY)
hegy.boot.pval(serie, HEGY$fit, HEGY$stat, deterministic = c(1,0,0), maxlag = 4,
               lag.method = 'AIC', nb = 1000)
# Nada:
HEGY <- hegy.test(serie, deterministic = c(0,0,0), lag.method = 'AIC', maxlag = 4)
summary(HEGY)
hegy.boot.pval(serie, HEGY$fit, HEGY$stat, deterministic = c(0,0,0), maxlag = 4,
               lag.method = 'AIC', nb = 1000)

# 3.2.2| OCSB -------------------------------------------------------------
OCSB <- ocsb(serie, method = 'ML', nrun = 1000, seed = 123)
OCSB

# 3.2.3| CH ---------------------------------------------------------------
CH <- ch.test(serie, type = 'trigonometric', pvalue = 'RS',
              lag1 = TRUE, NW.order = 4)
summary(CH$fit)
CH

# 3.2.4| Resumen ----------------------------------------------------------
nsdiffs(serie, alpha = 0.05, test = 'seas') 
nsdiffs(serie, alpha = 0.05, test = 'ocsb') 
nsdiffs(serie, alpha = 0.05, test = 'hegy') 
nsdiffs(serie, alpha = 0.05, test = 'ch')

# 3.3| Red de búsqueda ----------------------------------------------------
lambdaEstimation <- BoxCox.lambda(serie, lower = 0)
model <- serie %>% 
  Arima(order    = c(0, 1, 0), 
        seasonal = c(0, 1, 0), 
        lambda   = lambdaEstimation)
summary(model)

results = data.frame(model = 0,
                     aic   = 0,
                     bic   = 0)
aux = 0
for(p in 0:4) {
  for(q in 0:4) {
    for(P in 0:2) {
      for(Q in 0:2) {
        aux = aux + 1
        model <- serie %>% 
          Arima(order    = c(p, 1, q), 
                seasonal = c(P, 1, Q), 
                lambda   = lambdaEstimation)
        results[aux, 1] = paste0('(', p, ',1,', q, ') x (', P, ',1,', Q, ')s')
        results[aux, 2] = model[['aic']]
        results[aux, 3] = model[['bic']]
      }
    }
  }
}

# Extracción del resultado:
results %>% subset(aic == min(results$aic))
results %>% subset(bic == min(results$bic))

# 3.4| Estimación y validación de supuestos -------------------------------
model <- serie %>% 
  Arima(order    = c(2, 1, 3), 
        seasonal = c(0, 1, 1), 
        lambda   = lambdaEstimation)
summary(model)

# Chequeo:
checkresiduals(model)
autoplot(model)

# 3.5| Pronóstico ---------------------------------------------------------
autoplot(forecast(model))
model %>% forecast(h = 12) %>% autoplot()
