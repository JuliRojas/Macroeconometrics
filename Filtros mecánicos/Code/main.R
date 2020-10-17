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
# 1.1| Funciones ----------------------------------------------------------
rm(list = ls())
library(readxl)       # Lectura de los .xlsx.
library(here)         # Direcciones relativas.
library(tidyverse)    # Manejo de datos y gráficas.
library(RColorBrewer) # Colores. 
library(mFilter)      # Filtro de HP.
library(TSA)

# 1.2| Bases de datos -----------------------------------------------------
firstYear          = 2005
firstMonth         = 1
quarterlyFrequency = 4

colombiaOriginalPIB <- read_excel(here('Input', '(DANE, 2020) PIB Colombia - Constante 2015 - Desestacionalizado.xlsx'),
                          sheet = 2, range = 'C21:BK21', col_names = FALSE) %>%
  as.numeric() %>% ts(start = c(firstYear, firstMonth), frequency = quarterlyFrequency)

colombiaSAPIB <- read_excel(here('Input', '(DANE, 2020) PIB Colombia - Constante 2015 - Desestacionalizado.xlsx'),
                                  sheet = 3, range = 'C21:BK21', col_names = FALSE) %>%
  as.numeric() %>% ts(start = c(firstYear, firstMonth), frequency = quarterlyFrequency)

# 1.3| Visualización ------------------------------------------------------
par(mfrow = c(2, 1))
plot(colombiaOriginalPIB,
     main = 'PIB a precios constantes de 2015',
     sub  = 'Elaboración propia, con datos extraídos del DANE.',
     ylab = 'Miles de millones',
     xlab = 'Fecha',
     col  = '#E41A1C',
     type = 'l')
plot(colombiaSAPIB,
     main = 'PIB a precios constantes de 2015 \n Datos corregidos de efectos estacionales y de calendario',
     sub  = 'Elaboración propia, con datos extraídos del DANE.',
     ylab = 'Miles de millones',
     xlab = 'Fecha',
     col  = '#377EB8',
     type = 'l')
par(mfrow = c(1, 1))

# 2| Filtrado -------------------------------------------------------------
serie <- colombiaSAPIB

# 2.1| Tendencia lineal ---------------------------------------------------
# 2.1.1| Cálculo ----------------------------------------------------------
linearModel <- lm(serie ~ time(serie))
summary(linearModel)

linearTrend <- linearModel$fitted.values
tendency    <- ts(linearTrend, start = c(firstYear, firstMonth), frequency = quarterlyFrequency)
linearCycle <- serie - tendency

# 2.1.2| Visualización ----------------------------------------------------
par(mfrow = c(1, 2))
plot(serie,
     main = 'PIB a precios constantes de 2015 \n Datos corregidos de efectos estacionales y de calendario',
     sub  = 'Elaboración propia, con datos extraídos del DANE.',
     ylab = 'Miles de millones',
     xlab = 'Fecha',
     col  = '#E41A1C',
     type = 'l') 
lines(tendency, col = '#377EB8')
legend('topleft', legend = c('Original', 'Tendencia'), pch = 15, 
       col = c('#E41A1C', '#377EB8'))
plot(linearCycle,
     main = 'Componente Cíclico \n Aplicación del Filtro Lineal',
     sub  = 'Elaboración propia. Desviaciones de la tendencia.',
     ylab = 'Miles de millones',
     xlab = 'Fecha',
     col  = '#4DAF4A',
     type = 'l')
abline(h = 0, col = 'black', lty = 2)
legend('topright', legend = c('Ciclo'), pch = 15, col = c('#4DAF4A'))
par(mfrow = c(1, 1))

# 2.2| Hodrick-Prescott ---------------------------------------------------
# 2.2.1| Cálculo ----------------------------------------------------------
HP      <- hpfilter(serie, freq = 1600, type = 'lambda')
hpCycle <- HP[['cycle']]

# 2.2.2| Visualización ----------------------------------------------------
# Automática:
plot(HP)

# Desagregando la lista:
par(mfrow = c(1, 2))
plot(serie,
     main = 'PIB a precios constantes de 2015 \n Datos corregidos de efectos estacionales y de calendario',
     sub  = 'Elaboración propia, con datos extraídos del DANE.',
     ylab = 'Miles de millones',
     xlab = 'Fecha',
     col  = '#E41A1C',
     type = 'l') 
lines(HP[['trend']], col = '#377EB8')
legend('topleft', legend = c('Original', 'Tendencia'), pch = 15, 
       col = c('#E41A1C', '#377EB8'))
plot(HP[['cycle']],
     main = 'Componente Cíclico \n Aplicación del Filtro Hodrick-Prescott',
     sub  = 'Elaboración propia. Desviaciones de la tendencia.',
     ylab = 'Miles de millones',
     xlab = 'Fecha',
     col  = '#4DAF4A',
     type = 'l')
abline(h = 0, col = 'black', lty = 2)
legend('topright', legend = c('Ciclo'), pch = 15, col = c('#4DAF4A'))
par(mfrow = c(1, 1))

# 2.3| Baxter-King --------------------------------------------------------
# 2.3.1| Cálculo ----------------------------------------------------------
BK      <- bkfilter(serie, pl = 6, pu = 32)
bkCycle <- BK$cycle

# 2.3.2| Visualización ----------------------------------------------------
# Automática:
plot(BK)

# Desagregando la lista:
par(mfrow = c(1, 2))
plot(serie,
     main = 'PIB a precios constantes de 2015 \n Datos corregidos de efectos estacionales y de calendario',
     sub  = 'Elaboración propia, con datos extraídos del DANE.',
     ylab = 'Miles de millones',
     xlab = 'Fecha',
     col  = '#E41A1C',
     type = 'l') 
lines(BK$trend, col = '#377EB8')
legend('topleft', legend = c('Original', 'Tendencia'), pch = 15, 
       col = c('#E41A1C', '#377EB8'))
plot(BK$cycle,
     main = 'Componente Cíclico \n Aplicación del Filtro Baxter-King',
     sub  = 'Elaboración propia. Desviaciones de la tendencia.',
     ylab = 'Miles de millones',
     xlab = 'Fecha',
     col  = '#4DAF4A',
     type = 'l')
abline(h = 0, col = 'black', lty = 2)
legend('topright', legend = c('Ciclo'), pch = 15, col = c('#4DAF4A'))
par(mfrow = c(1, 1))

# 2.4| Christiano-Fitzgerald ----------------------------------------------
# 2.4.1| Cálculo ----------------------------------------------------------
CF      <- cffilter(serie, pl = 6, pu = 32, root = T)
cfCycle <- CF$cycle

# 2.4.2| Visualización ----------------------------------------------------
# Automática:
plot(CF)

# Desagregando la lista:
par(mfrow = c(1, 2))
plot(serie,
     main = 'PIB a precios constantes de 2015 \n Datos corregidos de efectos estacionales y de calendario',
     sub  = 'Elaboración propia, con datos extraídos del DANE.',
     ylab = 'Miles de millones',
     xlab = 'Fecha',
     col  = '#E41A1C',
     type = 'l') 
lines(CF$trend, col = '#377EB8')
legend('topleft', legend = c('Original', 'Tendencia'), pch = 15, 
       col = c('#E41A1C', '#377EB8'))
plot(CF$cycle,
     main = 'Componente Cíclico \n Aplicación del Filtro Christiano-Fitzgerald',
     sub  = 'Elaboración propia. Desviaciones de la tendencia.',
     ylab = 'Miles de millones',
     xlab = 'Fecha',
     col  = '#4DAF4A',
     type = 'l')
abline(h = 0, col = 'black', lty = 2)
legend('topright', legend = c('Ciclo'), pch = 15, col = c('#4DAF4A'))
par(mfrow = c(1, 1))

# 2.5| Combinación --------------------------------------------------------
# 2.5.1| Cálculo ----------------------------------------------------------
combination <- ts.union(linearCycle, hpCycle, bkCycle, cfCycle)

# 2.5.2| Visualización ----------------------------------------------------
display.brewer.all()
colors = brewer.pal(4, 'Set1')

plot.ts(combination,
     main = 'PIB a precios constantes de 2015 \n Componente cíclico mediante varios filtros mecánicos',
     sub  = 'Elaboración propia, con datos extraídos del DANE.',
     ylab = 'Miles de millones',
     xlab = 'Fecha',
     col  = colors,
     type = 'l',
     plot.type = 'single') 
legend('topleft', legend = c('Lineal', 'Hodrick-Prescott', 'Baxter-King', 'Christiano-Fitzgerald'),
       pch = 15, col = colors)

# 3| Descomposición espectral ---------------------------------------------
graphics.off()

# 3.1| Simulación ---------------------------------------------------------
# 3.1.1| Generación -------------------------------------------------------
set.seed(123)
observations <- 100
t <- seq(from = 1, to = observations, by = 1)
w <- c(6/observations, 30/observations, 40/observations) # Frecuencias.

x1 <- 2 * cos(2 * pi * t * w[1]) + 3 * sin(2 * pi * t * w[1]) # 6 ciclos.
x2 <- 4 * cos(2 * pi * t * w[2]) + 5 * sin(2 * pi * t * w[2]) # 30 ciclos.
x3 <- 6 * cos(2 * pi * t * w[3]) + 7 * sin(2 * pi * t * w[3]) # 40 ciclos.
y <- x1 + x2 + x3 + rnorm(observations, mean = 0, sd = 1)

# 3.1.2| Visualización ----------------------------------------------------
par(mfrow = c(2, 2))
plot(x1, type = 'l', main = 'Primer componente \n Baja frecuencia', xlab = 'Tiempo')
plot(x2, type = 'l', main = 'Segundo componente \n Media frecuencia', xlab = 'Tiempo')
plot(x3, type = 'l', main = 'Tercer componente \n Alta frecuencia', xlab = 'Tiempo')
plot(y, type = 'l', main = 'Serie completa', xlab = 'Tiempo')
par(mfrow = c(1, 1))

# 3.1.3| Periodograma -----------------------------------------------------
periodogram(x1, main = 'Periodograma \n Primer componente: Baja frecuencia', 
            col = 'red', xlab = 'Frecuencia', ylab = 'Periodograma')
periodogram(x2, main = 'Periodograma \n Segundo componente: Media frecuencia', 
            col = 'red', xlab = 'Frecuencia', ylab = 'Periodograma')
periodogram(x3, main = 'Periodograma \n Tercer componente: Alta frecuencia', 
            col = 'red', xlab = 'Frecuencia', ylab = 'Periodograma')
periodogram(y, main = 'Periodograma \n Serie completa',
            col = 'red', xlab = 'Frecuencia', ylab = 'Periodograma')

# 3.1.4| Aplicación de filtros --------------------------------------------
# Exclusión de las altas frecuencias.
cfY <- cffilter(y, pl = 16, pu = 20)
periodogram(cfY$cycle, main = 'Periodograma \n Serie completa - Filtrada con CF',
            sub = expression(paste('Los umbrales escogidos son: ', rho^l, '=16, ', rho^u, '=20')),
            col = 'red', xlab = 'Frecuencia', ylab = 'Periodograma')

# 3.1.5| Comparación ------------------------------------------------------
par(mfrow = c(1, 2))
plot(x1, 
     main = 'Comparación de valores \n No observados vs. Estimados',
     sub  = 'Elaboración propia.',
     ylab = 'Valores',
     xlab = 'Tiempo',
     col  = 'black',
     type = 'l', 
     lty = 1)
lines(cfY$cycle, lty = 3, lwd = 3, col = 'red')
legend('topleft', legend = c('Original', 'Filtrada'), 
       lty = c(1, 3), col = c('black', 'red'), bty = 'o')

plot(y, 
     main = 'Comparación de valores \n Serie observada vs. Componente de baja frecuencia',
     sub  = 'Elaboración propia.',
     ylab = 'Valores',
     xlab = 'Tiempo',
     col  = 'black',
     type = 'l', 
     lty = 1)
lines(cfY$cycle, lty = 3, lwd = 3, col = 'red')
legend('topleft', legend = c('Observada', 'Filtrada'), 
       lty = c(1, 3), col = c('black', 'red'))
par(mfrow = c(1, 1))

# 3.2| Empírico -----------------------------------------------------------
par(mfrow = c(2, 2))
periodogram(linearCycle, main = 'Filtro lineal', col = 'red', 
            xlab = 'Frecuencia', ylab = 'Periodograma')
periodogram(hpCycle, main = 'Filtro Hodrick-Prescott', col = 'red', 
            xlab = 'Frecuencia', ylab = 'Periodograma')
periodogram(na.omit(bkCycle), main = 'Filtro Baxter-King', col = 'red', 
            xlab = 'Frecuencia', ylab = 'Periodograma')
periodogram(cfCycle, main = 'Filtro Christiano-Fitzgerald', col = 'red', 
            xlab = 'Frecuencia', ylab = 'Periodograma')
par(mfrow = c(1, 1))
