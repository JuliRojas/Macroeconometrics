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
graphics.off()
library(readr)     # Lectura de los .csv.
library(here)      # Direcciones relativas.
library(tidyverse) # Manejo de datos y gráficas.
library(urca)      # Pruebas de raíz unitaria.

# 1.2| Bases de datos -----------------------------------------------------
futuresGold   <- read_csv(here('input', 'futuresGold.csv'),
                          locale = locale(decimal_mark = ',', grouping_mark = '.'))
futuresSilver <- read_csv(here('input', 'futuresSilver.csv'), 
                          locale = locale(decimal_mark = ',', grouping_mark = '.'))

futuresGold   <- futuresGold[nrow(futuresGold):1, 'Último'] %>%
  ts(start = c(1980, 1), frequency = 12) %>%
  as.numeric() %>%
  ts(start = c(1980, 1), frequency = 12)

futuresSilver <- futuresSilver[nrow(futuresSilver):1, 'Último'] %>%
  ts(start = c(1980, 1), frequency = 12) %>%
  as.numeric() %>%
  ts(start = c(1980, 1), frequency = 12)

# 1.3| Visualización ------------------------------------------------------
par(mfrow = c(1, 2))
plot(futuresGold,
     main = 'Futuro del oro',
     sub  = 'Elaboración propia, con datos extraídos de: Investing.',
     ylab = 'Precio',
     xlab = 'Fecha',
     col  = '#E41A1C',
     type = 'l')
plot(futuresSilver,
     main = 'Futuro de la plata',
     sub  = 'Elaboración propia, con datos extraídos de: Investing.',
     ylab = 'Precio',
     xlab = 'Fecha',
     col  = '#377EB8',
     type = 'l')
par(mfrow = c(1, 1))

# 2| Pruebas de raíz unitaria ---------------------------------------------
# 2.1| Oro ----------------------------------------------------------------
# ¿Es integrada?
goldADF <- ur.df(futuresGold, type = 'trend', selectlags = 'BIC')
summary(goldADF)

goldADF <- ur.df(futuresGold, type = 'drift', selectlags = 'BIC')
summary(goldADF)

goldADF <- ur.df(futuresGold, type = 'none', selectlags = 'BIC')
summary(goldADF)

# ¿Lo seguirá siendo?
goldADF <- ur.df(diff(futuresGold), type = 'trend', selectlags = 'BIC')
summary(goldADF)

# Conclusión: I(1).

# 2.2| Plata --------------------------------------------------------------
# ¿Es integrada?
silverADF <- ur.df(futuresSilver, type = 'trend', selectlags = 'BIC')
summary(silverADF)

silverKPSS <- ur.kpss(futuresSilver, type = 'tau', lags = 'short')
summary(silverKPSS)

silverPP = ur.pp(futuresSilver, type = 'Z-tau', model = 'trend', lags = 'short')
summary(silverPP)

# Conclusión: I(1).

# 3| Estimación -----------------------------------------------------------
# 3.1| Oro = f(Plata) -----------------------------------------------------
data       <- ts.union(futuresGold, futuresSilver)
regression <- lm(futuresGold ~ futuresSilver, data = data)
summary(regression)
plot.ts(regression$residuals)

# 3.2| Errores ruido blanco -----------------------------------------------
regressionErrors     <- regression$residuals
regressionErrorsTest <- ur.df(regression$residuals, type = 'none', selectlags = 'BIC')
summary(regressionErrorsTest)

# 3.3| Corrección de errores ----------------------------------------------
futuresGoldDifference     <- diff(futuresGold)[-1]
futuresSilverDifference   <- diff(futuresSilver)[-1]
errorsEC                 <- regressionErrors[-1:-2]
futuresGoldDifferenceL1   <- diff(futuresGold)[-(length(futuresGold) - 1)]
futuresSilverDifferenceL1 <- diff(futuresSilver[-(length(futuresSilver) - 1)])

modelGoldEC <- lm(futuresGoldDifference ~ errorsEC + futuresGoldDifferenceL1 + futuresSilverDifferenceL1)
summary(modelGoldEC)

modelSilverEC <- lm(futuresSilverDifference ~ errorsEC + futuresGoldDifferenceL1 + futuresSilverDifferenceL1)
summary(modelSilverEC)