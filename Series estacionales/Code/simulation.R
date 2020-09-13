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

# 2| Simulación -----------------------------------------------------------
set.seed(123)

# 2.1| Modelo acelerador de Samuelson -------------------------------------
# Parámetros del MAS para la economía estadounidense:
alpha0   = 2984.67
alpha1   = 0.6
beta     = 2.43
tao_disp = 1 - 0.3576
g        = 22092.14

# Parámetros solución del sistema de ecuaciones en diferencias:
phi0 = g - alpha0
phi1 = alpha1 * tao_disp * (1 + beta)
phi2 = - alpha1 * beta * tao_disp

# Valores iniciales:
y0   = 59999.51
y1   = 62868.92

# 2.1.1| Alrededor de una media -------------------------------------------
simulations = 200

constantGDP <- function() {
  Y   = c(y0, y1)
  e   = rnorm(n = 100, mean = 0, sd = 1242)
  for (t in 2:length(e)) {
    Y[t+1] = phi0 + phi1*Y[t] + phi2*Y[t-1] + e[t]
  }
  return(Y)
}

simulationsConstantGDP = replicate(n = simulations, constantGDP())

yMax <- max(simulationsConstantGDP)
yMin <- min(simulationsConstantGDP)
plot(simulationsConstantGDP[, 1], 
     type = 'l', col = '#ff000010', ylim = c(yMin, yMax),
     main = 'Simulación del PIB para la economía estadounidense \n Se asume el proceso no tiene tendencia',
     sub  = 'Solución al sistema de ecuaciones en diferencia de Samuelson',
     ylab = 'PIB',
     xlab = 'Tiempo')
for (i in 2:simulations){
  lines(simulationsConstantGDP[,i], type='l', col='#ff000010')
}

# 2.1.2| Alrededor de una tendencia ---------------------------------------
simulations = 200

trendGDP <- function() {
  Y   = c(y0, y1)
  e   = rnorm(n = 100, mean = 0, sd = 1242)
  for (t in 2:length(e)) {
    Y[t+1] = phi0 + phi1*Y[t] + phi2*Y[t-1] + 1389.9*t + e[t]
  }
  return(Y)
}

simulationstrendGDP = replicate(n = simulations, trendGDP())

yMax <- max(simulationstrendGDP)
yMin <- min(simulationstrendGDP)
plot(simulationstrendGDP[, 1], 
     type = 'l', col = '#ff000010', ylim = c(yMin, yMax),
     main = 'Simulación del PIB para la economía estadounidense \n Se asume el proceso sí tiene tendencia',
     sub  = 'Solución al sistema de ecuaciones en diferencia de Samuelson',
     ylab = 'PIB',
     xlab = 'Tiempo')
for (i in 2:simulations){
  lines(simulationstrendGDP[,i], type='l', col='#ff000010')
}

# 2.2| Procesos estacionales ----------------------------------------------


