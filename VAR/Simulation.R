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

# Preparación -------------------------------------------------------------
set.seed(123456)
library(urca)
library(vars)
library(dynlm)

# 1| Definición de las variables ------------------------------------------
T = 240                                        # Tamaño de la muestra.
y_t = as.matrix(cbind(rep(0, T), rep(0, T)))   # Variable dependiente.
u_t = as.matrix(cbind(rnorm(T), rnorm(T)))     # Errores aleatorios.
A_t = as.matrix(cbind(c(0.3, 0.5), c(0, 0.6))) # Coeficientes.

# 2| Simulación del modelo ------------------------------------------------
for (i in 2:T) {
  y_t[i,] = A_t %*% y_t[i-1,] + u_t[i,]
}

# Análisis de las series de tiempo:
# - Gráfica:
plot.ts(y_t[,1])
plot.ts(y_t[,2])
# - Raíz unitaria: Las series son estacionarias.
summary(ur.df(y_t[,1], type='none', selectlags = 'AIC'))
summary(ur.df(y_t[,2], type='none', selectlags = 'AIC'))

# 3| Selección del VAR ----------------------------------------------------
VARselect(y_t) # Selección del número de rezagos.
               # Se elige p = 1.

# 3.1| Tendencia y constante ----------------------------------------------
# VAR: Estima el VAR a través de un OLS ecuación por ecuación.
var_tendecia = VAR(y_t, p = 1, type = 'both')
summary(var_tendecia)

# 3.2| Sin componentes determinísticos ------------------------------------
var_sin= VAR(y_t, p = 1, type = 'none')
summary(var_sin)

u_t_estimado = residuals(var_sin)
sigma_ut = summary(var_sin)$covres

# 3.3| Intento de VAR(2) --------------------------------------------------
# Ningún coeficiente es significativo en el rezago 2.
var_sin2= VAR(y_t, p = 2, type = 'none')
summary(var_sin2)

# 4| Condiciones de estabilidad -------------------------------------------
# Aclaración: - Roots devuelve los módulos, no las raíces.
#               Devuelve los valores propios de una matriz.
#             - Acoef devuelve los coeficientes estimados de las matrices.
#             - eigen es la descomposición espectral de una matriz.
# Módulos del polinomio característico: Menores a la unidad implica estabilidad.

# 4.1| Matriz estimada ----------------------------------------------------
# Primera forma: Módulos del polinomio.
modulos_est = roots(var_sin)
modulos_est
# Segunda forma: Valores propios de la matriz A.
A_est = Acoef(var_sin)
modulo_est2 = eigen(as.matrix(A_est[[1]]))

# Forma manual para las raíces:
# Mod: Extrae el módulo.
modulo1=Mod(modulo_est2$values[1]) # Modulos menores a uno.
modulo2=Mod(modulo_est2$values[2])
raiz1=1/modulo1                    # Raíces fuera del círculo unitario.
raiz2=1/modulo2

# 4.2| Matriz teórica -----------------------------------------------------
# Los valores propios de la simulación son:
modulos_teoricos <- eigen(A_t)
modulos_teoricos$values
# Es acorde dado que la matriz es triangular inferior y los elementos de la
# diagonal principal son sus valores propios. Luego, el proceso es estable.

# Las raíces de la simulación son:
raices_teoricas <- polyroot(c(1, -0.9, 0.18))
paste0('Raíces polinomio: |z_i| = ', round(abs(raices_teoricas), 4))

# 4.3| Comparación --------------------------------------------------------
# Módulos con la matriz estimada:
modulos_est
# Módulos con la matriz teórica
modulos_teoricos$values

# 5| Estimación ecuación por ecuación -------------------------------------
# Comparación de los resultados:
# - Creación de las variables, contemporáneas y rezagas.
y1     = y_t[,1]
y1lag1 = c(NA, y1[1:T-1])
y2     = y_t[,2]
y2lag1 = c(NA, y2[1:T-1])

# Creación de las estimaciones:
ecuacion_y1 = lm(y1~y1lag1+y2lag1-1)
summary(ecuacion_y1)
res_ec1     = residuals(ecuacion_y1)
ecuacion_y2 = lm(y2~y1lag1+y2lag1-1)
summary(ecuacion_y2)
res_ec2     = residuals(ecuacion_y2)
# Matriz de varianza y covarianza de los errores.
sigmau1 = var(res_ec1)*(T-1)/(T-2)
sigmau2 = var(res_ec2)*(T-1)/(T-2)
covu1u2 = cov(res_ec1,res_ec2)*(T-1)/(T-2)
Sigma_U_estimado = as.matrix(cbind(c(sigmau1, covu1u2), c(covu1u2, sigmau2)))

# Comparación:
Acoef(var_sin)    # Coeficientes del VAR(1) en matriz.
coef(ecuacion_y1) # Coeficientes estimados por MCO de la primera ecuación.
coef(ecuacion_y2) # Coeficientes estimados por MCO de la segunda ecuación.

# Matriz de varianzas y covarianzas:
sigma_ut
Sigma_U_estimado

# 5.1| Descomposición de Choleski -----------------------------------------
P_transpuesta = chol(sigma_ut)
P             = t(P_transpuesta)
P_transpuesta %*% P               # El producto es la matriz de covarianzas.

# 6| Impulso-respuesta ortogonal ------------------------------------------
# Choque (1, 0) en el momento t = 0.
choque = as.matrix(c(1,0))
A_1    = as.matrix(A_est[[1]])
IRF0 = P%*%choque
IRF1 = A_1%*%IRF0
IRF2 = A_1%*%IRF1
IRF3 = A_1%*%IRF2
IRF4 = A_1%*%IRF3
IRF5 = A_1%*%IRF4

IRF_y1_y1 = irf(var_sin, impulse = 'y1', response='y1', boot=TRUE)
IRF_y1_y1
plot(IRF_y1_y1)
IRF_y1_y2 = irf(var_sin, impulse = 'y1', response='y2', boot=TRUE)
IRF_y1_y2 
plot(IRF_y1_y2)

# Comparación automático vs. manual
IRF_en_R = cbind (as.matrix(IRF_y1_y1$irf$y1), as.matrix(IRF_y1_y2$irf$y1))
IRF_en_R
IRF_calculado = cbind(IRF0, IRF1, IRF2, IRF3, IRF4, IRF4 )
t(IRF_calculado)

# 7| Causalidad de Granger ------------------------------------------------
causality(var_sin, cause = c('y2'))
causality(var_sin, cause = c('y1'))