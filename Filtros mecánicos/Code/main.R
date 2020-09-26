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
library(here)      # Direcciones relativas.
library(mFilter)   # Filtro de HP.

# 2| Filtrado -------------------------------------------------------------
# 2.1| Hodrick-Prescott ---------------------------------------------------
HP <- hpfilter(serieSA, freq = 14400, drift = T)
plot(HP)


