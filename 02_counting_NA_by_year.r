rm(list=ls(all=TRUE)); if(length(dev.list())>0){dev.off()}
# Realizado el 2022-04-25 como creación completa de script para cumplir un objetivo.
# ────────────────────────────────────────────────────────────────────────────────
# 🧪 Propósito del script
# Este código implementa un enfoque robusto para procesar series temporales diarias 
# con posibles datos faltantes (NA), común en climatología, sensores ambientales 
# o cualquier fuente de datos que registre información día a día.
#
# 🎯 Objetivos principales:
# 1. Detectar bloques de valores NA consecutivos en series diarias (indicador de cortes prolongados).
# 2. Calcular promedios mensuales solo si los datos cumplen ciertos criterios de calidad.
#
# ✅ Criterios de calidad:
# - Se utiliza `consec_na` para identificar cuántos días están afectados por bloques de NA consecutivos 
#   (por ejemplo, 3 días seguidos sin datos). Este indicador es más estricto, pues refleja fallos prolongados.
# - Se mide `total_na`, la cantidad total de NA en el mes, útil para detectar fallos dispersos o aislados.
# - Si un mes tiene demasiados NA consecutivos o demasiados NA totales, su promedio mensual es descartado (NA).
#
# 📌 Conclusión:
# Combinar ambos indicadores (NA consecutivos + NA totales) permite filtrar los meses con datos insuficientes 
# o de baja calidad, evitando así obtener promedios distorsionados o poco confiables.
# ────────────────────────────────────────────────────────────────────────────────

####
library(lubridate)  
library(reshape)   

# Función para detectar n o más NA consecutivos

detectar_na_consecutivos <- function(x, n = 2) {
  # Identifica posiciones donde hay n o más NA consecutivos
  rle_na <- rle(is.na(x))
  rle_na$values <- rle_na$values & rle_na$lengths >= n
  inverse.rle(rle_na)
}

# Ejemplo simple con matriz
m <- matrix(c(1, NA, 2, NA, 1, NA, NA, NA, 1, 1, 4, 3, 4, 8, 5, 6), ncol = 2)
na_index <- apply(m, 2, detectar_na_consecutivos, n = 3)  # Detecta NA consecutivos de largo 3 o más
print(sum(na_index[, 1]))  # Cuántos en la primera columna

# Generar una serie temporal diaria ficticia (2000-2024)
fechas <- seq(ymd("2000-01-01"), ymd("2024-12-31"), by = "day")
# ymd("1961-01-01"), ymd("2017-12-31"), by = "day")
anio <- year(fechas)
mes <- month(fechas)
dia <- day(fechas)

# Simular datos con NA aleatorios
set.seed(123)
valores <- runif(length(fechas), min = 0, max = 50)
valores[sample(seq_along(valores), 1000)] <- NA  # Asignar 1000 NA aleatoriamente

datos <- data.frame(anio, mes, dia, valores)

# Reorganizar datos por mes (reshape ancho)
datos_mes <- reshape(datos, idvar = c("anio", "mes"), timevar = "dia", direction = "wide")
dias_del_mes <- days_in_month(seq(ymd("2000-01-01"), ymd("2024-12-31"), by = "month"))

# Extraer solo las columnas de días
matriz_dias <- datos_mes[, grep("^valores\\.", names(datos_mes))]

# Detectar NA consecutivos y calcular promedios mensuales válidos
n_meses <- nrow(matriz_dias)
promedios <- ml <- nal <- numeric(n_meses)

for (i in 1:n_meses) {
  dias_validos <- 1:dias_del_mes[i]
  serie <- as.numeric(matriz_dias[i, dias_validos])
  
  na_consec <- detectar_na_consecutivos(serie, n = 2)
  ml[i] <- sum(na_consec)
  nal[i] <- sum(is.na(serie))
  
  if (ml[i] >= 3 || nal[i] > 5) {
    promedios[i] <- NA
  } else {
    promedios[i] <- mean(serie, na.rm = TRUE)
  }
}

# Añadir resultados al data frame original
datos_mes$promedio <- promedios # Cantidad de NA consecutivos (en días individuales)
datos_mes$consec_na <- ml # Cantidad de NA totales en el mes
datos_mes$total_na <- nal # Promedio mensual con filtro de calidad

