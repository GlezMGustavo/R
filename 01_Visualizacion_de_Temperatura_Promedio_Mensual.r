rm(list=ls(all=TRUE)); if(length(dev.list())>0){dev.off()}
# Realizado el 2022-03-26, como asesoramiento de correcición de script.
# ────────────────────────────────────────────────────────────────────────────────
# 🌡️ Análisis exploratorio de temperatura mensual promedio (1961–2017)
#
# 📍 Descripción:
# Este script genera una visualización de series de tiempo a partir de datos simulados 
# de temperatura mensual promedio para una estación climática específica (Estación 1001), 
# cubriendo el período de 1961 a 2017.
#
# 🎯 Objetivo:
# Proporcionar una herramienta visual para detectar patrones de largo plazo en la temperatura, 
# incluyendo variaciones estacionales, tendencias generales y posibles señales de cambio climático.
#
# 🛠️ Lo que hace el código:
# - Simula datos mensuales de temperatura con una distribución normal.
# - Genera un gráfico de línea con `ggplot2` que muestra la evolución temporal.
# - Incluye una línea de suavizado (`geom_smooth`) para destacar tendencias generales.
# - Personaliza el eje temporal, etiquetas y diseño del gráfico para facilitar su interpretación.
#
# 📊 Utilidad:
# Esta visualización es útil como parte de un análisis exploratorio inicial en estudios climatológicos, 
# ayudando a identificar tendencias anómalas, ciclos estacionales o puntos de inflexión relevantes.
# ────────────────────────────────────────────────────────────────────────────────

#### Librerias
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

#### Datos
fecha <- seq(ymd("1961-01-01"), ymd("2017-12-01"), by = "month")

df <- tibble(
  Year = year(fecha),
  Mes = month(fecha),
  Promedio_mensual = rnorm(length(fecha), mean = 34.66, sd = 3.13)
)

df <- df %>%
  mutate(Fecha = ymd(paste(Year, Mes, "01", sep = "-")))

#### Gráfico
ggplot(df, aes(x = Fecha, y = Promedio_mensual)) +
  geom_line(color = "blue", size = 1, alpha = 0.85) +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linetype = "dashed", size = 0.8) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(
    limits = c(20, 36),
    breaks = seq(20, 36, by = 2)
  ) +
  labs(
    title = "Temperatura mensual promedio",
    subtitle = "Estación 1001 (1961–2017)",
    x = "Año",
    y = "Temperatura promedio (°C)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
