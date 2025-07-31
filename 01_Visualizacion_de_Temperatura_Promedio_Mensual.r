# Realizado el 2022-03-26, como asesoramiento de correcición de script.
# Este script en R realiza un análisis exploratorio y visualización de series de tiempo de temperatura mensual promedio, 
# desde 1961 hasta 2017, para una estación climática específica (1001)
# El gráfico final permite visualizar tendencias de largo plazo en la temperatura mensual promedio, 
# facilitando la detección de patrones climáticos, cambios estacionales o posibles efectos del cambio climático en la estación analizada.

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
