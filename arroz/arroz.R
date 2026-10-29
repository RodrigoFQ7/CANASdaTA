# --- 1. CARGA DE LIBRERÍAS ---
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)

# --- 2. IMPORTACIÓN Y LIMPIEZA ---
# Asegúrate de que la ruta apunte a tu carpeta de arroz
archivos_arroz <- list.files("~/ProyectoLDM/arroz/", pattern = "\\.(csv|CSV)$", full.names = TRUE, recursive = TRUE)
message("Archivos de precio de arroz encontrados: ", length(archivos_arroz))

datos_arroz <- archivos_arroz %>%
  map_dfr(~read.csv(.x, fileEncoding = "latin1") %>% 
            # Filtramos únicamente por Unidad KG, sin restricciones en Especificación
            filter(toupper(trimws(Unidad)) == "KG"))

resumen_arroz <- datos_arroz %>%
  group_by(Anio, Mes) %>%
  summarise(precio_promedio_arroz = mean(Precio.promedio, na.rm = TRUE),
            desviacion_arroz = sd(Precio.promedio, na.rm = TRUE), .groups = "drop")

# --- 3. VISUALIZACIÓN (CORREGIDO) ---
resumen_arroz %>%
  mutate(fecha_arroz = as.Date(paste(Anio, Mes, "01", sep = "-"))) %>%
  ggplot(aes(x = fecha_arroz, y = precio_promedio_arroz)) + # Aquí estaba el error
  geom_line(color = "seagreen") + geom_point() +
  geom_errorbar(aes(ymin = precio_promedio_arroz - desviacion_arroz, 
                    ymax = precio_promedio_arroz + desviacion_arroz), width = 0.2, color = "gray") +
  theme_minimal() + labs(title = "Evolución del Precio del Arroz", y = "Precio (KG)")

# --- 4. MODELADO Y PRONÓSTICO ---
ts_serie_arroz <- resumen_arroz %>%
  mutate(fecha_arroz = yearmonth(paste(Anio, Mes, sep = "-"))) %>%
  as_tsibble(index = fecha_arroz)

modelo_arroz <- ts_serie_arroz %>% model(arima = ARIMA(precio_promedio_arroz), ets = ETS(precio_promedio_arroz))
pronostico_arroz <- modelo_arroz %>% forecast(h = 12)

# Gráfico de pronóstico
pronostico_arroz %>% autoplot(ts_serie_arroz) + theme_minimal() + labs(title = "Pronóstico Precio Arroz")

# --- 5. ANÁLISIS ESTACIONAL ---
modelo_descomp_arroz <- ts_serie_arroz %>% model(STL(precio_promedio_arroz ~ season(period = 12)))
components(modelo_descomp_arroz) %>% autoplot()

estacionalidad_mes_arroz <- components(modelo_descomp_arroz) %>%
  as_tsibble() %>%
  mutate(mes_etiqueta = month(fecha_arroz, label = TRUE)) %>%
  group_by(mes_etiqueta) %>%
  summarise(efecto_estacional_arroz = mean(season_12))

ggplot(estacionalidad_mes_arroz, aes(x = mes_etiqueta, y = efecto_estacional_arroz, group = 1)) +
  geom_line(color = "darkgreen", size = 1.2) + geom_point(size = 3) +
  theme_minimal() + labs(title = "Ciclo Estacional del Precio del Arroz", y = "Desviación (pesos)")
