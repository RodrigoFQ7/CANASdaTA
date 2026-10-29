# --- 1. CARGA DE LIBRERÍAS ---
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)

# --- 2. IMPORTACIÓN Y LIMPIEZA ---
# Asegúrate de que la ruta apunte a tu carpeta de cebolla
archivos_cebolla <- list.files("~/ProyectoLDM/cebolla/", pattern = "\\.(csv|CSV)$", full.names = TRUE, recursive = TRUE)
message("Archivos de precio de cebolla encontrados: ", length(archivos_cebolla))

datos_cebolla <- archivos_cebolla %>%
  map_dfr(~read.csv(.x, fileEncoding = "latin1") %>% 
            # Filtramos por Unidad KG y que la Especificacion contenga BLANCA
            filter(toupper(trimws(Unidad)) == "KG",
                   str_detect(toupper(trimws(Especificacion)), "BLANCA")))

resumen_cebolla <- datos_cebolla %>%
  group_by(Anio, Mes) %>%
  summarise(precio_promedio_cebolla = mean(Precio.promedio, na.rm = TRUE),
            desviacion_cebolla = sd(Precio.promedio, na.rm = TRUE), .groups = "drop")

# --- 3. VISUALIZACIÓN ---
resumen_cebolla %>%
  mutate(fecha_cebolla = as.Date(paste(Anio, Mes, "01", sep = "-"))) %>%
  ggplot(aes(x = fecha_cebolla, y = precio_promedio_cebolla)) +
  geom_line(color = "orange") + geom_point() +
  geom_errorbar(aes(ymin = precio_promedio_cebolla - desviacion_cebolla, 
                    ymax = precio_promedio_cebolla + desviacion_cebolla), width = 0.2, color = "gray") +
  theme_minimal() + labs(title = "Evolución del Precio de la Cebolla (Blanca)", y = "Precio (KG)")

# --- 4. MODELADO Y PRONÓSTICO ---
ts_serie_cebolla <- resumen_cebolla %>%
  mutate(fecha_cebolla = yearmonth(paste(Anio, Mes, sep = "-"))) %>%
  as_tsibble(index = fecha_cebolla)

modelo_cebolla <- ts_serie_cebolla %>% model(arima = ARIMA(precio_promedio_cebolla), ets = ETS(precio_promedio_cebolla))
pronostico_cebolla <- modelo_cebolla %>% forecast(h = 12)

# Gráfico de pronóstico
pronostico_cebolla %>% autoplot(ts_serie_cebolla) + theme_minimal() + labs(title = "Pronóstico Precio Cebolla Blanca")

# --- 5. ANÁLISIS ESTACIONAL ---
modelo_descomp_cebolla <- ts_serie_cebolla %>% model(STL(precio_promedio_cebolla ~ season(period = 12)))
components(modelo_descomp_cebolla) %>% autoplot()

estacionalidad_mes_cebolla <- components(modelo_descomp_cebolla) %>%
  as_tsibble() %>%
  mutate(mes_etiqueta = month(fecha_cebolla, label = TRUE)) %>%
  group_by(mes_etiqueta) %>%
  summarise(efecto_estacional_cebolla = mean(season_12))

ggplot(estacionalidad_mes_cebolla, aes(x = mes_etiqueta, y = efecto_estacional_cebolla, group = 1)) +
  geom_line(color = "darkorange", size = 1.2) + geom_point(size = 3) +
  theme_minimal() + labs(title = "Ciclo Estacional del Precio de la Cebolla Blanca", y = "Desviación (pesos)")
