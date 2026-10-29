
# --- 1. CARGA DE LIBRERÍAS ---
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)

# --- 2. IMPORTACIÓN Y LIMPIEZA ---
archivos_tortilla <- list.files("~/ProyectoLDM/tortillas/", pattern = "\\.(csv|CSV)$", full.names = TRUE, recursive = TRUE)
message("Archivos de precio de tortilla encontrados: ", length(archivos_tortilla))

datos_tortilla <- archivos_tortilla %>%
  map_dfr(~read.csv(.x, fileEncoding = "latin1") %>% # Usamos R base para ser más permisivos
            filter(toupper(trimws(Unidad)) == "KG",
                   str_detect(toupper(trimws(Especificacion)), "A GRANEL")))

resumen_tortilla <- datos_tortilla %>%
  group_by(Anio, Mes) %>%
  summarise(precio_promedio_tortilla = mean(Precio.promedio, na.rm = TRUE),
            desviacion_tortilla = sd(Precio.promedio, na.rm = TRUE), .groups = "drop")

# --- 3. VISUALIZACIÓN ---
resumen_tortilla %>%
  mutate(fecha_tortilla = as.Date(paste(Anio, Mes, "01", sep = "-"))) %>%
  ggplot(aes(x = fecha_tortilla, y = precio_promedio_tortilla)) +
  geom_line(color = "blue") + geom_point() +
  geom_errorbar(aes(ymin = precio_promedio_tortilla - desviacion_tortilla, 
                    ymax = precio_promedio_tortilla + desviacion_tortilla), width = 0.2, color = "gray") +
  theme_minimal() + labs(title = "Evolución del Precio de la Tortilla (A Granel)", y = "Precio (KG)")

# --- 4. MODELADO Y PRONÓSTICO ---
ts_serie_tortilla <- resumen_tortilla %>%
  mutate(fecha_tortilla = yearmonth(paste(Anio, Mes, sep = "-"))) %>%
  as_tsibble(index = fecha_tortilla)

modelo_tortilla <- ts_serie_tortilla %>% model(arima = ARIMA(precio_promedio_tortilla), ets = ETS(precio_promedio_tortilla))
pronostico_tortilla <- modelo_tortilla %>% forecast(h = 12)

# Gráfico de pronóstico y tabla de datos
pronostico_tortilla %>% autoplot(ts_serie_tortilla) + theme_minimal()
datos_pronostico_tortilla <- pronostico_tortilla %>% hilo() %>% as_tibble()

# --- 5. ANÁLISIS ESTACIONAL ---
modelo_descomp_tortilla <- ts_serie_tortilla %>% model(STL(precio_promedio_tortilla ~ season(period = 12)))
components(modelo_descomp_tortilla) %>% autoplot()

estacionalidad_mes_tortilla <- components(modelo_descomp_tortilla) %>%
  as_tsibble() %>%
  mutate(mes_etiqueta = month(fecha_tortilla, label = TRUE)) %>%
  group_by(mes_etiqueta) %>%
  summarise(efecto_estacional_tortilla = mean(season_12))

ggplot(estacionalidad_mes_tortilla, aes(x = mes_etiqueta, y = efecto_estacional_tortilla, group = 1)) +
  geom_line(color = "darkblue", size = 1.2) + geom_point(size = 3) +
  theme_minimal() + labs(title = "Ciclo Estacional del Precio de la Tortilla", y = "Desviación (pesos)")
