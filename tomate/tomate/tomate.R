# --- 1. CARGA DE LIBRERÍAS ---
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)

# --- 2. IMPORTACIÓN Y LIMPIEZA ---
archivos_tomate <- list.files("~/ProyectoLDM/tomate/", pattern = "\\.(csv|CSV)$", full.names = TRUE, recursive = TRUE)
message("Archivos de precio de tomate encontrados: ", length(archivos_tomate))

datos_tomate <- archivos_tomate %>%
  map_dfr(~read.csv(.x) %>% filter(toupper(trimws(Unidad)) == "KG"))

resumen_tomate <- datos_tomate %>%
  group_by(Anio, Mes) %>%
  summarise(precio_promedio_tomate = mean(Precio.promedio, na.rm = TRUE),
            desviacion_tomate = sd(Precio.promedio, na.rm = TRUE), .groups = "drop")

# --- 3. VISUALIZACIÓN ---
resumen_tomate %>%
  mutate(fecha_tomate = as.Date(paste(Anio, Mes, "01", sep = "-"))) %>%
  ggplot(aes(x = fecha_tomate, y = precio_promedio_tomate)) +
  geom_line(color = "red") + geom_point() +
  geom_errorbar(aes(ymin = precio_promedio_tomate - desviacion_tomate, 
                    ymax = precio_promedio_tomate + desviacion_tomate), width = 0.2, color = "gray") +
  theme_minimal() + labs(title = "Evolución del Precio del Tomate", y = "Precio (KG)")

# --- 4. MODELADO Y PRONÓSTICO ---
ts_serie_tomate <- resumen_tomate %>%
  mutate(fecha_tomate = yearmonth(paste(Anio, Mes, sep = "-"))) %>%
  as_tsibble(index = fecha_tomate)

modelo_tomate <- ts_serie_tomate %>% model(arima = ARIMA(precio_promedio_tomate), ets = ETS(precio_promedio_tomate))
pronostico_tomate <- modelo_tomate %>% forecast(h = 12)

# Gráfico de pronóstico y tabla de datos
pronostico_tomate %>% autoplot(ts_serie_tomate) + theme_minimal()
datos_pronostico_tomate <- pronostico_tomate %>% hilo() %>% as_tibble()

# --- 5. ANÁLISIS ESTACIONAL ---
modelo_descomp_tomate <- ts_serie_tomate %>% model(STL(precio_promedio_tomate ~ season(period = 12)))
components(modelo_descomp_tomate) %>% autoplot()

estacionalidad_mes_tomate <- components(modelo_descomp_tomate) %>%
  as_tsibble() %>%
  mutate(mes_etiqueta = month(fecha_tomate, label = TRUE)) %>%
  group_by(mes_etiqueta) %>%
  summarise(efecto_estacional_tomate = mean(season_12))

ggplot(estacionalidad_mes_tomate, aes(x = mes_etiqueta, y = efecto_estacional_tomate, group = 1)) +
  geom_line(color = "darkgreen", size = 1.2) + geom_point(size = 3) +
  theme_minimal() + labs(title = "Ciclo Estacional del Precio del Tomate", y = "Desviación (pesos)")
