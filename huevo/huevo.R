# --- 1. CARGA DE LIBRERÍAS ---
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)

# --- 2. IMPORTACIÓN Y LIMPIEZA ---
archivos_huevo <- list.files("~/ProyectoLDM/huevo/", pattern = "\\.(csv|CSV)$", full.names = TRUE, recursive = TRUE)
message("Archivos de precio de huevo encontrados: ", length(archivos_huevo))

datos_huevo <- archivos_huevo %>%
  map_dfr(~read.csv(.x) %>% filter(toupper(trimws(Unidad)) == "KG"))

resumen_huevo <- datos_huevo %>%
  group_by(Anio, Mes) %>%
  summarise(precio_promedio_huevo = mean(Precio.promedio, na.rm = TRUE),
            desviacion_huevo = sd(Precio.promedio, na.rm = TRUE), .groups = "drop")

# --- 3. VISUALIZACIÓN ---
resumen_huevo %>%
  mutate(fecha_huevo = as.Date(paste(Anio, Mes, "01", sep = "-"))) %>%
  ggplot(aes(x = fecha_huevo, y = precio_promedio_huevo)) +
  geom_line(color = "blue") + geom_point() +
  geom_errorbar(aes(ymin = precio_promedio_huevo - desviacion_huevo, 
                    ymax = precio_promedio_huevo + desviacion_huevo), width = 0.2, color = "gray") +
  theme_minimal() + labs(title = "Evolución del Precio del Huevo", y = "Precio (KG)")

# --- 4. MODELADO Y PRONÓSTICO ---
ts_serie_huevo <- resumen_huevo %>%
  mutate(fecha_huevo = yearmonth(paste(Anio, Mes, sep = "-"))) %>%
  as_tsibble(index = fecha_huevo)

modelo_huevo <- ts_serie_huevo %>% model(arima = ARIMA(precio_promedio_huevo), ets = ETS(precio_promedio_huevo))
pronostico_huevo <- modelo_huevo %>% forecast(h = 12)

# Gráfico de pronóstico y tabla de datos
pronostico_huevo %>% autoplot(ts_serie_huevo) + theme_minimal()
datos_pronostico_huevo <- pronostico_huevo %>% hilo() %>% as_tibble()

# --- 5. ANÁLISIS ESTACIONAL ---
modelo_descomp_huevo <- ts_serie_huevo %>% model(STL(precio_promedio_huevo ~ season(period = 12)))
components(modelo_descomp_huevo) %>% autoplot()

estacionalidad_mes_huevo <- components(modelo_descomp_huevo) %>%
  as_tsibble() %>%
  mutate(mes_etiqueta = month(fecha_huevo, label = TRUE)) %>%
  group_by(mes_etiqueta) %>%
  summarise(efecto_estacional_huevo = mean(season_12))

ggplot(estacionalidad_mes_huevo, aes(x = mes_etiqueta, y = efecto_estacional_huevo, group = 1)) +
  geom_line(color = "orange", size = 1.2) + geom_point(size = 3) +
  theme_minimal() + labs(title = "Ciclo Estacional del Precio del Huevo", y = "Desviación (pesos)")

