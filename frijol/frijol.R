# --- 1. CARGA DE LIBRERÍAS ---
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)

# --- 2. IMPORTACIÓN Y LIMPIEZA ---
archivos_frijol <- list.files("~/ProyectoLDM/frijol/", pattern = "\\.(csv|CSV)$", full.names = TRUE, recursive = TRUE)
message("Archivos de precio de frijol encontrados: ", length(archivos_frijol))

datos_frijol <- archivos_frijol %>%
  map_dfr(~read.csv(.x) %>% 
            # Filtramos solo KG y buscamos "pinto" dentro del texto de Especificacion
            filter(toupper(trimws(Unidad)) == "KG",
                   str_detect(toupper(Especificacion), "PINTO")))

resumen_frijol <- datos_frijol %>%
  group_by(Anio, Mes) %>%
  summarise(precio_promedio_frijol = mean(Precio.promedio, na.rm = TRUE),
            desviacion_frijol = sd(Precio.promedio, na.rm = TRUE), .groups = "drop")

# --- 3. VISUALIZACIÓN ---
resumen_frijol %>%
  mutate(fecha_frijol = as.Date(paste(Anio, Mes, "01", sep = "-"))) %>%
  ggplot(aes(x = fecha_frijol, y = precio_promedio_frijol)) +
  geom_line(color = "brown") + geom_point() +
  geom_errorbar(aes(ymin = precio_promedio_frijol - desviacion_frijol, 
                    ymax = precio_promedio_frijol + desviacion_frijol), width = 0.2, color = "gray") +
  theme_minimal() + labs(title = "Evolución del Precio del Frijol", y = "Precio (KG)")

# --- 4. MODELADO Y PRONÓSTICO ---
ts_serie_frijol <- resumen_frijol %>%
  mutate(fecha_frijol = yearmonth(paste(Anio, Mes, sep = "-"))) %>%
  as_tsibble(index = fecha_frijol)

modelo_frijol <- ts_serie_frijol %>% model(arima = ARIMA(precio_promedio_frijol), ets = ETS(precio_promedio_frijol))
pronostico_frijol <- modelo_frijol %>% forecast(h = 12)

# Gráfico de pronóstico y tabla de datos
pronostico_frijol %>% autoplot(ts_serie_frijol) + theme_minimal()
datos_pronostico_frijol <- pronostico_frijol %>% hilo() %>% as_tibble()

# --- 5. ANÁLISIS ESTACIONAL ---
modelo_descomp_frijol <- ts_serie_frijol %>% model(STL(precio_promedio_frijol ~ season(period = 12)))
components(modelo_descomp_frijol) %>% autoplot()

estacionalidad_mes_frijol <- components(modelo_descomp_frijol) %>%
  as_tsibble() %>%
  mutate(mes_etiqueta = month(fecha_frijol, label = TRUE)) %>%
  group_by(mes_etiqueta) %>%
  summarise(efecto_estacional_frijol = mean(season_12))

ggplot(estacionalidad_mes_frijol, aes(x = mes_etiqueta, y = efecto_estacional_frijol, group = 1)) +
  geom_line(color = "darkred", size = 1.2) + geom_point(size = 3) +
  theme_minimal() + labs(title = "Ciclo Estacional del Precio del Frijol", y = "Desviación (pesos)")

