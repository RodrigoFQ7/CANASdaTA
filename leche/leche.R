# --- 1. CARGA DE LIBRERÍAS ---
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)

# --- 2. IMPORTACIÓN Y LIMPIEZA ---
# Cambiado a la carpeta de leche
archivos_leche <- list.files("~/ProyectoLDM/leche/", pattern = "\\.(csv|CSV)$", full.names = TRUE, recursive = TRUE)
message("Archivos de precio de leche encontrados: ", length(archivos_leche))

datos_leche <- archivos_leche %>%
  map_dfr(~read.csv(.x) %>% 
            # Filtramos por Unidad LITRO y excluimos si "especificacion" contiene "sabor"
            filter(toupper(trimws(Unidad)) == "LT",
                   !str_detect(toupper(trimws(Especificacion)), "SABOR")))

resumen_leche <- datos_leche %>%
  group_by(Anio, Mes) %>%
  summarise(precio_promedio_leche = mean(Precio.promedio, na.rm = TRUE),
            desviacion_leche = sd(Precio.promedio, na.rm = TRUE), .groups = "drop")

# --- 3. VISUALIZACIÓN ---
resumen_leche %>%
  mutate(fecha_leche = as.Date(paste(Anio, Mes, "01", sep = "-"))) %>%
  ggplot(aes(x = fecha_leche, y = precio_promedio_leche)) +
  geom_line(color = "blue") + geom_point() + # Cambiado color a azul
  geom_errorbar(aes(ymin = precio_promedio_leche - desviacion_leche, 
                    ymax = precio_promedio_leche + desviacion_leche), width = 0.2, color = "gray") +
  theme_minimal() + labs(title = "Evolución del Precio de la Leche", y = "Precio (LITRO)")

# --- 4. MODELADO Y PRONÓSTICO ---
ts_serie_leche <- resumen_leche %>%
  mutate(fecha_leche = yearmonth(paste(Anio, Mes, sep = "-"))) %>%
  as_tsibble(index = fecha_leche)

modelo_leche <- ts_serie_leche %>% model(arima = ARIMA(precio_promedio_leche), ets = ETS(precio_promedio_leche))
pronostico_leche <- modelo_leche %>% forecast(h = 12)

# Gráfico de pronóstico y tabla de datos
pronostico_leche %>% autoplot(ts_serie_leche) + theme_minimal()
datos_pronostico_leche <- pronostico_leche %>% hilo() %>% as_tibble()

# --- 5. ANÁLISIS ESTACIONAL ---
modelo_descomp_leche <- ts_serie_leche %>% model(STL(precio_promedio_leche ~ season(period = 12)))
components(modelo_descomp_leche) %>% autoplot()

estacionalidad_mes_leche <- components(modelo_descomp_leche) %>%
  as_tsibble() %>%
  mutate(mes_etiqueta = month(fecha_leche, label = TRUE)) %>%
  group_by(mes_etiqueta) %>%
  summarise(efecto_estacional_leche = mean(season_12))

ggplot(estacionalidad_mes_leche, aes(x = mes_etiqueta, y = efecto_estacional_leche, group = 1)) +
  geom_line(color = "darkblue", size = 1.2) + geom_point(size = 3) +
  theme_minimal() + labs(title = "Ciclo Estacional del Precio de la Leche", y = "Desviación (pesos)")
