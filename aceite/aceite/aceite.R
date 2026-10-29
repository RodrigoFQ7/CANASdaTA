# --- 1. CARGA DE LIBRERÍAS ---
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)

# --- 2. IMPORTACIÓN Y LIMPIEZA ---
# Asegúrate de que la ruta apunte a tu carpeta de aceite
archivos_aceite <- list.files("~/ProyectoLDM/aceite/", pattern = "\\.(csv|CSV)$", full.names = TRUE, recursive = TRUE)
message("Archivos de precio de aceite encontrados: ", length(archivos_aceite))

datos_aceite <- archivos_aceite %>%
  map_dfr(~read.csv(.x, fileEncoding = "latin1") %>% 
            # Filtramos solo por "LT" y excluimos los de lujo, el de 170gr y ahora el de 170ml
            filter(toupper(trimws(Unidad)) == "LT",
                   !str_detect(toupper(trimws(Especificacion)), 
                               "OLIVA|AGUACATE|ALMENDRA|COCO|ACEITE DE CANOLA, BOTE DE 170 GR|PAM, ACEITE, DE CANOLA, BOTELLA DE 170 ML")))

resumen_aceite <- datos_aceite %>%
  group_by(Anio, Mes) %>%
  summarise(precio_promedio_aceite = mean(Precio.promedio, na.rm = TRUE),
            desviacion_aceite = sd(Precio.promedio, na.rm = TRUE), .groups = "drop")

# --- 3. VISUALIZACIÓN ---
resumen_aceite %>%
  mutate(fecha_aceite = as.Date(paste(Anio, Mes, "01", sep = "-"))) %>%
  ggplot(aes(x = fecha_aceite, y = precio_promedio_aceite)) +
  geom_line(color = "gold3") + geom_point() +
  geom_errorbar(aes(ymin = precio_promedio_aceite - desviacion_aceite, 
                    ymax = precio_promedio_aceite + desviacion_aceite), width = 0.2, color = "gray") +
  theme_minimal() + labs(title = "Evolución del Precio del Aceite (Excluyendo Lujo)", y = "Precio (Litro)")

# --- 4. MODELADO Y PRONÓSTICO ---
ts_serie_aceite <- resumen_aceite %>%
  mutate(fecha_aceite = yearmonth(paste(Anio, Mes, sep = "-"))) %>%
  as_tsibble(index = fecha_aceite)

modelo_aceite <- ts_serie_aceite %>% model(arima = ARIMA(precio_promedio_aceite), ets = ETS(precio_promedio_aceite))
pronostico_aceite <- modelo_aceite %>% forecast(h = 12)

# Gráfico de pronóstico
pronostico_aceite %>% autoplot(ts_serie_aceite) + theme_minimal() + labs(title = "Pronóstico Precio Aceite (Consumo General)")

# --- 5. ANÁLISIS ESTACIONAL ---
modelo_descomp_aceite <- ts_serie_aceite %>% model(STL(precio_promedio_aceite ~ season(period = 12)))
components(modelo_descomp_aceite) %>% autoplot()

estacionalidad_mes_aceite <- components(modelo_descomp_aceite) %>%
  as_tsibble() %>%
  mutate(mes_etiqueta = month(fecha_aceite, label = TRUE)) %>%
  group_by(mes_etiqueta) %>%
  summarise(efecto_estacional_aceite = mean(season_12))

ggplot(estacionalidad_mes_aceite, aes(x = mes_etiqueta, y = efecto_estacional_aceite, group = 1)) +
  geom_line(color = "goldenrod3", size = 1.2) + geom_point(size = 3) +
  theme_minimal() + labs(title = "Ciclo Estacional del Precio del Aceite", y = "Desviación (pesos)")

