# --- 1. CARGA DE LIBRERÍAS ---
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)

# --- 2. IMPORTACIÓN Y LIMPIEZA ---
# Asegúrate de que la ruta apunte a tu carpeta de azúcar
archivos_azucar <- list.files("~/ProyectoLDM/azucar/", pattern = "\\.(csv|CSV)$", full.names = TRUE, recursive = TRUE)
message("Archivos de precio de azúcar encontrados: ", length(archivos_azucar))

datos_azucar <- archivos_azucar %>%
  map_dfr(~read.csv(.x, fileEncoding = "latin1") %>% 
            # Filtramos por Unidad KG y que contenga específicamente ESTANDAR
            filter(toupper(trimws(Unidad)) == "KG",
                   str_detect(toupper(trimws(Especificacion)), "ESTANDAR")))

resumen_azucar <- datos_azucar %>%
  group_by(Anio, Mes) %>%
  summarise(precio_promedio_azucar = mean(Precio.promedio, na.rm = TRUE),
            desviacion_azucar = sd(Precio.promedio, na.rm = TRUE), .groups = "drop")

# --- 3. VISUALIZACIÓN ---
resumen_azucar %>%
  mutate(fecha_azucar = as.Date(paste(Anio, Mes, "01", sep = "-"))) %>%
  ggplot(aes(x = fecha_azucar, y = precio_promedio_azucar)) +
  geom_line(color = "skyblue4") + geom_point() +
  geom_errorbar(aes(ymin = precio_promedio_azucar - desviacion_azucar, 
                    ymax = precio_promedio_azucar + desviacion_azucar), width = 0.2, color = "gray") +
  theme_minimal() + labs(title = "Evolución del Precio del Azúcar (Estandar)", y = "Precio (KG)")

# --- 4. MODELADO Y PRONÓSTICO ---
ts_serie_azucar <- resumen_azucar %>%
  mutate(fecha_azucar = yearmonth(paste(Anio, Mes, sep = "-"))) %>%
  as_tsibble(index = fecha_azucar)

modelo_azucar <- ts_serie_azucar %>% model(arima = ARIMA(precio_promedio_azucar), ets = ETS(precio_promedio_azucar))
pronostico_azucar <- modelo_azucar %>% forecast(h = 12)

# Gráfico de pronóstico
pronostico_azucar %>% autoplot(ts_serie_azucar) + theme_minimal() + labs(title = "Pronóstico Precio Azúcar Estandar")

# --- 5. ANÁLISIS ESTACIONAL ---
modelo_descomp_azucar <- ts_serie_azucar %>% model(STL(precio_promedio_azucar ~ season(period = 12)))
components(modelo_descomp_azucar) %>% autoplot()

estacionalidad_mes_azucar <- components(modelo_descomp_azucar) %>%
  as_tsibble() %>%
  mutate(mes_etiqueta = month(fecha_azucar, label = TRUE)) %>%
  group_by(mes_etiqueta) %>%
  summarise(efecto_estacional_azucar = mean(season_12))

ggplot(estacionalidad_mes_azucar, aes(x = mes_etiqueta, y = efecto_estacional_azucar, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) + geom_point(size = 3) +
  theme_minimal() + labs(title = "Ciclo Estacional del Precio del Azúcar Estandar", y = "Desviación (pesos)")
