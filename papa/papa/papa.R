# --- 1. CARGA DE LIBRERÍAS ---
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)

# --- 2. IMPORTACIÓN Y LIMPIEZA ---
# Asegúrate de que la ruta apunte a tu carpeta de papa
archivos_papa <- list.files("~/ProyectoLDM/papa/", pattern = "\\.(csv|CSV)$", full.names = TRUE, recursive = TRUE)
message("Archivos de precio de papa encontrados: ", length(archivos_papa))

datos_papa <- archivos_papa %>%
  map_dfr(~read.csv(.x, fileEncoding = "latin1") %>% 
            # Filtramos por Unidad KG y que contenga los 3 elementos (AND lógico)
            filter(toupper(trimws(Unidad)) == "KG",
                   str_detect(toupper(trimws(Especificacion)), 
                              "(?=.*PAPA)(?=.*BLANCA)(?=.*A GRANEL)")))

resumen_papa <- datos_papa %>%
  group_by(Anio, Mes) %>%
  summarise(precio_promedio_papa = mean(Precio.promedio, na.rm = TRUE),
            desviacion_papa = sd(Precio.promedio, na.rm = TRUE), .groups = "drop")

# --- 3. VISUALIZACIÓN ---
resumen_papa %>%
  mutate(fecha_papa = as.Date(paste(Anio, Mes, "01", sep = "-"))) %>%
  ggplot(aes(x = fecha_papa, y = precio_promedio_papa)) +
  geom_line(color = "brown") + geom_point() +
  geom_errorbar(aes(ymin = precio_promedio_papa - desviacion_papa, 
                    ymax = precio_promedio_papa + desviacion_papa), width = 0.2, color = "gray") +
  theme_minimal() + labs(title = "Evolución del Precio de la Papa (Blanca, A Granel)", y = "Precio (KG)")

# --- 4. MODELADO Y PRONÓSTICO ---
ts_serie_papa <- resumen_papa %>%
  mutate(fecha_papa = yearmonth(paste(Anio, Mes, sep = "-"))) %>%
  as_tsibble(index = fecha_papa)

modelo_papa <- ts_serie_papa %>% model(arima = ARIMA(precio_promedio_papa), ets = ETS(precio_promedio_papa))
pronostico_papa <- modelo_papa %>% forecast(h = 12)

# Gráfico de pronóstico
pronostico_papa %>% autoplot(ts_serie_papa) + theme_minimal() + labs(title = "Pronóstico Precio Papa Blanca a Granel")

# --- 5. ANÁLISIS ESTACIONAL ---
modelo_descomp_papa <- ts_serie_papa %>% model(STL(precio_promedio_papa ~ season(period = 12)))
components(modelo_descomp_papa) %>% autoplot()

estacionalidad_mes_papa <- components(modelo_descomp_papa) %>%
  as_tsibble() %>%
  mutate(mes_etiqueta = month(fecha_papa, label = TRUE)) %>%
  group_by(mes_etiqueta) %>%
  summarise(efecto_estacional_papa = mean(season_12))

ggplot(estacionalidad_mes_papa, aes(x = mes_etiqueta, y = efecto_estacional_papa, group = 1)) +
  geom_line(color = "brown4", size = 1.2) + geom_point(size = 3) +
  theme_minimal() + labs(title = "Ciclo Estacional del Precio de la Papa Blanca a Granel", y = "Desviación (pesos)")
