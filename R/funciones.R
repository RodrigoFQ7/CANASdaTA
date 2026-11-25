# Funciones del modelo predictivo de CANASdaTA.
# Sirven para cualquier producto de R/productos.R, así no hay que repetir el código 10 veces.

suppressPackageStartupMessages({
  library(tidyverse)
  library(tsibble)
  library(fable)
  library(feasts)
})

meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
              "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# 1. Lectura ----------------------------------------------------------------
# Une todos los CSV de la carpeta del producto y aplica sus filtros.
# Cualquier CSV nuevo que se agregue a la carpeta se lee automáticamente.
leer_producto <- function(carpeta, unidad, incluir = NA, excluir = NA) {
  archivos <- list.files(carpeta, pattern = "\\.(csv|CSV)$", full.names = TRUE)
  if (length(archivos) == 0) {
    stop("No encontré archivos CSV en la carpeta '", carpeta, "'. ",
         "Abre CANASdaTA.Rproj para que R trabaje desde la carpeta del repositorio.")
  }

  datos <- map_dfr(archivos, function(archivo) {
    # Algunos CSV vienen en latin1 y otros en UTF-8. Se detecta en cada archivo
    # para que las letras con acento o "Ñ" no corten la lectura.
    lineas <- readLines(archivo, warn = FALSE)
    codificacion <- if (all(validUTF8(lineas))) "UTF-8" else "latin1"
    d <- read.csv(archivo, encoding = codificacion, check.names = FALSE)
    # Algunos archivos escriben "Especificación" con acento y otros sin él.
    names(d)[startsWith(names(d), "Especificaci")] <- "Especificacion"
    names(d)[startsWith(names(d), "Precio")] <- "Precio"
    if (!"Especificacion" %in% names(d)) d$Especificacion <- NA_character_
    tibble(
      anio = as.integer(d$Anio),
      mes = as.integer(d$Mes),
      precio = as.numeric(d$Precio),
      unidad = toupper(trimws(d$Unidad)),
      especificacion = toupper(trimws(d$Especificacion))
    )
  })

  datos <- filter(datos, .data$unidad == .env$unidad)
  if (!is.na(incluir)) datos <- filter(datos, str_detect(especificacion, incluir))
  if (!is.na(excluir)) datos <- filter(datos, !str_detect(especificacion, excluir))
  datos
}

# 2. Serie mensual ----------------------------------------------------------
# Precio promedio de cada mes. Si faltara algún mes, se estima con los meses
# de al lado y queda marcado en la columna "interpolado".
serie_mensual <- function(datos) {
  datos %>%
    group_by(anio, mes) %>%
    summarise(desviacion = sd(precio, na.rm = TRUE),
              registros = n(),
              precio = mean(precio, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(fecha = yearmonth(make_date(anio, mes, 1))) %>%
    select(fecha, precio, desviacion, registros) %>%
    as_tsibble(index = fecha) %>%
    fill_gaps() %>%
    mutate(interpolado = is.na(precio),
           precio = approx(seq_along(precio), precio, xout = seq_along(precio), rule = 2)$y)
}

# 3. Evaluación del modelo ----------------------------------------------------
# Simula cómo le habría ido al modelo en los últimos meses: para cada mes,
# entrena solo con los datos anteriores y pronostica ese mes. Así se mide el
# error con precios que el modelo no conocía.
# "ingenuo" es la referencia más simple: el mes siguiente cuesta lo mismo que este.
evaluar_modelos <- function(serie, meses_prueba = 12) {
  n <- nrow(serie)
  ventanas <- serie %>%
    select(fecha, precio) %>%
    stretch_tsibble(.init = n - meses_prueba, .step = 1) %>%
    filter(.id <= meses_prueba)

  ventanas %>%
    model(arima = ARIMA(precio), ets = ETS(precio), ingenuo = NAIVE(precio)) %>%
    forecast(h = 1) %>%
    hilo(80) %>%
    unpack_hilo(`80%`) %>%
    as_tibble() %>%
    select(modelo = .model, fecha, pronostico = .mean,
           inferior_80 = `80%_lower`, superior_80 = `80%_upper`) %>%
    left_join(as_tibble(serie) %>% select(fecha, precio_real = precio), by = "fecha")
}

resumir_errores <- function(pruebas) {
  pruebas %>%
    group_by(modelo) %>%
    summarise(error_promedio_pct = mean(abs(precio_real - pronostico) / precio_real, na.rm = TRUE) * 100,
              error_promedio_pesos = mean(abs(precio_real - pronostico), na.rm = TRUE),
              meses_evaluados = sum(!is.na(pronostico)),
              .groups = "drop")
}

# Entre ARIMA y ETS se queda el que tuvo menor error.
elegir_modelo <- function(errores) {
  errores %>%
    filter(modelo %in% c("arima", "ets")) %>%
    slice_min(error_promedio_pct, n = 1, with_ties = FALSE) %>%
    pull(modelo)
}

# 4. Alertas ------------------------------------------------------------------
# Un mes tiene alerta cuando se cumplen dos cosas:
#   el precio real quedó fuera del rango en el que el modelo esperaba
#   encontrarlo 8 de cada 10 veces (intervalo del 80%), y
#   la diferencia con el pronóstico es de al menos "cambio_minimo" por ciento,
#   para no avisar por cambios de centavos que no le importan a una familia.
detectar_alertas <- function(pruebas, modelo_elegido, cambio_minimo = 5) {
  pruebas %>%
    filter(modelo == modelo_elegido) %>%
    mutate(diferencia_pct = (precio_real - pronostico) / pronostico * 100,
           tipo = case_when(
             precio_real > superior_80 & diferencia_pct >= cambio_minimo ~ "Aumento fuera de lo esperado",
             precio_real < inferior_80 & diferencia_pct <= -cambio_minimo ~ "Baja fuera de lo esperado",
             TRUE ~ NA_character_
           )) %>%
    filter(!is.na(tipo))
}

# 5. Pronóstico ---------------------------------------------------------------
pronosticar <- function(serie, modelo_elegido, meses = 12) {
  definicion <- switch(modelo_elegido, arima = ARIMA(precio), ets = ETS(precio))
  serie %>%
    select(fecha, precio) %>%
    model(elegido = definicion) %>%
    forecast(h = meses)
}

tabla_pronostico <- function(pronostico) {
  pronostico %>%
    hilo(80) %>%
    unpack_hilo(`80%`) %>%
    as_tibble() %>%
    select(fecha, pronostico = .mean, inferior_80 = `80%_lower`, superior_80 = `80%_upper`) %>%
    # Un precio no puede ser negativo. En productos muy variables, como el tomate,
    # el rango a varios meses es tan amplio que su límite inferior bajaría de cero.
    mutate(inferior_80 = pmax(inferior_80, 0))
}

# 6. Calendario de compra -------------------------------------------------------
# Separa la temporada del precio (STL) y calcula, para cada mes, qué tan caro o
# barato suele estar comparado con su nivel normal. Usa los últimos años para
# reflejar el comportamiento reciente.
calendario_compra <- function(serie, anios_recientes = 5, diferencia_minima = 5) {
  componentes <- serie %>%
    model(STL(log(precio) ~ season(period = 12))) %>%
    components() %>%
    as_tibble()

  ultima_fecha <- max(componentes$fecha)
  calendario <- componentes %>%
    filter(fecha > ultima_fecha - 12 * anios_recientes) %>%
    mutate(mes = month(fecha)) %>%
    group_by(mes) %>%
    summarise(efecto_pct = (exp(mean(season_12)) - 1) * 100, .groups = "drop") %>%
    mutate(mes_nombre = factor(meses_es[mes], levels = meses_es))

  # Si la diferencia entre el mes más caro y el más barato es pequeña,
  # el producto no tiene una temporada clara y no se recomienda ningún mes.
  rango <- max(calendario$efecto_pct) - min(calendario$efecto_pct)
  calendario %>%
    mutate(clasificacion = case_when(
      rango < diferencia_minima ~ "Sin temporada marcada",
      rank(efecto_pct, ties.method = "first") <= 3 ~ "Buen mes para comprar",
      rank(-efecto_pct, ties.method = "first") <= 3 ~ "Mes más caro",
      TRUE ~ "Normal"
    ))
}

# 7. Gráficas -------------------------------------------------------------------
grafica_pronostico <- function(pronostico, serie, nombre, desde = 2020) {
  historia <- as_tibble(serie) %>%
    mutate(fecha = as.Date(fecha)) %>%
    filter(year(fecha) >= desde)
  futuro <- tabla_pronostico(pronostico) %>%
    mutate(fecha = as.Date(fecha))

  ggplot() +
    geom_ribbon(data = futuro, aes(x = fecha, ymin = inferior_80, ymax = superior_80),
                fill = "#90CAF9", alpha = 0.6) +
    geom_line(data = historia, aes(x = fecha, y = precio), color = "grey20") +
    geom_line(data = futuro, aes(x = fecha, y = pronostico), color = "#1565C0", linewidth = 1) +
    labs(title = paste("Pronóstico del precio:", nombre),
         subtitle = "Línea gris: precio real. Línea azul: pronóstico. Zona azul: rango esperado (80%)",
         x = NULL, y = "Precio promedio (pesos)") +
    theme_minimal()
}

grafica_calendario <- function(calendario, nombre) {
  colores <- c("Buen mes para comprar" = "#2E7D32", "Normal" = "#9E9E9E",
               "Mes más caro" = "#C62828", "Sin temporada marcada" = "#9E9E9E")
  ggplot(calendario, aes(x = mes_nombre, y = efecto_pct, fill = clasificacion)) +
    geom_col() +
    geom_hline(yintercept = 0, color = "grey40") +
    scale_fill_manual(values = colores) +
    labs(title = paste("¿Cuándo conviene comprar?", nombre),
         subtitle = "Qué tan caro o barato suele estar cada mes respecto a su nivel normal",
         x = NULL, y = "Diferencia (%)", fill = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
}
