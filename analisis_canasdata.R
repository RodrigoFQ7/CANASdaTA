# CANASdaTA: modelo predictivo de precios de la canasta básica
# -------------------------------------------------------------
# Corre el análisis completo de los 10 productos con un solo script.
# Antes de correrlo, abre CANASdaTA.Rproj en RStudio para que R trabaje
# desde la carpeta del repositorio.
#
# Qué hace con cada producto:
#   1. Une sus CSV y calcula el precio promedio de cada mes.
#   2. Mide el error de ARIMA y ETS con los últimos 12 meses y elige el mejor.
#   3. Marca alertas en los meses en que el precio salió de lo esperado.
#   4. Pronostica los próximos 12 meses.
#   5. Arma un calendario con los meses en que suele convenir comprar.
#
# Los resultados se guardan en la carpeta "resultados".
# Tarda unos minutos porque entrena muchos modelos.

source("R/productos.R", encoding = "UTF-8")
source("R/funciones.R", encoding = "UTF-8")

meses_prueba <- 12      # meses que se usan para medir el error
meses_pronostico <- 12  # meses que se pronostican hacia adelante
meta_error <- 15        # meta del proyecto: error menor a 15%
cambio_minimo <- 5      # diferencia mínima (%) para marcar una alerta

dir.create("resultados/graficas", recursive = TRUE, showWarnings = FALSE)

analizar_producto <- function(producto, nombre, carpeta, unidad, incluir, excluir) {
  message("Analizando ", nombre, "...")

  datos <- leer_producto(carpeta, unidad, incluir, excluir)
  serie <- serie_mensual(datos)

  pruebas <- evaluar_modelos(serie, meses_prueba)
  errores <- resumir_errores(pruebas)
  modelo <- elegir_modelo(errores)
  alertas <- detectar_alertas(pruebas, modelo, cambio_minimo)

  pronostico <- pronosticar(serie, modelo, meses_pronostico)
  calendario <- calendario_compra(serie)

  ggsave(file.path("resultados/graficas", paste0(producto, "_pronostico.png")),
         grafica_pronostico(pronostico, serie, nombre), width = 8, height = 4.5, dpi = 120, bg = "white")
  ggsave(file.path("resultados/graficas", paste0(producto, "_calendario.png")),
         grafica_calendario(calendario, nombre), width = 8, height = 4.5, dpi = 120, bg = "white")

  agregar_producto <- function(tabla) tibble(producto = nombre, tabla)
  list(
    serie = agregar_producto(as_tibble(serie)),
    pruebas = agregar_producto(pruebas),
    errores = agregar_producto(errores) %>% mutate(elegido = modelo == .env$modelo),
    alertas = agregar_producto(alertas),
    pronostico = agregar_producto(tabla_pronostico(pronostico) %>% mutate(modelo = .env$modelo)),
    calendario = agregar_producto(calendario)
  )
}

resultados <- pmap(productos, analizar_producto)
juntar <- function(parte) map_dfr(resultados, parte)
fecha_texto <- function(fecha) format(as.Date(fecha), "%Y-%m")

# Tablas de resultados ----------------------------------------------------------
errores <- juntar("errores")

resumen <- errores %>%
  group_by(producto) %>%
  summarise(
    modelo_elegido = modelo[elegido],
    error_modelo_pct = error_promedio_pct[elegido],
    error_ingenuo_pct = error_promedio_pct[modelo == "ingenuo"],
    .groups = "drop"
  ) %>%
  mutate(
    cumple_meta = error_modelo_pct < meta_error,
    mejor_que_ingenuo = error_modelo_pct < error_ingenuo_pct
  ) %>%
  left_join(
    juntar("serie") %>%
      group_by(producto) %>%
      summarise(ultimo_mes = fecha_texto(max(fecha)),
                ultimo_precio = precio[which.max(fecha)],
                meses_interpolados = sum(interpolado), .groups = "drop"),
    by = "producto"
  )

write_excel_csv(resumen %>% mutate(across(where(is.numeric), ~ round(.x, 2))),
                "resultados/resumen_modelo.csv")
write_excel_csv(errores %>% mutate(across(where(is.numeric), ~ round(.x, 2))),
                "resultados/error_por_modelo.csv")
write_excel_csv(juntar("pruebas") %>% mutate(fecha = fecha_texto(fecha), across(where(is.numeric), ~ round(.x, 2))),
                "resultados/pruebas_ultimos_meses.csv")
write_excel_csv(juntar("alertas") %>% mutate(fecha = fecha_texto(fecha), across(where(is.numeric), ~ round(.x, 2))) %>%
                  select(producto, fecha, tipo, precio_real, pronostico, inferior_80, superior_80, diferencia_pct),
                "resultados/alertas.csv")
write_excel_csv(juntar("pronostico") %>% mutate(fecha = fecha_texto(fecha), across(where(is.numeric), ~ round(.x, 2))),
                "resultados/pronosticos.csv")
write_excel_csv(juntar("calendario") %>% mutate(efecto_pct = round(efecto_pct, 1)) %>%
                  select(producto, mes, mes_nombre, efecto_pct, clasificacion),
                "resultados/calendario_compra.csv")
write_excel_csv(juntar("serie") %>% mutate(fecha = fecha_texto(fecha), across(where(is.numeric), ~ round(.x, 2))),
                "resultados/series_mensuales.csv")

message("\nListo. Resumen del modelo:")
print(resumen, n = Inf)
