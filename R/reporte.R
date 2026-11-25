# Página para familias de CANASdaTA.
# Toma los resultados del modelo y arma una página web sencilla, pensada para
# verse en el celular: una tarjeta por producto con su precio, si está caro o
# barato frente a lo esperado, el pronóstico del mes siguiente y un semáforo
# con los meses en que suele convenir comprar.
# La página es un solo archivo HTML que se abre con cualquier navegador.

meses_corto <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
iniciales_mes <- c("E", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

# Orden de las tarjetas: primero los productos que más preocupan según la
# encuesta, luego los que tienen temporada marcada y al final los demás.
orden_tarjetas <- c("Huevo", "Leche", "Tortilla", "Tomate", "Cebolla", "Papa",
                    "Arroz", "Frijol", "Azúcar", "Aceite")

pesos <- function(x) paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
porcentaje <- function(x, digitos = 0) paste0(formatC(abs(x), format = "f", digits = digitos), "%")
numero_mes <- function(fecha) as.integer(format(as.Date(fecha), "%m"))
nombre_mes <- function(fecha) tolower(meses_es[numero_mes(fecha)])
mes_y_anio <- function(fecha) paste(nombre_mes(fecha), "de", format(as.Date(fecha), "%Y"))
mes_corto_anio <- function(fecha) paste(meses_corto[numero_mes(fecha)], format(as.Date(fecha), "%Y"))
unir_palabras <- function(x) {
  n <- length(x)
  if (n <= 1) return(x)
  paste(paste(x[-n], collapse = ", "), "y", x[n])
}

# Gráfica pequeña con el precio real de los últimos meses y el pronóstico.
grafica_mini <- function(real, futuro, nombre) {
  valores <- c(real, futuro)
  ancho <- 320
  alto <- 72
  margen <- 6
  rango <- range(valores)
  if (diff(rango) == 0) rango <- rango + c(-1, 1)
  x <- function(i) margen + (i - 1) * (ancho - 2 * margen) / (length(valores) - 1)
  y <- function(v) alto - margen - (v - rango[1]) / diff(rango) * (alto - 2 * margen)
  n <- length(real)
  puntos_real <- paste(sprintf("%.1f,%.1f", x(seq_len(n)), y(real)), collapse = " ")
  puntos_futuro <- paste(sprintf("%.1f,%.1f", x(n:(n + length(futuro))), y(c(real[n], futuro))), collapse = " ")
  sprintf(paste0(
    '<svg class="mini" viewBox="0 0 %d %d" role="img" aria-label="Precio de %s en los últimos dos años y pronóstico de los meses siguientes">',
    '<line class="mini-hoy" x1="%.1f" x2="%.1f" y1="0" y2="%d"/>',
    '<polyline class="mini-real" points="%s"/>',
    '<polyline class="mini-futuro" points="%s"/>',
    '<circle class="mini-punto" cx="%.1f" cy="%.1f" r="4"/>',
    '</svg>'),
    ancho, alto, nombre, x(n), x(n), alto, puntos_real, puntos_futuro, x(n), y(real[n]))
}

tarjeta_producto <- function(nombre, unidad, serie, pronostico, calendario, alertas, error_pct) {
  serie <- arrange(serie, fecha)
  pronostico <- arrange(pronostico, fecha)
  ultimo <- tail(serie, 1)
  siguiente <- head(pronostico, 1)
  real <- tail(serie$precio, 24)
  futuro <- head(pronostico$pronostico, 6)
  inicio_grafica <- tail(serie$fecha, 24)[1]
  fin_grafica <- pronostico$fecha[length(futuro)]
  unidad_texto <- if (unidad == "LT") "litro" else "kilo"

  # Estado del último mes con datos, según las alertas del modelo.
  alerta <- alertas %>% filter(fecha == ultimo$fecha)
  if (nrow(alerta) > 0 && alerta$tipo[1] == "Aumento fuera de lo esperado") {
    estado <- c("caro", "Más caro de lo esperado")
  } else if (nrow(alerta) > 0) {
    estado <- c("barato", "Más barato de lo esperado")
  } else {
    estado <- c("normal", "Dentro de lo esperado")
  }

  # Pronóstico del mes siguiente comparado con el último precio.
  cambio <- (siguiente$pronostico - ultimo$precio) / ultimo$precio * 100
  comparacion <- if (abs(cambio) < 2) {
    paste("casi igual que en", nombre_mes(ultimo$fecha))
  } else if (cambio > 0) {
    paste("un", porcentaje(cambio), "más que en", nombre_mes(ultimo$fecha))
  } else {
    paste("un", porcentaje(cambio), "menos que en", nombre_mes(ultimo$fecha))
  }
  texto_pronostico <- sprintf("Para %s se espera alrededor de <strong>%s</strong>, %s.",
                              mes_y_anio(siguiente$fecha), pesos(siguiente$pronostico), comparacion)

  # Semáforo de meses.
  calendario <- arrange(calendario, mes)
  sin_temporada <- all(calendario$clasificacion == "Sin temporada marcada")
  clase_celda <- case_when(
    calendario$clasificacion == "Buen mes para comprar" ~ "barato",
    calendario$clasificacion == "Mes más caro" ~ "caro",
    TRUE ~ "normal"
  )
  celdas <- paste(sprintf('<span class="mes %s" title="%s">%s</span>',
                          clase_celda, meses_es[calendario$mes], iniciales_mes[calendario$mes]),
                  collapse = "")
  if (sin_temporada) {
    texto_temporada <- "Su precio casi no cambia según el mes del año, así que no hay una temporada mejor para comprarlo."
  } else {
    baratos <- tolower(meses_es[calendario$mes[clase_celda == "barato"]])
    caros <- tolower(meses_es[calendario$mes[clase_celda == "caro"]])
    texto_temporada <- sprintf("Suele estar más barato en %s, y más caro en %s.",
                               unir_palabras(baratos), unir_palabras(caros))
  }

  paste0(
    '<article class="tarjeta">',
    '<header class="tarjeta-cabeza"><h3>', nombre, '</h3>',
    '<span class="estado ', estado[1], '">', estado[2], '</span></header>',
    '<p class="precio"><span class="cifra">', pesos(ultimo$precio), '</span>',
    '<span class="unidad">por ', unidad_texto, ' en ', mes_y_anio(ultimo$fecha), '</span></p>',
    '<figure class="grafica">', grafica_mini(real, futuro, nombre),
    '<figcaption><span>', mes_corto_anio(inicio_grafica), '</span>',
    '<span class="leyenda-mini"><i class="linea-real"></i>real <i class="linea-futuro"></i>pronóstico</span>',
    '<span>', mes_corto_anio(fin_grafica), '</span></figcaption></figure>',
    '<p class="pronostico">', texto_pronostico, '</p>',
    '<div class="semaforo" aria-label="Meses del año según su precio">', celdas, '</div>',
    '<p class="temporada">', texto_temporada, '</p>',
    '<p class="confianza">En el último año, el pronóstico de un mes falló en promedio un ',
    porcentaje(error_pct, 1), '.</p>',
    '</article>')
}

fila_alerta <- function(producto, fecha, precio_real, pronostico, diferencia_pct) {
  paste0(
    '<li class="alerta">',
    '<span class="alerta-cambio">', porcentaje(diferencia_pct), ' más</span>',
    '<div class="alerta-texto"><strong>', producto, ', ', mes_y_anio(fecha), '</strong>',
    '<span>Costó ', pesos(precio_real), ' cuando se esperaban unos ', pesos(pronostico), '.</span></div>',
    '</li>')
}

generar_pagina_familias <- function(resumen, series, pronosticos, calendario, alertas,
                                    productos, archivo) {
  nombres <- intersect(orden_tarjetas, resumen$producto)
  nombres <- c(nombres, setdiff(resumen$producto, nombres))
  ultima_fecha <- max(series$fecha)
  primera_fecha <- min(series$fecha)

  tarjetas <- map_chr(nombres, function(nombre) {
    tarjeta_producto(
      nombre = nombre,
      unidad = productos$unidad[productos$nombre == nombre],
      serie = filter(series, producto == nombre),
      pronostico = filter(pronosticos, producto == nombre),
      calendario = filter(calendario, producto == nombre),
      alertas = filter(alertas, producto == nombre),
      error_pct = resumen$error_modelo_pct[resumen$producto == nombre]
    )
  })

  aumentos <- alertas %>%
    filter(tipo == "Aumento fuera de lo esperado") %>%
    arrange(desc(diferencia_pct))
  lista_aumentos <- if (nrow(aumentos) == 0) {
    '<p>No hubo aumentos fuera de lo esperado en el último año.</p>'
  } else {
    paste0('<ul class="alertas">',
           paste(pmap_chr(select(aumentos, producto, fecha, precio_real, pronostico, diferencia_pct),
                          fila_alerta), collapse = ""),
           '</ul>')
  }

  html <- paste0(
    '<!doctype html>\n<html lang="es">\n<head>\n',
    '<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">\n',
    '<title>CANASdaTA para familias</title>\n',
    '<meta name="claude:viewport-fit-sides" content="cover">\n',
    '<link rel="preconnect" href="https://fonts.googleapis.com">\n',
    '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>\n',
    '<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible:wght@400;700&family=Bricolage+Grotesque:opsz,wght@12..96,600;12..96,800&display=swap">\n',
    '<style>\n', estilos_pagina, '\n</style>\n',
    '</head>\n<body>\n<main class="pagina">\n',
    '<header class="portada">',
    '<p class="sello">CANASdaTA</p>',
    '<h1>¿Cuándo conviene comprar?</h1>',
    '<p class="entrada">Precios de 10 productos de la canasta básica en la Zona Metropolitana de Monterrey. ',
    'Para cada uno te decimos si está caro o barato, qué precio esperamos el mes siguiente y en qué meses suele salir más barato.</p>',
    '<p class="aviso">Datos hasta ', mes_y_anio(ultima_fecha), '.</p>',
    '<div class="como-leer"><span class="como-leer-titulo">Cómo leer el semáforo</span>',
    '<span class="clave"><i class="mes barato"></i>Mes barato</span>',
    '<span class="clave"><i class="mes normal"></i>Mes normal</span>',
    '<span class="clave"><i class="mes caro"></i>Mes caro</span></div>',
    '</header>\n',
    '<section class="productos" aria-label="Productos">', paste(tarjetas, collapse = "\n"), '</section>\n',
    '<section class="bloque"><h2>Aumentos fuera de lo esperado</h2>',
    '<p>Estos son los meses del último año en que un producto costó bastante más de lo que el modelo esperaba.</p>',
    lista_aumentos, '</section>\n',
    '<section class="bloque nota"><h2>¿De dónde salen estos datos?</h2>',
    '<p>Usamos los precios promedio de cada mes desde ', mes_y_anio(primera_fecha), '. ',
    'Para saber si el modelo funciona, le pedimos que pronosticara cada mes del último año sin conocerlo ',
    'y comparamos su respuesta con el precio real. En los 10 productos el error promedio fue menor al 15%.</p>',
    '<p>CANASdaTA es un proyecto social de Líderes del Mañana del Tec de Monterrey.</p></section>\n',
    '</main>\n</body>\n</html>\n')

  writeLines(enc2utf8(html), archivo, useBytes = TRUE)
  invisible(archivo)
}

estilos_pagina <- '
/* Diseño: columna única en celular, rejilla de tarjetas tipo etiqueta de precio en pantallas anchas. */
:root {
  --fondo: #f3f5f1;
  --superficie: #ffffff;
  --tinta: #1c2420;
  --tenue: #5a6861;
  --linea: #d9dfd9;
  --acento: #0e6b57;
  --barato: #2f7d3a;
  --barato-suave: #e2f1e3;
  --caro: #c0392b;
  --caro-suave: #fbe4e1;
  --neutro: #dde3de;
  --neutro-texto: #48544e;
  --letra-titulo: "Bricolage Grotesque", "Segoe UI", system-ui, sans-serif;
  --letra-texto: "Atkinson Hyperlegible", "Segoe UI", system-ui, sans-serif;
}
@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) {
    --fondo: #111614; --superficie: #1a211e; --tinta: #e7ede9; --tenue: #9fada6; --linea: #2c3531;
    --acento: #4cc3a5; --barato: #6cc77a; --barato-suave: #1f3423; --caro: #f07a6c; --caro-suave: #3d2220;
    --neutro: #36433d; --neutro-texto: #c3cec8; color-scheme: dark;
  }
}
:root[data-theme="dark"] {
  --fondo: #111614; --superficie: #1a211e; --tinta: #e7ede9; --tenue: #9fada6; --linea: #2c3531;
  --acento: #4cc3a5; --barato: #6cc77a; --barato-suave: #1f3423; --caro: #f07a6c; --caro-suave: #3d2220;
  --neutro: #36433d; --neutro-texto: #c3cec8; color-scheme: dark;
}
* { box-sizing: border-box; }
body { margin: 0; background: var(--fondo); color: var(--tinta); font-family: var(--letra-texto); font-size: 16px; line-height: 1.5; }
.pagina { max-width: 1060px; margin: 0 auto; padding-block: 28px 48px; padding-left: max(16px, env(safe-area-inset-left, 0px)); padding-right: max(16px, env(safe-area-inset-right, 0px)); display: grid; gap: 32px; }
h1, h2, h3 { font-family: var(--letra-titulo); text-wrap: balance; margin: 0; }
h1 { font-size: clamp(2rem, 6vw, 3.2rem); font-weight: 800; line-height: 1.05; letter-spacing: -0.01em; }
h2 { font-size: 1.4rem; font-weight: 800; }
h3 { font-size: 1.35rem; font-weight: 800; }
p { margin: 0; }
.portada { display: grid; gap: 12px; max-width: 46rem; }
.sello { font-family: var(--letra-titulo); font-weight: 800; color: var(--acento); letter-spacing: 0.02em; font-size: 1.05rem; }
.entrada { font-size: 1.1rem; color: var(--tenue); max-width: 62ch; }
.aviso { font-weight: 700; }
.como-leer { display: flex; flex-wrap: wrap; align-items: center; gap: 8px 16px; padding-top: 4px; }
.como-leer-titulo { font-size: 0.8rem; text-transform: uppercase; letter-spacing: 0.06em; color: var(--tenue); font-weight: 700; }
.clave { display: inline-flex; align-items: center; gap: 6px; font-size: 0.95rem; }
.clave .mes { width: 18px; height: 18px; display: inline-block; }
.productos { display: grid; grid-template-columns: 1fr; gap: 16px; }
@media (min-width: 760px) { .productos { grid-template-columns: 1fr 1fr; } }
.tarjeta { background: var(--superficie); border: 1px solid var(--linea); border-radius: 14px; padding: 18px; display: grid; gap: 12px; min-width: 0; align-content: start; }
.tarjeta-cabeza { display: flex; flex-wrap: wrap; align-items: center; justify-content: space-between; gap: 8px; }
.estado { font-size: 0.85rem; font-weight: 700; padding: 4px 10px; border-radius: 999px; background: var(--neutro); color: var(--neutro-texto); }
.estado.caro { background: var(--caro-suave); color: var(--caro); }
.estado.barato { background: var(--barato-suave); color: var(--barato); }
.precio { display: flex; flex-wrap: wrap; align-items: baseline; gap: 4px 10px; }
.cifra { font-family: var(--letra-titulo); font-weight: 800; font-size: 2.4rem; line-height: 1; font-variant-numeric: tabular-nums; }
.unidad { color: var(--tenue); }
.grafica { margin: 0; display: grid; gap: 4px; }
.mini { width: 100%; height: auto; display: block; overflow: visible; }
.mini-real { fill: none; stroke: var(--tinta); stroke-width: 2; stroke-linejoin: round; }
.mini-futuro { fill: none; stroke: var(--acento); stroke-width: 2.5; stroke-dasharray: 5 4; stroke-linejoin: round; }
.mini-punto { fill: var(--acento); stroke: var(--superficie); stroke-width: 2; }
.mini-hoy { stroke: var(--linea); stroke-width: 1.5; }
.grafica figcaption { display: flex; justify-content: space-between; gap: 8px; font-size: 0.8rem; color: var(--tenue); }
.leyenda-mini { display: inline-flex; align-items: center; gap: 6px; }
.leyenda-mini i { display: inline-block; width: 16px; height: 0; border-top: 2px solid var(--tinta); }
.leyenda-mini i.linea-futuro { border-top: 2px dashed var(--acento); margin-left: 6px; }
.semaforo { display: grid; grid-template-columns: repeat(12, 1fr); gap: 3px; }
.mes { display: grid; place-items: center; height: 30px; border-radius: 5px; font-size: 0.8rem; font-weight: 700; background: var(--neutro); color: var(--neutro-texto); }
.mes.barato { background: var(--barato); color: var(--superficie); }
.mes.caro { background: var(--caro); color: var(--superficie); }
.temporada { font-size: 0.95rem; }
.confianza { font-size: 0.85rem; color: var(--tenue); border-top: 1px solid var(--linea); padding-top: 10px; }
.bloque { display: grid; gap: 12px; max-width: 46rem; }
.alertas { list-style: none; margin: 0; padding: 0; display: grid; gap: 8px; }
.alerta { display: flex; align-items: center; gap: 14px; background: var(--superficie); border: 1px solid var(--linea); border-radius: 12px; padding: 12px 14px; }
.alerta-cambio { flex: none; font-family: var(--letra-titulo); font-weight: 800; color: var(--caro); background: var(--caro-suave); border-radius: 8px; padding: 6px 10px; font-variant-numeric: tabular-nums; min-width: 5.5rem; text-align: center; }
.alerta-texto { display: grid; min-width: 0; }
.alerta-texto span { color: var(--tenue); font-size: 0.95rem; }
.nota p { color: var(--tenue); }
'
