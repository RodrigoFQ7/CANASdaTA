# CANASdaTA

Análisis y pronóstico de precios de la canasta básica en la Zona Metropolitana de Monterrey con series de tiempo en R.

## ¿Qué es?

CANASdaTA es un proyecto social de Líderes del Mañana del Tec de Monterrey. Analiza cómo cambian los precios de 10 productos de la canasta básica para ayudar a las familias a saber en qué meses conviene comprar y a detectar aumentos fuera de lo normal.

Los productos son aceite, arroz, azúcar, cebolla, frijol, huevo, leche, papa, tomate y tortilla. Los datos son precios promedio mensuales desde enero de 2011.

## Cómo correr el modelo

Abre `CANASdaTA.Rproj` en RStudio. Esto es importante porque así R trabaja desde la carpeta del repositorio y encuentra los datos.

La primera vez instala los paquetes:

```r
install.packages(c("tidyverse", "fable", "tsibble", "feasts", "urca"))
```

Después abre `analisis_canasdata.R` y córrelo completo. Tarda unos minutos porque entrena muchos modelos. Al terminar, todos los resultados quedan en la carpeta `resultados`. Ahí también queda `pagina_familias.html`, una página con los mismos resultados explicados en palabras sencillas para las familias. Se abre con doble clic.

## Qué hace el modelo

El script analiza los 10 productos con el mismo código. Para cada uno une sus archivos CSV, aplica los mismos filtros que usaban los scripts originales y calcula el precio promedio de cada mes.

Después mide qué tan bien pronostica. Para cada uno de los últimos 12 meses entrena el modelo solo con los datos anteriores y pronostica ese mes, como si todavía no lo conociera. Así compara ARIMA y ETS y se queda con el que tuvo menor error. También los compara con una referencia muy simple, que supone que el mes siguiente cuesta lo mismo que el actual.

Con el modelo elegido pronostica los próximos 12 meses y marca alertas en los meses en que el precio real salió del rango que el modelo esperaba y además cambió al menos 5% respecto al pronóstico. Por último separa la temporada de cada producto para armar un calendario con los meses en que suele estar más barato o más caro.

## Resultados

| Archivo | Contenido |
|---|---|
| `resultados/resumen_modelo.csv` | Modelo elegido y error de cada producto |
| `resultados/error_por_modelo.csv` | Error de ARIMA, ETS y la referencia simple |
| `resultados/alertas.csv` | Meses con aumentos o bajas fuera de lo esperado |
| `resultados/pronosticos.csv` | Pronóstico de los próximos 12 meses con su rango esperado |
| `resultados/calendario_compra.csv` | Qué tan caro o barato suele estar cada mes |
| `resultados/pruebas_ultimos_meses.csv` | Detalle de cada pronóstico de prueba |
| `resultados/series_mensuales.csv` | Precio promedio mensual de cada producto |
| `resultados/graficas/` | Gráfica de pronóstico y calendario de cada producto |
| `resultados/pagina_familias.html` | Página sencilla para familias, se abre en el navegador o el celular |

Con datos hasta mayo de 2026, el error promedio a un mes fue menor al 15% en los 10 productos. El más difícil de pronosticar es el tomate, con un error cercano al 11%, y le siguen la papa y la cebolla. En seis productos la referencia simple tuvo un error un poco menor que ARIMA y ETS, lo cual es común en precios que cambian poco de un mes a otro.

Cebolla, papa y tomate tienen una temporada clara y el arroz una muy ligera. En los demás productos la diferencia entre meses es muy pequeña y el calendario los marca como "Sin temporada marcada".

## Cómo actualizar los datos cada mes

Descarga el mes nuevo de la misma fuente y agrega sus filas al archivo CSV más reciente de cada producto, por ejemplo `tomate/tomate_2024_2026.CSV`. También puedes guardar un archivo CSV nuevo dentro de la carpeta del producto, porque el script lee todos los CSV que encuentre ahí. Luego vuelve a correr `analisis_canasdata.R`.

## Mejoras a futuro

Actualizar los datos cada semana en lugar de cada mes, usando una fuente con precios semanales. Descargar los datos de forma automática. Comparar precios entre tiendas cercanas, que fue lo que más pidieron las familias en la encuesta. Mostrar los resultados en una página web pensada para el celular.

## Estructura

```
CANASdaTA/
├── CANASdaTA.Rproj
├── analisis_canasdata.R      script principal del modelo
├── R/
│   ├── productos.R           productos y filtros de cada uno
│   └── funciones.R           funciones del modelo
├── resultados/               tablas y gráficas que genera el script
└── tomate/                   una carpeta igual para cada producto
    ├── tomate.R              script original del análisis
    ├── tomate_2011_2018.CSV
    ├── tomate_2018_2024.CSV
    └── tomate_2024_2026.CSV
```

Los scripts originales de cada carpeta siguen funcionando y sirven para explorar un producto por separado.

## Fuente de datos

Pendiente de documentar el enlace exacto de descarga.

## Equipo

Rodrigo Fuentes Quintana y Fernanda Meléndez Banda.
