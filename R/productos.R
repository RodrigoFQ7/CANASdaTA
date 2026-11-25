# Productos que analiza CANASdaTA y filtros de cada uno.
# Los filtros son los mismos que usaban los scripts originales de cada carpeta.
#   unidad:  unidad de medida que se conserva (KG o LT)
#   incluir: la especificación debe contener este texto (NA = sin filtro)
#   excluir: se descartan las especificaciones que contengan este texto (NA = sin filtro)

productos <- tibble::tribble(
  ~producto,  ~nombre,    ~carpeta,    ~unidad, ~incluir,                                ~excluir,
  "aceite",   "Aceite",   "aceite",    "LT",    NA,                                      "OLIVA|AGUACATE|ALMENDRA|COCO|ACEITE DE CANOLA, BOTE DE 170 GR|PAM, ACEITE, DE CANOLA, BOTELLA DE 170 ML",
  "arroz",    "Arroz",    "arroz",     "KG",    NA,                                      NA,
  "azucar",   "Azúcar",   "azucar",    "KG",    "ESTANDAR",                              NA,
  "cebolla",  "Cebolla",  "cebolla",   "KG",    "BLANCA",                                NA,
  "frijol",   "Frijol",   "frijol",    "KG",    "PINTO",                                 NA,
  "huevo",    "Huevo",    "huevo",     "KG",    NA,                                      NA,
  "leche",    "Leche",    "leche",     "LT",    NA,                                      "SABOR",
  "papa",     "Papa",     "papa",      "KG",    "(?=.*PAPA)(?=.*BLANCA)(?=.*A GRANEL)",  NA,
  "tomate",   "Tomate",   "tomate",    "KG",    NA,                                      NA,
  "tortilla", "Tortilla", "tortillas", "KG",    "A GRANEL",                              NA
)
