# Ambiente 
library(readr)
library(dplyr)
library(FactoClass)  # plotct() y cluster.carac()
library(knitr)       # kable()
library(RColorBrewer)

# Dataset PetFinder enriquecido
petfinder_enriquecido_final <- read_csv("Documents/UN/2026-2S/EDM/petfinder_enriquecido_final.csv")
colnames(petfinder_enriquecido_final)

# Variables cualitativas
cualitativas <- c('Type', 'Name', 'Breed1', 'Breed2', 'Gender', 'Color1', 'Color2', 'Color3', 'MaturitySize', 'FurLength', 'Vaccinated', 'Dewormed', 'Sterilized', 'Health', 'State', 'RescuerID', 'AdoptionSpeed')
length(cualitativas)

# Dataset variables cualitativas
petfinder_cuali <- petfinder_enriquecido_final %>% select(all_of(cualitativas))
# Convertimos las variables en factores
petfinder_cuali <- lapply(petfinder_cuali, as.factor)
# Lo convertimos a un objeto tipo df
petfinder_cuali <- as.data.frame(petfinder_cuali)


# Tratamiento de variables de alta cardinalidad ----
# Agrupa en 'Otros' las categorías de un factor cuya frecuencia relativa
# es menor al 1% (prop_min).
# Si NINGUNA categoría alcanza ese umbral, conserva las 10 (max_niveles) categorías
# más frecuentes y agrupa el resto en 'Otros'.

agrupar_niveles <- function(f, prop_min = 0.01, max_niveles = 10,
                            etiqueta = "Otros", nombre = "variable") {
  
  f <- factor(f)                          # elimina niveles sin observaciones
  if (nlevels(f) < 10) return(f)           # Si tiene 10 niveles o menos no hay nada que agrupar
  
  mf    <- table(f)                       # tamaño de cada categoría (excluye NA)
  n     <- sum(mf)                        # observaciones usadas
  n_min <- prop_min * n                   # tamaño mínimo de una categoría
  
  # Caso 1: todas alcanzan el mínimo -> sin cambios
  if (all(mf >= n_min)) return(f)
  
  # Caso 2: ninguna alcanza el mínimo -> se conservan las 10 (max_niveles)
  # categorías más frecuentes y el resto pasa a 'Otros'
  if (!any(mf >= n_min)) {
    top   <- names(sort(mf, decreasing = TRUE))[seq_len(max_niveles)]
    nuevo <- as.character(f)
    nuevo[!is.na(nuevo) & !(nuevo %in% top)] <- etiqueta
    message("'", nombre, "': ninguna categoría alcanza el ", 100 * prop_min,
            " %; se conservan las ", max_niveles, " más frecuentes.")
    return(factor(nuevo, levels = c(top, etiqueta)))
  }
  
  # Caso 3: se agrupan las categorías pequeñas
  orden <- names(sort(mf))                # de menor a mayor tamaño
  k <- max(sum(mf < n_min), 2)            # cuántas agrupar (al menos 2)
  
  # 'Otros' también debe alcanzar el mínimo; si no, se le suma la
  # siguiente categoría más pequeña
  while (k < length(orden) - 1 && sum(mf[orden[seq_len(k)]]) < n_min) {
    k <- k + 1
  }
  raras <- orden[seq_len(k)]
  
  # Los NA se respetan (no se convierten en 'Otros')
  nuevo <- as.character(f)
  nuevo[!is.na(nuevo) & nuevo %in% raras] <- etiqueta
  
  # 'Otros' al final; el resto ordenado de mayor a menor frecuencia
  mantener <- setdiff(names(sort(mf, decreasing = TRUE)), raras)
  factor(nuevo, levels = c(mantener, etiqueta))
}

# Copia de seguridad antes de modificar 
if (!exists("petfinder_cuali_orig")) petfinder_cuali_orig <- petfinder_cuali

# Variables a tratar: AdoptionSpeed queda fuera por ser la de referencia
vars <- setdiff(cualitativas, "AdoptionSpeed")

# Se aplica a cada variable, siempre partiendo de la base original, y se
# asigna de vuelta al data frame (así no se pierden las demás columnas)
petfinder_cuali[vars] <- lapply(vars, function(v) {
  agrupar_niveles(petfinder_cuali_orig[[v]], prop_min = 0.01, nombre = v)
})

# Verificación: niveles antes y después, peso de 'Otros' y esperado mínimo
antes     <- sapply(petfinder_cuali_orig[vars], function(x) nlevels(factor(x)))
despues   <- sapply(petfinder_cuali[vars], nlevels)
pct_otros <- sapply(petfinder_cuali[vars], function(x)
  if ("Otros" %in% levels(x)) round(100 * mean(x == "Otros", na.rm = TRUE), 1) else 0)
min_esp   <- sapply(petfinder_cuali[vars], function(x) {
  tab <- table(x, petfinder_cuali$AdoptionSpeed)
  round(min(outer(rowSums(tab), colSums(tab)) / sum(tab)), 2)
})
data.frame(antes, despues, pct_otros, min_esp)

# Peso de cada categoría en una variable concreta (en %)
round(100 * prop.table(table(petfinder_cuali$Breed1)), 1)

# Análisis Bivariado ----

analisis_bivariado <- function(variable) {
  
  # ---------------------------------------------------------------
  # 0. VALIDACIONES: se comprueba el argumento antes de calcular
  # ---------------------------------------------------------------
  if (!is.character(variable) || length(variable) != 1) {
    stop("'variable' debe ser un único nombre entre comillas, p. ej. \"Type\".")
  }
  if (variable == "AdoptionSpeed") {
    stop("AdoptionSpeed es la variable de referencia; elige otra variable.")
  }
  if (!variable %in% cualitativas) {
    stop("'", variable, "' no pertenece al vector 'cualitativas'.")
  }
  if (!variable %in% names(petfinder_cuali)) {
    stop("'", variable, "' no es una columna de petfinder_cuali.")
  }
  
  # ---------------------------------------------------------------
  # 1. ACCESO A LAS COLUMNAS
  # ---------------------------------------------------------------
  x <- petfinder_cuali[[variable]]        # variable a contrastar
  y <- petfinder_cuali$AdoptionSpeed      # variable de referencia (fija)
  
  # Aviso (no detiene la función): con muchos niveles las tablas y
  # los gráficos se vuelven ilegibles (Name, RescuerID, Breed1...)
  if (nlevels(factor(x)) > 30) {
    warning("'", variable, "' tiene ", nlevels(factor(x)),
            " niveles: las tablas y los gráficos pueden ser ilegibles.")
  }
  
  # Función auxiliar para imprimir encabezados ordenados
  titulo <- function(txt) {
    cat("\n", strrep("=", 70), "\n", txt, "\n", strrep("=", 70), "\n", sep = "")
  }
  
  # ---------------------------------------------------------------
  # 2. TABLA DE CONTINGENCIA
  #    dnn pone nombres a las dimensiones (filas = variable,
  #    columnas = AdoptionSpeed).
  # ---------------------------------------------------------------
  titulo(paste0("1. Tabla de contingencia: ", variable, " x AdoptionSpeed"))
  tab <- unclass(table(x, y, dnn = c(variable, "AdoptionSpeed")))
  print(tab)
  
  # n = observaciones realmente usadas en la tabla (table() excluye NA)
  n <- sum(tab)
  
  # ---------------------------------------------------------------
  #    GRÁFICOS: perfil fila (arriba) y perfil columna (abajo)
  # ---------------------------------------------------------------
  titulo("2. Gráficos de perfil fila (arriba) y perfil columna (abajo)")
  par(mfrow=c(2,1), mai=c(0.4,1,0.3,0.1))
  
  colors_set <- brewer.pal(5, "Pastel2")
  colors_set <- colorRampPalette(colors_set)(13)
  
  # main se pasa a barplot() a través de '...', así que ya no hace
  # falta title()
  tabs <- plotct(tab, "row", col = colors_set,
                 main = paste0("Perfil fila: ", variable, " vs AdoptionSpeed"),
                 tables = TRUE)
  plotct(t(tab), "row", col = colors_set,
         main = paste0("Perfil columna: AdoptionSpeed vs ", variable))
  
  # ---------------------------------------------------------------
  # 3. PERFILES FILA
  # ---------------------------------------------------------------
  titulo(paste0("3. Perfiles fila: ", variable))
  # p_{j|i}: cada fila suma 1 (distribución de AdoptionSpeed dentro
  # de cada categoría de la variable)
  print(kable(tabs$perR, digits = 1))
  
  # ---------------------------------------------------------------
  # 4. PERFILES COLUMNA
  # ---------------------------------------------------------------
  titulo(paste0("4. Perfiles columna: ", variable))
  # p_{i|j}: cada columna suma 1 (distribución de la variable dentro
  # de cada categoría de AdoptionSpeed)
  print(kable(tabs$perC, digits = 1))

  # ---------------------------------------------------------------
  # 5. PROPORCIONES CONJUNTAS Y FRECUENCIAS
  # ---------------------------------------------------------------
  titulo("5. Proporciones conjuntas (%)")
  print(kable(tabs$ctm / n * 100, digits = 1))
  
  titulo("6. Frecuencias con totales marginales")
  print(kable(tabs$ctm, digits = 0))
  
  # ---------------------------------------------------------------
  # 6. PRUEBA CHI-CUADRADO (resultado completo, objeto 'htest')
  # ---------------------------------------------------------------
  titulo(paste0("7. Chi-cuadrado: ", variable, " vs AdoptionSpeed"))
  chi <- chisq.test(x, y)
  chi$data.name <- paste(variable, "and AdoptionSpeed")  # etiqueta legible
  print(chi)
  
  # ---------------------------------------------------------------
  # 7. VALORES TEST
  #    data.frame(AdoptionSpeed = y) da nombre a la columna;
  #    'x' (la variable elegida) actúa como clase.
  #    withVisible() reproduce el comportamiento de escribir la
  #    llamada en la consola: se imprime solo si la función devuelve
  #    un resultado visible.
  # ---------------------------------------------------------------
  titulo(paste0("8. Valores test (clases = ", variable, ")"))
  vt <- withVisible(cluster.carac(data.frame(AdoptionSpeed = y), x))
  if (vt$visible) print(vt$value)
  invisible(list(tabla = tab, perfiles = tabs, n = n,
                 chisq = chi, valores_test = vt$value))
}

vars
analisis_bivariado('Type')
analisis_bivariado("Name")
analisis_bivariado("Breed1")
analisis_bivariado('Breed2')
analisis_bivariado('Gender')
analisis_bivariado('Color1')
analisis_bivariado('Color2')
analisis_bivariado('Color3')
analisis_bivariado('MaturitySize')
analisis_bivariado('FurLength')
analisis_bivariado('Vaccinated')
analisis_bivariado('Dewormed')
analisis_bivariado('Sterilized')
analisis_bivariado('Health')
analisis_bivariado('State')
analisis_bivariado('RescuerID')
