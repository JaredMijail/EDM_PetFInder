# ============================================================
# PROYECTO: PetFinder Adoption Prediction - Dataset enriquecido
# ============================================================

# ---- 0. Librerías ------------------------------------------
# install.packages("renv")

library(tidyverse)
library(jsonlite)
library(here)


# ---- 1. Rutas dinámicas ------------------------------------

ruta_train_csv <- here("data", "train.csv")
ruta_sentiment <- here("data", "train_sentiment")
ruta_metadata  <- here("data", "train_metadata")

# ---- 2. Leer CSV principal ---------------------------------
train <- read_csv(ruta_train_csv, show_col_types = FALSE)
cat("Filas originales:", nrow(train), "| Columnas:", ncol(train), "\n\n")

# ---- 3. Sentimiento (NLP) ----------------------------------
procesar_sentimiento <- function(ruta_carpeta, df) {
  archivos <- list.files(ruta_carpeta, pattern = "\\.json$", full.names = TRUE)
  if (length(archivos) == 0) {
    warning("No se encontraron JSON en: ", ruta_carpeta); return(df)
  }
  datos <- map_dfr(archivos, function(f) {
    pet_id <- tools::file_path_sans_ext(basename(f))
    j <- tryCatch(fromJSON(f), error = function(e) NULL)
    if (!is.null(j) && !is.null(j$documentSentiment)) {
      tibble(PetID = pet_id,
             sentiment_score     = j$documentSentiment$score,
             sentiment_magnitude = j$documentSentiment$magnitude)
    } else {
      tibble(PetID = pet_id, sentiment_score = NA, sentiment_magnitude = NA)
    }
  })
  df %>% left_join(datos, by = "PetID")
}

# ---- 4. Metadatos de imagen (Vision API) -------------------
procesar_metadata <- function(ruta_carpeta, df) {
  archivos <- list.files(ruta_carpeta, pattern = "\\.json$", full.names = TRUE)
  if (length(archivos) == 0) {
    warning("No se encontraron JSON en: ", ruta_carpeta); return(df)
  }
  datos <- map_dfr(archivos, function(f) {
    pet_id <- str_extract(basename(f), "^[^-]+")
    j <- tryCatch(fromJSON(f), error = function(e) NULL)
    if (!is.null(j)) {
      n_labels <- length(j$labelAnnotations$description)
      n_faces  <- length(j$faceAnnotations$detectionConfidence)
      colores  <- j$imagePropertiesAnnotation$dominantColors$colors
      n_colors <- if (!is.null(colores) && is.data.frame(colores)) nrow(colores) else 0
      tibble(PetID = pet_id, n_labels = n_labels, n_faces = n_faces, n_colors = n_colors)
    } else {
      tibble(PetID = pet_id, n_labels = NA, n_faces = NA, n_colors = NA)
    }
  })
  datos_agg <- datos %>%
    group_by(PetID) %>%
    summarise(
      total_labels = sum(n_labels, na.rm = TRUE),
      total_faces  = sum(n_faces,  na.rm = TRUE),
      total_colors = sum(n_colors, na.rm = TRUE),
      .groups = "drop"
    )
  df %>% left_join(datos_agg, by = "PetID")
}

# ---- 5. Aplicar extracciones -------------------------------
cat("Procesando sentimientos...\n")
train_enriquecido <- procesar_sentimiento(ruta_sentiment, train)

cat("Procesando metadatos de imágenes...\n")
train_enriquecido <- procesar_metadata(ruta_metadata, train_enriquecido)

cat("Dataset tras uniones:", nrow(train_enriquecido), "x",
    ncol(train_enriquecido), "\n\n")

# ---- 6. Convertir categóricas a FACTOR ---------------------
train_enriquecido <- train_enriquecido %>%
  mutate(
    Type          = factor(Type, levels = c(1, 2),
                           labels = c("Dog", "Cat")),
    Gender        = factor(Gender, levels = c(1, 2, 3),
                           labels = c("Male", "Female", "Mixed")),
    MaturitySize  = factor(MaturitySize, levels = c(0, 1, 2, 3, 4),
                           labels = c("NotSpec", "Small", "Medium",
                                      "Large", "XLarge")),
    FurLength     = factor(FurLength, levels = c(0, 1, 2, 3),
                           labels = c("NotSpec", "Short", "Medium", "Long")),
    Vaccinated    = factor(Vaccinated, levels = c(1, 2, 3),
                           labels = c("Yes", "No", "NotSure")),
    Dewormed      = factor(Dewormed,   levels = c(1, 2, 3),
                           labels = c("Yes", "No", "NotSure")),
    Sterilized    = factor(Sterilized, levels = c(1, 2, 3),
                           labels = c("Yes", "No", "NotSure")),
    Health        = factor(Health, levels = c(0, 1, 2, 3),
                           labels = c("NotSpec", "Healthy",
                                      "MinorInjury", "SeriousInjury")),
    AdoptionSpeed = factor(AdoptionSpeed, levels = 0:4,
                           labels = c("SameDay", "1-7d", "8-30d",
                                      "31-90d", "NoAdoption")),
    across(c(Breed1, Breed2, Color1, Color2, Color3, State), as.factor)
  )

# ---- 6.b NUEVA: longitud del texto descriptivo -------------
train_enriquecido <- train_enriquecido %>%
  mutate(
    desc_char_len   = nchar(Description, type = "chars"),  # nº de caracteres
    desc_word_count = str_count(Description, "\\S+")       # nº de palabras
  )

# ---- 7. Verificación final ---------------------------------
vars_num <- names(train_enriquecido)[sapply(train_enriquecido, is.numeric)]
vars_cat <- names(train_enriquecido)[sapply(train_enriquecido, function(x)
  is.factor(x) || is.character(x))]

cat("=== RESUMEN FINAL ===\n")
cat("Filas:", nrow(train_enriquecido), "| Columnas:", ncol(train_enriquecido), "\n")
cat("Cuantitativas:", length(vars_num), "\n"); print(vars_num)
cat("\nCualitativas:", length(vars_cat), "\n"); print(vars_cat)

cat("\n--- Diagnóstico de NA ---\n")
na_info <- data.frame(
  variable = names(train_enriquecido),
  n_NA     = colSums(is.na(train_enriquecido)),
  pct_NA   = round(colMeans(is.na(train_enriquecido)) * 100, 2),
  tipo     = sapply(train_enriquecido, function(x) class(x)[1])
) %>% arrange(desc(pct_NA))
print(na_info, row.names = FALSE)

# ---- 8. Guardar --------------------------------------------
ruta_csv <- here("data", "petfinder_enriquecido_final.csv")
ruta_rds <- here("data", "petfinder_enriquecido_final.rds")

readr::write_csv(train_enriquecido, ruta_csv)
saveRDS(train_enriquecido, ruta_rds)

cat("\n✅ Archivos guardados:\n")
cat("  -", ruta_csv, "\n")
cat("  -", ruta_rds, "\n")
cat("  Tamaño CSV:", round(file.size(ruta_csv) / 1024^2, 2), "MB\n")
cat("  Tamaño RDS:", round(file.size(ruta_rds) / 1024^2, 2), "MB\n")