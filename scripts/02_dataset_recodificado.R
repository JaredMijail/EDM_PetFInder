# ============================================================
# PROYECTO: PetFinder Adoption Prediction - Dataset recodificado
# ============================================================
# Aplica las transformaciones establecidas en
# entregas/decisiones_univariado.qmd a partir del dataset enriquecido:
#   - Quantity    -> cualitativa ordinal (3 niveles: 1 / 2-3 / >=4)
#   - Fee         -> cualitativa ordinal (3 niveles: 0 / 1-99 / >=100)
#   - VideoAmt    -> binaria (sin/con video)
#   - total_faces -> binaria (sin/con rostros; los perdidos se conservan)
# ============================================================

# ---- 0. Librerías ------------------------------------------
library(tidyverse)
library(here)


# ---- 1. Rutas ----------------------------------------------
ruta_entrada <- here("data", "petfinder_enriquecido_final.csv")
ruta_csv     <- here("data", "petfinder_recodificado_final.csv")
ruta_rds     <- here("data", "petfinder_recodificado_final.rds")

# ---- 2. Leer dataset enriquecido ---------------------------
datos <- readr::read_csv(ruta_entrada, show_col_types = FALSE)
cat("Dataset enriquecido:", nrow(datos), "x", ncol(datos), "\n\n")


# ---- 3. Recodificaciones -----------------------------------
datos_recod <- datos |>
  mutate(
    # Quantity -> ordinal de 3 niveles, orden creciente en el nº de animales
    Quantity_ord = factor(
      case_when(
        Quantity == 1     ~ "Único (1)",
        Quantity %in% 2:3 ~ "Pocos (2–3)",
        Quantity >= 4     ~ "Muchos (≥ 4)"
      ),
      levels  = c("Único (1)", "Pocos (2–3)", "Muchos (≥ 4)"),
      ordered = TRUE
    ),
    # Fee -> ordinal de 3 niveles: gratuita vs. tarifa baja/alta.
    # Corte en 100 (≈ mediana de las tarifas positivas) para repartir la masa
    # de los anuncios con tarifa lo más equitativamente posible.
    Fee_cat = factor(
      case_when(
        Fee == 0   ~ "Gratuita (0)",
        Fee < 100  ~ "Tarifa baja (< 100)",
        Fee >= 100 ~ "Tarifa alta (≥ 100)"
      ),
      levels  = c("Gratuita (0)", "Tarifa baja (< 100)", "Tarifa alta (≥ 100)"),
      ordered = TRUE
    ),
    # VideoAmt -> binaria: presencia/ausencia de video
    VideoAmt_bin = factor(
      ifelse(VideoAmt == 0, "Sin video (0)", "Con video (≥ 1)"),
      levels = c("Sin video (0)", "Con video (≥ 1)")
    ),
    # total_faces -> binaria: presencia/ausencia de rostros (perdidos -> NA)
    total_faces_bin = factor(
      case_when(
        is.na(total_faces) ~ NA_character_,
        total_faces == 0   ~ "Sin rostros (0)",
        total_faces >= 1   ~ "Con rostros (≥ 1)"
      ),
      levels = c("Sin rostros (0)", "Con rostros (≥ 1)")
    )
  )


# ---- 4. Verificación ---------------------------------------
stopifnot(
  !anyNA(datos_recod$Quantity_ord),                                   # Quantity sin perdidos
  !anyNA(datos_recod$Fee_cat),                                        # Fee sin perdidos
  !anyNA(datos_recod$VideoAmt_bin),                                   # VideoAmt sin perdidos
  sum(is.na(datos_recod$total_faces_bin)) == sum(is.na(datos_recod$total_faces)),  # solo los estructurales
  identical(levels(datos_recod$Quantity_ord), c("Único (1)", "Pocos (2–3)", "Muchos (≥ 4)")),
  identical(levels(datos_recod$Fee_cat), c("Gratuita (0)", "Tarifa baja (< 100)", "Tarifa alta (≥ 100)")),
  nrow(datos_recod) == nrow(datos)                                    # no se pierden filas
)
cat("Verificación superada: no se pierden observaciones.\n\n")

cat("--- Distribución de las variables recodificadas ---\n")
for (v in c("Quantity_ord", "Fee_cat", "VideoAmt_bin", "total_faces_bin")) {
  cat("\n>", v, "(perdidos:", sum(is.na(datos_recod[[v]])), ")\n")
  print(round(100 * prop.table(table(datos_recod[[v]])), 2))
}

cat("\n=== RESUMEN FINAL ===\n")
cat("Filas:", nrow(datos_recod), "| Columnas:", ncol(datos_recod), "\n")
cat("Nuevas columnas:",
    paste(setdiff(names(datos_recod), names(datos)), collapse = ", "), "\n")


# ---- 5. Guardar --------------------------------------------
readr::write_csv(datos_recod, ruta_csv)
saveRDS(datos_recod, ruta_rds)

cat("\nArchivos guardados:\n")
cat("  -", ruta_csv, "\n")
cat("  -", ruta_rds, "\n")
cat("  Tamaño CSV:", round(file.size(ruta_csv) / 1024^2, 2), "MB\n")
cat("  Tamaño RDS:", round(file.size(ruta_rds) / 1024^2, 2), "MB\n")
