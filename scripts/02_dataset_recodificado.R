# ============================================================
# PROYECTO: PetFinder Adoption Prediction - Dataset recodificado
# ============================================================
# Aplica las transformaciones establecidas en
# entregas/decisiones_univariado.qmd a partir del dataset enriquecido:
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
  !anyNA(datos_recod$VideoAmt_bin),                                   # VideoAmt sin perdidos
  sum(is.na(datos_recod$total_faces_bin)) == sum(is.na(datos_recod$total_faces)),  # solo los estructurales
  identical(levels(datos_recod$VideoAmt_bin), c("Sin video (0)", "Con video (≥ 1)")),
  identical(levels(datos_recod$total_faces_bin), c("Sin rostros (0)", "Con rostros (≥ 1)")),
  nrow(datos_recod) == nrow(datos)                                    # no se pierden filas
)
cat("Verificación superada: no se pierden observaciones.\n\n")

cat("--- Distribución de las variables recodificadas ---\n")
for (v in c("VideoAmt_bin", "total_faces_bin")) {
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
