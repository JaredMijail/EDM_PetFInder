# =============================================================================
# EDM_PetFinder — Análisis bivariado CUANTITATIVO vs CUANTITATIVO
# Variable de interés: Age (edad del animal, en meses)
# -----------------------------------------------------------------------------
# Objetivo del proyecto (objetivo general): estimar y EXPLICAR la velocidad de
# adopción a partir de las señales del anuncio. La pregunta específica 4 es
# "¿qué variables se asocian con la velocidad de adopción y en qué dirección?".
# Este script cubre, como paso previo al modelo, las asociaciones ENTRE las
# variables cuantitativas, tomando Age como variable de interés.
#
# POR QUÉ Age (justificación conceptual, NO basada en resultados posteriores):
#   1) La literatura que el propio proyecto cita (Zadeh et al., 2022, en el marco
#      conceptual) identifica la EDAD entre los atributos con mayor asociación a
#      la velocidad de adopción.
#   2) En el marco teórico de señalización, la edad es una señal tabular
#      observable al momento de publicar y potencialmente "intervenible" (dónde
#      y cómo se publicita un animal joven o adulto).
#   3) Es una variable de razón, con n completo (0 faltantes) y directamente
#      comparable en las tres fuentes del anuncio, lo que la vuelve una
#      referencia natural para ordenar el resto de señales cuantitativas.
#   La elección NO usa ninguna correlación medida en este script.
#
# Alcance: para cada par (Age, otra cuantitativa) se reporta
#   - el gráfico más adecuado (dispersión con ajuste / mapa de densidad 2D),
#   - medidas descriptivas y de asociación (covarianza, Pearson, Spearman,
#     regresión lineal simple) y su significancia,
#   - un párrafo interpretativo generado a partir de los valores calculados
#     (ninguna cifra se escribe a mano).
# Lectura: asociaciones descriptivas; el diseño es observacional, NO causal.
# =============================================================================

# ---- 0. Entorno -------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(knitr)
})

options(OutDec = ",")                 # separador decimal en español
set.seed(2026)

# Deja en TRUE para exportar las figuras a _borradores/bivar_figs/ (revisión);
# en FALSE se imprimen en pantalla (RStudio) y no se escribe ningún archivo.
GUARDAR_FIGS <- FALSE
DIR_FIGS     <- here::here("_borradores", "bivar_figs")

# ---- 1. Datos ---------------------------------------------------------------
datos <- readr::read_csv(
  here::here("data", "petfinder_enriquecido_final.csv"),
  show_col_types = FALSE
)

# Las 12 variables cuantitativas del proyecto (misma lista que el univariado)
CUANTI <- c("Age", "Quantity", "Fee", "PhotoAmt", "VideoAmt",
            "sentiment_score", "sentiment_magnitude",
            "total_labels", "total_faces", "total_colors",
            "desc_char_len", "desc_word_count")

VAR_INTERES <- "Age"

# Etiquetas legibles en español para ejes y tablas
ETIQ <- c(
  Age                 = "Edad (meses)",
  Quantity            = "Animales en el anuncio",
  Fee                 = "Tarifa (MYR)",
  PhotoAmt            = "Fotografías",
  VideoAmt            = "Videos",
  sentiment_score     = "Polaridad del sentimiento (−1 a 1)",
  sentiment_magnitude = "Intensidad del sentimiento",
  total_labels        = "Etiquetas detectadas",
  total_faces         = "Rostros detectados",
  total_colors        = "Colores detectados",
  desc_char_len       = "Caracteres del relato",
  desc_word_count     = "Palabras del relato"
)

etiqueta <- function(v) {           # vectorizada: sirve para escalares y vectores
  out <- unname(ETIQ[v])
  out[is.na(out)] <- v[is.na(out)]
  out
}

# ---- 2. Utilidades (formato y estilo del sitio) -----------------------------
fmt  <- function(x, d = 2) {
  x <- as.numeric(x)
  if (all(abs(x - round(x)) < 1e-9)) {
    return(format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE))
  }
  format(round(x, d), big.mark = ".", decimal.mark = ",", nsmall = d, scientific = FALSE)
}
ent  <- function(x) format(as.integer(round(as.numeric(x))), big.mark = ".", decimal.mark = ",")
pct  <- function(x, d = 1) gsub("\\.", ",", sprintf(paste0("%.", d, "f %%"), x))

# p-valor legible: "p < 0,001" cuando es muy pequeño
pval <- function(p) ifelse(is.na(p), "—",
                           ifelse(p < 0.001, "p < 0,001", sprintf("p = %s", fmt(p, 3))))

# Fuerza de la asociación lineal (umbrales orientativos de uso corriente)
fuerza <- function(r) {
  a <- abs(r)
  dplyr::case_when(
    is.na(a)  ~ "no estimable",
    a < 0.10  ~ "despreciable",
    a < 0.30  ~ "débil",
    a < 0.50  ~ "moderada",
    a < 0.70  ~ "fuerte",
    TRUE      ~ "muy fuerte"
  )
}
direccion <- function(r) ifelse(is.na(r), "", ifelse(r > 0, "positiva", "negativa"))

# Tema de los gráficos (idéntico al del sitio)
tema_u <- function() {
  theme_minimal(base_size = 11.5) +
    theme(plot.background  = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.title       = element_text(colour = "#1a3a5c", face = "bold", size = 11.5),
          plot.subtitle    = element_text(colour = "#4b5563", size = 9.5),
          axis.title       = element_text(colour = "#4b5563", size = 10),
          panel.grid.minor = element_blank(),
          legend.position  = "none")
}

# ---- 3. Medidas para un par (x = Age, y = otra cuantitativa) ----------------
# Covarianza y correlaciones se calculan sobre casos completos del par
# (eliminación por pares: cada par usa el máximo de observaciones disponibles).
medidas_par <- function(x, y, nombre_y) {
  ok <- complete.cases(x, y)
  x  <- as.numeric(x[ok]); y <- as.numeric(y[ok]); n <- length(x)

  t_p <- suppressWarnings(cor.test(x, y, method = "pearson"))
  t_s <- suppressWarnings(cor.test(x, y, method = "spearman"))

  m  <- stats::lm(y ~ x)               # regresión simple: y (otra) ~ Age
  sm <- summary(m)
  ci <- suppressMessages(stats::confint(m))

  tibble::tibble(
    Variable        = nombre_y,
    n               = n,
    Perdidos        = sum(!ok),
    r_Pearson       = unname(t_p$estimate),
    p_Pearson       = t_p$p.value,
    rho_Spearman    = unname(t_s$estimate),
    p_Spearman      = t_s$p.value,
    Covarianza      = stats::cov(x, y),
    Pendiente       = unname(stats::coef(m)[2]),
    Pendiente_li    = ci[2, 1],
    Pendiente_ls    = ci[2, 2],
    Intercepto      = unname(stats::coef(m)[1]),
    R2              = sm$r.squared,
    Error_residual  = sm$sigma,
    p_modelo        = sm$coefficients[2, 4],
    Media_y         = mean(y),
    Mediana_y       = stats::median(y),
    SD_y            = stats::sd(y),
    Unicos_y        = length(unique(y))
  )
}

# ---- 4. Gráficos del par ----------------------------------------------------
# Se elige el gráfico "más adecuado": cuando alguna de las dos variables es muy
# discreta (pocos valores distintos) la nube de puntos se agolpa en rejillas y
# se usa dispersión con jitter; cuando ambas son densas se añade un mapa de
# densidad 2D (binning) además de la dispersión con ajuste lineal.
graficos_par <- function(x, y, nombre_y) {
  d <- tibble::tibble(edad = as.numeric(x), otro = as.numeric(y)) |> tidyr::drop_na()
  discreta <- length(unique(d$otro)) <= 30 || length(unique(d$edad)) <= 30
  cap <- sprintf("Edad vs %s (n = %s)", etiqueta(nombre_y), ent(nrow(d)))

  p1 <- ggplot(d, aes(edad, otro)) +
    { if (discreta) geom_jitter(width = 0.35, height = 0.12, alpha = 0.10,
                                size = 0.5, colour = "#2e7d9a", na.rm = TRUE)
      else          geom_point(alpha = 0.10, size = 0.55, colour = "#2e7d9a", na.rm = TRUE) } +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = "#dc2626", fill = "#dc2626", alpha = 0.12, linewidth = 0.9) +
    labs(title = "Dispersión con ajuste lineal",
         subtitle = cap,
         x = etiqueta("Age"), y = etiqueta(nombre_y)) +
    tema_u()

  p2 <- ggplot(d, aes(edad, otro)) +
    geom_bin2d(bins = 45) +
    scale_fill_gradient(low = "#eaf1f5", high = "#1a3a5c",
                        name = "Anuncios", trans = "log10") +
    labs(title = "Mapa de densidad (conteo por celda)",
         subtitle = cap,
         x = etiqueta("Age"), y = etiqueta(nombre_y)) +
    tema_u() +
    theme(legend.position = "right")

  if (GUARDAR_FIGS) {
    dir.create(DIR_FIGS, showWarnings = FALSE, recursive = TRUE)
    ggsave(file.path(DIR_FIGS, sprintf("disp_%s.png", nombre_y)), p1,
           width = 6, height = 4, dpi = 130)
    ggsave(file.path(DIR_FIGS, sprintf("dens_%s.png", nombre_y)), p2,
           width = 6, height = 4, dpi = 130)
  } else {
    print(p1); print(p2)
  }
  invisible(NULL)
}

# ---- 5. Párrafo interpretativo por par (cifras calculadas, no transcritas) --
texto_par <- function(m) {
  sprintf(
    paste0(
      "Entre Age y %s se usaron %s anuncios (%s descartados por faltantes en el par). ",
      "La covarianza es %s (en unidades del producto meses × %s). ",
      "La asociación lineal (Pearson) es r = %s (%s), %s y %s; ",
      "la asociación monótona (Spearman) es ρ = %s (%s). ",
      "El modelo %s ~ Age estima una pendiente de %s (unidades de %s) por cada mes de edad ",
      "(IC 95 %%: %s a %s) con R² = %s, esto es, la edad explica el %s de la ",
      "variabilidad observada de %s. Lectura: la relación es %s y de intensidad %s; ",
      "con R² = %s la edad, por sí sola, %s."
    ),
    etiqueta(m$Variable),
    ent(m$n), ent(m$Perdidos),
    fmt(m$Covarianza, 2), etiqueta(m$Variable),
    fmt(m$r_Pearson, 3), pval(m$p_Pearson), direccion(m$r_Pearson), fuerza(m$r_Pearson),
    fmt(m$rho_Spearman, 3), pval(m$p_Spearman),
    etiqueta(m$Variable), fmt(m$Pendiente, 4), etiqueta(m$Variable),
    fmt(m$Pendiente_li, 4), fmt(m$Pendiente_ls, 4),
    fmt(m$R2, 4), pct(100 * m$R2, 1),
    etiqueta(m$Variable),
    direccion(m$r_Pearson), fuerza(m$r_Pearson),
    fmt(m$R2, 4),
    ifelse(m$R2 < 0.05,
           "no alcanza a ser un predictor útil (deja más del 95 % sin explicar)",
           "aporta una parte acotada pero no despreciable de la variabilidad")
  )
}

# ---- 6. Recorrido de los 11 pares -------------------------------------------
otras  <- setdiff(CUANTI, VAR_INTERES)
res    <- vector("list", length(otras))

for (i in seq_along(otras)) {
  v  <- otras[i]
  m  <- medidas_par(datos[[VAR_INTERES]], datos[[v]], v)
  res[[i]] <- m

  cat("\n", strrep("=", 78), "\n", sep = "")
  cat(sprintf("PAR %d/%d — %s vs %s\n", i, length(otras), etiqueta(VAR_INTERES), etiqueta(v)))
  cat(strrep("=", 78), "\n", sep = "")

  # gráficos del par
  graficos_par(datos[[VAR_INTERES]], datos[[v]], v)

  # ficha de medidas del par
  print(knitr::kable(
    tibble::tibble(
      Medida = c("n (casos completos)", "Perdidos en el par", "Covarianza",
                 "Pearson r", "Pearson p", "Spearman ρ", "Spearman p",
                 "Pendiente (β₁)", "IC 95 % β₁", "Intercepto (β₀)",
                 "R²", "p del modelo", "Error residual (σ)"),
      Valor  = c(ent(m$n), ent(m$Perdidos), fmt(m$Covarianza, 3),
                 fmt(m$r_Pearson, 3), pval(m$p_Pearson),
                 fmt(m$rho_Spearman, 3), pval(m$p_Spearman),
                 fmt(m$Pendiente, 4), sprintf("[%s ; %s]", fmt(m$Pendiente_li, 4), fmt(m$Pendiente_ls, 4)),
                 fmt(m$Intercepto, 3), fmt(m$R2, 4), pval(m$p_modelo),
                 fmt(m$Error_residual, 3))
    ), align = c("l", "r")))

  # párrafo
  cat("\nInterpretación. ", texto_par(m), "\n", sep = "")
}

# ---- 7. Tabla resumen de todos los pares ------------------------------------
resumen <- dplyr::bind_rows(res) |>
  dplyr::mutate(
    Variable   = etiqueta(Variable),
    Tipo       = ifelse(Perdidos > 0, "enriquecida (con faltantes)", "original"),
    `r (Pearson)`  = fmt(r_Pearson, 3),
    `ρ (Spearman)` = fmt(rho_Spearman, 3),
    Covarianza = fmt(Covarianza, 2),
    `β₁`       = fmt(Pendiente, 4),
    `IC 95 % β₁` = sprintf("[%s ; %s]", fmt(Pendiente_li, 4), fmt(Pendiente_ls, 4)),
    `R²`       = fmt(R2, 4),
    `Fuerza`   = fuerza(rho_Spearman),
    `Dirección` = direccion(rho_Spearman),
    `n`        = ent(n),
    `Perdidos` = ent(Perdidos)
  ) |>
  dplyr::arrange(dplyr::desc(abs(rho_Spearman))) |>
  dplyr::select(Variable, Tipo, n, Perdidos,
                `r (Pearson)`, `ρ (Spearman)`, Covarianza, `β₁`, `IC 95 % β₁`,
                `R²`, Fuerza, Dirección)

cat("\n\n", strrep("=", 78), "\n", sep = "")
cat("TABLA RESUMEN — Asociación de Age con las demás variables cuantitativas\n")
cat(strrep("=", 78), "\n", sep = "")
print(knitr::kable(resumen, align = c("l", "l", "r", "r", "r", "r", "r", "r", "l", "r", "l", "l")))

# ---- 8. Matriz de correlación (Spearman) de contexto ------------------------
cat("\n\n", strrep("=", 78), "\n", sep = "")
cat("Matriz de Spearman entre las 12 cuantitativas (contexto)\n")
cat(strrep("=", 78), "\n", sep = "")
mat <- stats::cor(datos[CUANTI], use = "pairwise.complete.obs", method = "spearman")
dimnames(mat) <- list(etiqueta(CUANTI), etiqueta(CUANTI))
print(knitr::kable(round(mat, 2)))

# ---- 9. Notas de lectura ----------------------------------------------------
cat("\n\n", strrep("-", 78), "\n", sep = "")
cat("Notas de lectura\n")
cat(strrep("-", 78), "\n", sep = "")
cat("1. Eliminación por pares: cada par usa todos los casos con dato en ambas\n")
cat("   variables (los faltantes del enriquecimiento no tienen dato en Age).\n")
cat("2. Empates: Age y varias de las otras variables son conteos/discretas con\n")
cat("   muchos empates, lo que atenúa el r de Pearson; por eso se reporta también\n")
cat("   Spearman (asociación monótona, robusta a empates y a la asimetría).\n")
cat("3. Dirección de la regresión: en todos los pares la respuesta es la OTRA\n")
cat("   variable (y) y el predictor es Age (x), según la variable de interés.\n")
cat("4. Redundancia: algunos pares entre las OTRAS variables son casi colineales\n")
cat("   (total_labels ≈ total_colors ≈ PhotoAmt; desc_char_len ≈ desc_word_count),\n")
cat("   lo que la matriz final hace visible para el modelamiento.\n")
cat("5. Causalidad: son asociaciones descriptivas; el diseño es observacional.\n")
cat(strrep("-", 78), "\n", sep = "")
