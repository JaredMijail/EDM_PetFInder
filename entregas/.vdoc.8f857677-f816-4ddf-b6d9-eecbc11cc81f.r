#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: setup-comun
#| echo: false
#| include: false
# Carga y traducción de las categorías del conjunto de datos
source(here::here("_setup.R"))
#
#
#
#
#
#| layout-ncol: 2
# --- estadísticos ---
moda      <- function(x) { x <- x[!is.na(x)]; ux <- unique(x); ux[which.max(tabulate(match(x, ux)))] }
asimetria <- function(x) { x <- x[!is.na(x)]; n <- length(x); m <- mean(x); s <- sd(x); n/((n-1)*(n-2))*sum(((x-m)/s)^3) }
curtosis  <- function(x) { x <- x[!is.na(x)]; n <- length(x); m <- mean(x); s <- sd(x)
  n*(n+1)/((n-1)*(n-2)*(n-3))*sum(((x-m)/s)^4) - 3*(n-1)^2/((n-2)*(n-3)) }

esc_num <- function(x) {
  x <- as.numeric(x); v <- x[!is.na(x)]; q <- quantile(v, c(.25, .75)); iqr <- diff(q)
  z <- (v - mean(v)) / sd(v); li <- q[1] - 1.5*iqr; ls <- q[2] + 1.5*iqr
  list(n = length(v), na = sum(is.na(x)), media = mean(v), mediana = median(v), moda = as.numeric(moda(v)),
       min = min(v), max = max(v), q1 = q[1], q3 = q[2], iqr = iqr, rango = max(v) - min(v),
       var = var(v), sd = sd(v), cv = 100*sd(v)/mean(v), asim = asimetria(v), curt = curtosis(v),
       out_iqr = sum(v < li | v > ls), pct_out_iqr = 100*mean(v < li | v > ls),
       out_z = sum(abs(z) > 3), pct_out_z = 100*mean(abs(z) > 3), pct_ceros = 100*mean(v == 0),
       unicos = length(unique(v)))
}

# --- Tablas ---
tabla_estadisticos <- function(x, caption = NULL) {
  s <- esc_num(x)

  tab <- tibble::tibble(
    `Estadístico` = c("Entradas válidas", "Valores faltantes", "Media", "Mediana", "Moda",
                      "Mínimo", "Máximo", "Q1", "Q3",
                      "IQR", "Rango", "Varianza", "Desviación estándar",
                      "Coeficiente de variación", "Asimetría", "Curtosis"),
    Valor = c(ent(s$n), ent(s$na), fmt(s$media), fmt(s$mediana), fmt(s$moda), fmt(s$min),
              fmt(s$max), fmt(s$q1), fmt(s$q3), fmt(s$iqr), fmt(s$rango), fmt(s$var),
              fmt(s$sd), pct(s$cv), fmt(s$asim, 3), fmt(s$curt, 3))
  )

  izq <- tab[1:8, ]
  der <- tab[9:16, ]

  knitr::kables(list(
    knitr::kable(izq, align = c("l", "r")),
    knitr::kable(der, align = c("l", "r"))
  ),
  caption = caption)
}

tabla_atipicos <- function(x, caption = NULL) {
  v <- as.numeric(x)
  v <- v[!is.na(v)]
  n <- length(v)

  q   <- unname(quantile(v, c(.25, .75)))
  riq <- q[2] - q[1]

  # Multiplicadores de las cercas de Tukey
  c_tukey <- c("Atípicos (1,5 × RIQ)" = 1.5,
               "Extremos (3 × RIQ)"   = 3)

  li   <- unname(q[1] - c_tukey * riq)
  ls   <- unname(q[2] + c_tukey * riq)
  n_at <- vapply(seq_along(c_tukey),
                 function(i) sum(v < li[i] | v > ls[i]),
                 numeric(1))

  tab <- tibble::tibble(
    Criterio          = names(c_tukey),
    `Límite inferior` = fmt(li),
    `Límite superior` = fmt(ls),
    Atípicos          = ent(n_at),
    `% del total`     = pct(100 * n_at / n)
  )

  knitr::kable(
    tab,
    format   = if (knitr::is_latex_output()) "latex" else "pipe",
    booktabs = TRUE,
    linesep  = "",
    align    = c("l", "r", "r", "r", "r"),
    caption  = caption,
    escape   = FALSE
  )
}

# --- Gráficos ---
tema_u <- function() {
  theme_minimal(base_size = 11.5) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(colour = "#1a3a5c", face = "bold", size = 11.5),
          axis.title = element_text(colour = "#4b5563", size = 10),
          panel.grid.minor = element_blank())
}

graficos_cuanti <- function(x, etiqueta, tipo = c("continuo", "conteo")) {
  tipo <- match.arg(tipo)
  d <- tibble::tibble(v = as.numeric(x))
  p1 <- ggplot(d, aes(v)) +
    geom_histogram(bins = 30, fill = "#2e7d9a", colour = "white", linewidth = 0.2) +
    labs(title = "Histograma", x = etiqueta, y = "Frecuencia") + tema_u()
  p2 <- ggplot(d, aes(y = v)) +
    stat_boxplot(geom = "errorbar", width = 0.15) +
    geom_boxplot(
      fill = "#d97706", 
      width = 0.32, 
      outlier.shape = 1,       
      outlier.colour = "black",  
      outlier.alpha = 1,       
      outlier.size = 1.5        
    ) +
    labs(title = "Diagrama de caja", x = NULL, y = etiqueta) +
    tema_u() +
    theme(
      axis.text.x = element_blank(),
      aspect.ratio = 1.8
    )
  p3 <- ggplot(d, aes(v)) +
    geom_density(fill = "#0f766e", alpha = 0.35, colour = "#0f766e") +
    labs(title = "Densidad", x = etiqueta, y = "Densidad") + tema_u()
  p4 <- if (tipo == "continuo") {
    ggplot(d, aes(sample = v)) +
      stat_qq(colour = "#1a3a5c", alpha = 0.4, size = 0.8) +
      stat_qq_line(colour = "#d97706") +
      labs(title = "QQ-plot (referencia normal)", x = "Cuantiles teóricos", y = "Cuantiles muestrales") + tema_u()
  } else {
    d |> count(v) |> slice_max(n, n = 8) |> mutate(v = factor(v)) |>
      ggplot(aes(x = v, y = n)) + geom_col(fill = "#1a3a5c") +
      labs(title = "Valores más frecuentes", x = etiqueta, y = "Frecuencia") + tema_u()
  }
  for (p in list(p1, p2, p3, p4)) print(p)
  invisible(NULL)
}
#
#
#
#
#
#
#| label: cuanti-Age
#| echo: true
tabla_estadisticos(datos$Age, caption = "Estadísticos descriptivos — Edad (meses)")
#
#
#
#
#| label: atip-Age
#| echo: true
tabla_atipicos(datos$Age, caption = "Detección de valores atípicos — Edad (meses)")
#
#
#
#
#| label: graf-Age
#| echo: true
#| layout-ncol: 2
graficos_cuanti(datos$Age, "Edad (meses)", tipo = "conteo")
#
#
#
#
#
#| label: cuanti-Quantity
#| echo: true
tabla_estadisticos(datos$Quantity, caption = "Estadísticos descriptivos — Cantidad de animales en el anuncio")
#
#
#
#
#| label: atip-Quantity
#| echo: true
tabla_atipicos(datos$Quantity, caption = "Detección de valores atípicos — Cantidad de animales en el anuncio")
#
#
#
#
#| label: graf-Quantity
#| echo: true
#| layout-ncol: 2
graficos_cuanti(datos$Quantity, "Animales en el anuncio", tipo = "conteo")
#
#
#
#
#
#| label: cuanti-Fee
#| echo: true
tabla_estadisticos(datos$Fee, caption = "Estadísticos descriptivos — Tarifa de adopción")
#
#
#
#
#| label: atip-Fee
#| echo: true
tabla_atipicos(datos$Fee, caption = "Detección de valores atípicos — Tarifa de adopción")
#
#
#
#
#| label: graf-Fee
#| echo: true
#| layout-ncol: 2
graficos_cuanti(datos$Fee, "Tarifa de adopción", tipo = "conteo")
#
#
#
#
#
#
#| label: cuanti-PhotoAmt
#| echo: true
tabla_estadisticos(datos$PhotoAmt, caption = "Estadísticos descriptivos — Cantidad de fotos")
#
#
#
#
#| label: atip-PhotoAmt
#| echo: true
tabla_atipicos(datos$PhotoAmt, caption = "Detección de valores atípicos — Cantidad de fotos")
#
#
#
#
#| label: graf-PhotoAmt
#| echo: true
#| layout-ncol: 2
graficos_cuanti(datos$PhotoAmt, "Cantidad de fotos", tipo = "conteo")
#
#
#
#
#
#| label: cuanti-VideoAmt
#| echo: true
tabla_estadisticos(datos$VideoAmt, caption = "Estadísticos descriptivos — Cantidad de videos")
#
#
#
#
#| label: atip-VideoAmt
#| echo: true
tabla_atipicos(datos$VideoAmt, caption = "Detección de valores atípicos — Cantidad de videos")
#
#
#
#
#| label: graf-VideoAmt
#| echo: true
#| layout-ncol: 2
graficos_cuanti(datos$VideoAmt, "Cantidad de videos", tipo = "conteo")
#
#
#
#
#
#| label: cuanti-sentiment-score
#| echo: true
tabla_estadisticos(datos$sentiment_score, caption = "Estadísticos descriptivos — Puntaje de sentimiento")
#
#
#
#| label: atip-sentiment-score
#| echo: true
tabla_atipicos(datos$sentiment_score, caption = "Detección de valores atípicos — Puntaje de sentimiento")
#
#
#
#| label: graf-sentiment-score
#| echo: true
#| layout-ncol: 2
graficos_cuanti(datos$sentiment_score, "Puntaje de sentimiento", tipo = "conteo")
#
#
#
#
#
#| label: cuanti-sentiment-magnitude
#| echo: true
tabla_estadisticos(datos$sentiment_magnitude, caption = "Estadísticos descriptivos — Magnitud de sentimiento")
#
#
#
#| label: atip-sentiment-magnitude
#| echo: true
tabla_atipicos(datos$sentiment_magnitude, caption = "Detección de valores atípicos — Magnitud de sentimiento")
#
#
#
#| label: graf-sentiment-magnitude
#| echo: true
#| layout-ncol: 2
graficos_cuanti(datos$sentiment_magnitude, "Magnitud de sentimiento", tipo = "conteo")
#
#
#
#
#
#| label: cuanti-total-labels
#| echo: true
tabla_estadisticos(datos$total_labels, caption = "Estadísticos descriptivos — Total de etiquetas")
#
#
#
#| label: atip-total-labels
#| echo: true
tabla_atipicos(datos$total_labels, caption = "Detección de valores atípicos — Total de etiquetas")
#
#
#
#| label: graf-total-labels
#| echo: true
#| layout-ncol: 2
graficos_cuanti(datos$total_labels, "Total de etiquetas", tipo = "conteo")
#
#
#
#
#
#| label: cuanti-total-faces
#| echo: true
tabla_estadisticos(datos$total_faces, caption = "Estadísticos descriptivos — Total de rostros detectados")
#
#
#
#| label: atip-total-faces
#| echo: true
tabla_atipicos(datos$total_faces, caption = "Detección de valores atípicos — Total de rostros detectados")
#
#
#
#| label: graf-total-faces
#| echo: true
#| layout-ncol: 2
graficos_cuanti(datos$total_faces, "Total de rostros detectados", tipo = "conteo")
#
#
#
#
#
#| label: cuanti-total-colors
#| echo: true
tabla_estadisticos(datos$total_colors, caption = "Estadísticos descriptivos — Total de colores detectados")
#
#
#
#| label: atip-total-colors
#| echo: true
tabla_atipicos(datos$total_colors, caption = "Detección de valores atípicos — Total de colores detectados")
#
#
#
#| label: graf-total-colors
#| echo: true
#| layout-ncol: 2
graficos_cuanti(datos$total_colors, "Total de colores detectados", tipo = "conteo")
#
#
#
#
#
#| label: cuanti-desc-char-len
#| echo: true
tabla_estadisticos(datos$desc_char_len, caption = "Estadísticos descriptivos — Longitud de la descripción (caracteres)")
#
#
#
#| label: atip-desc-char-len
#| echo: true
tabla_atipicos(datos$desc_char_len, caption = "Detección de valores atípicos — Longitud de la descripción (caracteres)")
#
#
#
#| label: graf-desc-char-len
#| echo: true
#| layout-ncol: 2
graficos_cuanti(datos$desc_char_len, "Longitud de la descripción (caracteres)", tipo = "conteo")
#
#
#
#
#
#| label: cuanti-desc-word-count
#| echo: true
tabla_estadisticos(datos$desc_word_count, caption = "Estadísticos descriptivos — Cantidad de palabras en la descripción")
#
#
#
#| label: atip-desc-word-count
#| echo: true
tabla_atipicos(datos$desc_word_count, caption = "Detección de valores atípicos — Cantidad de palabras en la descripción")
#
#
#
#| label: graf-desc-word-count
#| echo: true
#| layout-ncol: 2
graficos_cuanti(datos$desc_word_count, "Cantidad de palabras en la descripción", tipo = "conteo")
```
#
#
#
#
#
tabla_frecuencias <- function(x, etiqueta = "Categoría", n_top = 10) {
  v <- as.character(x); tb <- sort(table(v), decreasing = TRUE); n <- length(tb); tot <- sum(tb)
  if (n > n_top) {
    tab <- tibble::tibble(
      Categoría = c(names(tb)[1:(n_top - 1)], sprintf("Otras (%d categorías)", n - n_top + 1)),
      Frecuencia = c(as.integer(tb[1:(n_top - 1)]), tot - sum(tb[1:(n_top - 1)])))
  } else {
    tab <- tibble::tibble(Categoría = names(tb), Frecuencia = as.integer(tb))
  }
  tab |>
    mutate(`Frecuencia relativa` = 100*Frecuencia/sum(Frecuencia),
           `Frecuencia acumulada` = cumsum(`Frecuencia relativa`)) |>
    mutate(Frecuencia = ent(Frecuencia),
           `Frecuencia relativa` = pct(`Frecuencia relativa`),
           `Frecuencia acumulada` = pct(`Frecuencia acumulada`)) |>
    knitr::kable(col.names = c(etiqueta, "Frecuencia absoluta", "Frecuencia relativa",
                               "Frecuencia acumulada"), align = c("l", "r", "r", "r"))
}

tabla_cardinalidad <- function(x) {
  v <- as.character(x)
  tibble::tibble(
    Verificación = c("Observaciones", "Valores perdidos", "Categorías distintas",
                     "Proporción de valores distintos", "Categoría más frecuente",
                     "Frecuencia de la moda"),
    Resultado = c(ent(length(v)), ent(sum(is.na(x))), ent(length(unique(v))),
                  pct(100*length(unique(v))/length(v)),
                  names(sort(table(v), decreasing = TRUE))[1],
                  pct(100*max(table(v))/length(v)))
  ) |> knitr::kable(align = c("l", "r"))
}

graficos_cuali <- function(x, etiqueta, pastel = FALSE, n_top = 10) {
  v <- as.character(x); tb <- sort(table(v), decreasing = TRUE); n <- length(tb); tot <- sum(tb)
  d <- if (n > n_top) {
    tibble::tibble(cat = c(names(tb)[1:(n_top - 1)], sprintf("Otras (%d)", n - n_top + 1)),
                   n = c(as.integer(tb[1:(n_top - 1)]), tot - sum(tb[1:(n_top - 1)])))
  } else tibble::tibble(cat = names(tb), n = as.integer(tb))
  d <- d |> mutate(cat = factor(cat, levels = rev(cat)),
                   pct = 100*n/sum(n))
  p1 <- ggplot(d, aes(x = pct, y = cat)) +
    geom_col(fill = "#2e7d9a", width = 0.7) +
    geom_text(aes(label = sprintf("%.1f %%", pct)), hjust = -0.15, size = 3, colour = "#1f2937") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
    labs(title = "Distribución de frecuencias", x = "Porcentaje de anuncios", y = etiqueta) + tema_u()
  if (!pastel) { print(p1); return(invisible(NULL)) }
  p2 <- ggplot(d, aes(x = "", y = n, fill = cat)) +
    geom_col(width = 1, colour = "white") + coord_polar("y") +
    labs(title = "Composición porcentual", x = NULL, y = NULL) +
    theme_minimal(base_size = 11.5) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(colour = "#1a3a5c", face = "bold", size = 11.5),
          legend.title = element_blank(), legend.text = element_text(size = 9),
          legend.position = "right", axis.text = element_blank(), panel.grid = element_blank())
  print(p1); print(p2)
  invisible(NULL)
}
#
#
#
#
#
#
#
#
#| label: cuali-Type
#| echo: true
#| tbl-cap: "Distribución de frecuencias — Especie"
tabla_frecuencias(datos$Type, "Especie")
#
#
#
#
#
#| label: graf-cuali-Type
#| echo: true
#| layout-ncol: 2
graficos_cuali(datos$Type, "Especie", pastel = TRUE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-Gender
#| echo: true
#| tbl-cap: "Distribución de frecuencias — Género"
tabla_frecuencias(datos$Gender, "Género")
#
#
#
#
#
#| label: graf-cuali-Gender
#| echo: true
#| layout-ncol: 2
graficos_cuali(datos$Gender, "Género", pastel = TRUE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-MaturitySize
#| echo: true
#| tbl-cap: "Distribución de frecuencias — Tamaño de madurez"
tabla_frecuencias(datos$MaturitySize, "Tamaño de madurez")
#
#
#
#
#
#| label: graf-cuali-MaturitySize
#| echo: true
#| layout-ncol: 2
graficos_cuali(datos$MaturitySize, "Tamaño de madurez", pastel = TRUE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-FurLength
#| echo: true
#| tbl-cap: "Distribución de frecuencias — Longitud del pelaje"
tabla_frecuencias(datos$FurLength, "Longitud del pelaje")
#
#
#
#
#
#| label: graf-cuali-FurLength
#| echo: true
#| layout-ncol: 2
graficos_cuali(datos$FurLength, "Longitud del pelaje", pastel = TRUE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-Vaccinated
#| echo: true
#| tbl-cap: "Distribución de frecuencias — Vacunado"
tabla_frecuencias(datos$Vaccinated, "Vacunado")
#
#
#
#
#
#| label: graf-cuali-Vaccinated
#| echo: true
#| layout-ncol: 2
graficos_cuali(datos$Vaccinated, "Vacunado", pastel = TRUE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-Dewormed
#| echo: true
#| tbl-cap: "Distribución de frecuencias — Desparasitado"
tabla_frecuencias(datos$Dewormed, "Desparasitado")
#
#
#
#
#
#| label: graf-cuali-Dewormed
#| echo: true
#| layout-ncol: 2
graficos_cuali(datos$Dewormed, "Desparasitado", pastel = TRUE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-Sterilized
#| echo: true
#| tbl-cap: "Distribución de frecuencias — Esterilizado"
tabla_frecuencias(datos$Sterilized, "Esterilizado")
#
#
#
#
#
#| label: graf-cuali-Sterilized
#| echo: true
#| layout-ncol: 2
graficos_cuali(datos$Sterilized, "Esterilizado", pastel = TRUE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-Health
#| echo: true
#| tbl-cap: "Distribución de frecuencias — Estado de salud"
tabla_frecuencias(datos$Health, "Estado de salud")
#
#
#
#
#
#| label: graf-cuali-Health
#| echo: true
#| layout-ncol: 2
graficos_cuali(datos$Health, "Estado de salud", pastel = TRUE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-AdoptionSpeed
#| echo: true
#| tbl-cap: "Distribución de frecuencias — Velocidad de adopción"
tabla_frecuencias(datos$AdoptionSpeed, "Velocidad de adopción")
#
#
#
#
#
#| label: graf-cuali-AdoptionSpeed
#| echo: true
#| layout-ncol: 2
graficos_cuali(datos$AdoptionSpeed, "Velocidad de adopción", pastel = TRUE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-Color1
#| echo: true
#| tbl-cap: "Distribución de frecuencias — Color principal"
tabla_frecuencias(datos$Color1, "Color principal")
#
#
#
#
#
#| label: graf-cuali-Color1
#| echo: true
#| layout-ncol: 2
graficos_cuali(datos$Color1, "Color principal", pastel = TRUE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-Color2
#| echo: true
#| tbl-cap: "Distribución de frecuencias — Color secundario"
tabla_frecuencias(datos$Color2, "Color secundario")
#
#
#
#
#
#| label: graf-cuali-Color2
#| echo: true
#| layout-ncol: 2
graficos_cuali(datos$Color2, "Color secundario", pastel = TRUE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-Color3
#| echo: true
#| tbl-cap: "Distribución de frecuencias — Color terciario"
tabla_frecuencias(datos$Color3, "Color terciario")
#
#
#
#
#
#| label: graf-cuali-Color3
#| echo: true
#| layout-ncol: 2
graficos_cuali(datos$Color3, "Color terciario", pastel = TRUE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-Breed1
#| echo: true
#| tbl-cap: "Razas principales"
tabla_frecuencias(datos$Breed1, "Raza principal", n_top = 10)
#
#
#
#
#
#| label: graf-cuali-Breed1
#| echo: true
graficos_cuali(datos$Breed1, "Raza principal", pastel = FALSE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-Breed2
#| echo: true
#| tbl-cap: "Razas secundarias"
tabla_frecuencias(datos$Breed2, "Raza secundaria", n_top = 10)
#
#
#
#
#
#
#| label: graf-cuali-Breed2
#| echo: true
graficos_cuali(datos$Breed2, "Raza secundaria", pastel = FALSE, n_top = 10)
#
#
#
#
#
#
#
#
#| label: cuali-State
#| echo: true
#| tbl-cap: "Estados / regiones"
tabla_frecuencias(datos$State, "Estado / región", n_top = 10)
#
#
#
#
#
#| label: graf-cuali-State
#| echo: true
graficos_cuali(datos$State, "Estado / región", pastel = FALSE, n_top = 10)
#
#
#
#
#
#
#
#
#
#
#| label: cuali-Name
#| echo: true
#| tbl-cap: "Nombres más frecuentes"
tabla_frecuencias(datos$Name, "Nombre", n_top = 10)
#
#
#
#
#
#| label: graf-cuali-Name
#| echo: true
tabla_cardinalidad(datos$Name)
#
#
#
#
#
#
#
#
#| label: card-Description
#| echo: true
tabla_cardinalidad(datos$Description)
#
#
#
#
#
#
#
#
#| label: card-PetID
#| echo: true
tabla_cardinalidad(datos$PetID)
#
#
#
#
#
#
#
#
#| label: card-RescuerID
#| echo: true
tabla_cardinalidad(datos$RescuerID)
#
#
#
#
#
