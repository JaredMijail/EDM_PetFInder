# Ambiente
library(dplyr)
library(plotly)
library(knitr)
library(FactoClass)

# Dataset PetFinder enriquecido
petfinder_enriquecido_final <- read_csv("Documents/UN/2026-2S/EDM/petfinder_enriquecido_final.csv")
colnames(petfinder_enriquecido_final)

# Variables cuantitativas
cuantitativas <- c('Age', 'Quantity', 'Fee', 'VideoAmt', 'PhotoAmt', 'sentiment_score', 'sentiment_magnitude', 'total_labels', 'total_faces', 'total_colors', 'desc_char_len', 'desc_word_count')
length(cuantitativas)

# Variable cualitativa de interes
AdoptionSpeed <- petfinder_enriquecido_final$AdoptionSpeed

# Dataset variables cuantitativas
petfinder_cuanti <- petfinder_enriquecido_final %>% select(all_of(cuantitativas))

# Dataset uniendo variables cuantitativas y variable cualitativa de interés
petfinder_cuantis_cuali <- petfinder_cuanti |> mutate(AdoptionSpeed = AdoptionSpeed)

bivariado <- function(variable,
                      datos = petfinder_cuantis_cuali,
                      cualitativa = "AdoptionSpeed") {
  
  # Validación de nombres
  if (!variable %in% names(datos))
    stop("La variable '", variable, "' no está en los datos.")
  if (!cualitativa %in% names(datos))
    stop("La variable cualitativa '", cualitativa, "' no está en los datos.")
  
  # Casos completos para la pareja (variable, factor)
  f0 <- as.factor(datos[[cualitativa]])
  ok <- complete.cases(datos[[variable]], f0)
  x  <- datos[[variable]][ok]
  f  <- droplevels(f0[ok])
  df <- data.frame(x = x, f = f)
  
  # fig1: diagrama de cajas (niveles ordenados por mediana).
  # Se dibuja en un dispositivo nulo para guardarlo sin mostrarlo.
  f_med <- reorder(f, x, median)
  pdf(NULL)
  fig1 <- plot_ly(df, x = ~x, color = f_med, type = "box",
                  boxpoints = FALSE) %>%
    layout(xaxis = list(title = variable),
           yaxis = list(title = paste("sorted", cualitativa)),
           showlegend = FALSE)
  
  # fig2: gráfico de violín con puntos y media (niveles ordenados por media)
  f_mean <- reorder(f, x, mean)
  fig2 <- plot_ly(df, x = ~x, split = f_mean, type = "violin",
                  points = "all", pointpos = 0,
                  meanline = list(visible = TRUE, color = "black")) %>%
    layout(xaxis = list(title = variable),
           yaxis = list(title = paste("sorted", cualitativa)))
  
  # corr: razón de correlación (porcentaje)
  X    <- data.frame(x = x) |> setNames(variable)
  corr <- centroids(X, f)$cr * 100
  
  invisible(list(fig1 = fig1, fig2 = fig2, corr = corr))
}

res <- bivariado("Quantity")

res$fig1    # boxplot
res$fig2    # gráfico de violín
res$corr    # razón de correlación (%)
