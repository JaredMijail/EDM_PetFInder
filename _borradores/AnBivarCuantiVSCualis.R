# Ambiente
library(readr)
library(dplyr)
library(plotly)
library(knitr)
library(FactoClass)

# Dataset PetFinder enriquecido
petfinder_enriquecido_final <- read_csv("Documents/UN/2026-2S/EDM/petfinder_enriquecido_final.csv")
colnames(petfinder_enriquecido_final)

# Variables cualitativas
cualitativas <- c('Type', 'Name', 'Breed1', 'Breed2', 'Gender', 'Color1', 'Color2', 'Color3', 'MaturitySize', 'FurLength', 'Vaccinated', 'Dewormed', 'Sterilized', 'Health', 'State', 'RescuerID', 'AdoptionSpeed')
length(cualitativas)

# Variable cuantitativa de interés
Age <- petfinder_enriquecido_final$Age

# Dataset variables cualitativas
petfinder_cuali <- petfinder_enriquecido_final %>% select(all_of(cualitativas))

# Dataset uniendo variables cuantitativas y variable cualitativa de interés
petfinder_cualis_cuanti <- petfinder_cuali |> mutate(Age = Age)

bivariado <- function(cualitativa,
                      datos = petfinder_cualis_cuanti,
                      variable = 'Age') {
  
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
  
  # Razón de correlación (porcentaje)
  X    <- data.frame(x = x) |> setNames(variable)
  corr <- centroids(X, f)$cr * 100
  
  # Boxplot (niveles ordenados por mediana)
  f_med <- reorder(f, x, median)
  fig1 <- plot_ly(df, x = ~x, y = ~f_med, color = ~f_med, type = "box",
                  boxpoints = FALSE, orientation = "h") %>%
    layout(xaxis = list(title = variable),
           yaxis = list(title = paste("sorted", cualitativa)),
           showlegend = FALSE)
  
  # Violín (niveles ordenados por media)
  f_mean <- reorder(f, x, mean)
  fig2 <- plot_ly(df, x = ~x, y = ~f_mean, color = ~f_mean, type = "violin",
                  orientation = "h",
                  points = "all", pointpos = 0,
                  meanline = list(visible = TRUE, color = "black")) %>%
    layout(xaxis = list(title = variable),
           yaxis = list(title = paste("sorted", cualitativa)),
           showlegend = FALSE)
  
  # Panel combinado con la razón de correlación en el título
  subplot(fig1, fig2, nrows = 1, shareX = TRUE, shareY = FALSE,
          titleX = TRUE, titleY = TRUE, margin = 0.05) %>%
    layout(
      title = list(
        text = sprintf("%s vs %s &nbsp;|&nbsp; Razón de correlación: %.2f%%",
                       variable, cualitativa, corr),
        x = 0.5
      ),
      margin = list(t = 80)
    )
}
