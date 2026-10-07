# Librerías usadas en esta sección 
library(tidyverse)   # dplyr, ggplot2, readr, purrr, tibble, stringr, forcats, tidyr
library(here)        # rutas relativas al proyecto
library(knitr)       # kable() para tablas

# Carga y traducción de los niveles de las variables cualitativas del conjunto de datos
options(OutDec = ",")   # separador decimal en español
knitr::opts_chunk$set(fig.width = 4.6, fig.height = 3.3, dpi = 120, out.width = "100%")

datos <- readr::read_csv(
  here::here("data", "petfinder_enriquecido_final.csv"),
  show_col_types = FALSE
) |>
  mutate(
    Type          = factor(Type, levels = c("Dog", "Cat"), labels = c("Perro", "Gato")),
    Gender        = factor(Gender, levels = c("Male", "Female", "Mixed"),
                           labels = c("Macho", "Hembra", "Mixto")),
    MaturitySize  = factor(MaturitySize, levels = c("Small", "Medium", "Large", "XLarge"),
                           labels = c("Pequeño", "Mediano", "Grande", "Extra grande")),
    FurLength     = factor(FurLength, levels = c("Short", "Medium", "Long"),
                           labels = c("Corto", "Mediano", "Largo")),
    Vaccinated    = factor(Vaccinated, levels = c("Yes", "No", "NotSure"),
                           labels = c("Sí", "No", "No está seguro")),
    Dewormed      = factor(Dewormed, levels = c("Yes", "No", "NotSure"),
                           labels = c("Sí", "No", "No está seguro")),
    Sterilized    = factor(Sterilized, levels = c("Yes", "No", "NotSure"),
                           labels = c("Sí", "No", "No está seguro")),
    Health        = factor(Health, levels = c("Healthy", "MinorInjury", "SeriousInjury"),
                           labels = c("Sano", "Lesión leve", "Lesión grave")),
    AdoptionSpeed = factor(AdoptionSpeed,
                           levels = c("SameDay", "1-7d", "8-30d", "31-90d", "NoAdoption"),
                           labels = c("Mismo día", "1–7 días", "8–30 días",
                                      "31–90 días", "Sin adopción"))
  )

VARS_NUM <- c("Age", "Quantity", "Fee", "PhotoAmt", "VideoAmt", "sentiment_score",
              "sentiment_magnitude", "total_labels", "total_faces", "total_colors",
              "desc_char_len", "desc_word_count")
VARS_CAT <- c("Type", "Gender", "MaturitySize", "FurLength", "Vaccinated", "Dewormed",
              "Sterilized", "Health", "AdoptionSpeed", "Color1", "Color2", "Color3",
              "Breed1", "Breed2", "State", "Name", "Description", "PetID", "RescuerID")

# --- formateo de cifras en español ---
fmt <- function(x, d = 2) {
  x <- as.numeric(x)
  if (all(abs(x - round(x)) < 1e-9)) {
    return(format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE))
  }
  format(round(x, d), big.mark = ".", decimal.mark = ",", nsmall = d, scientific = FALSE)
}
ent <- function(x) format(as.integer(round(as.numeric(x))), big.mark = ".", decimal.mark = ",")
pct <- function(x, d = 1) gsub("\\.", ",", sprintf(paste0("%.", d, "f %%"), x))

# --- tema de gráficos ---
tema_u <- function() {
  theme_minimal(base_size = 11.5) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(colour = "#1a3a5c", face = "bold", size = 11.5),
          axis.title = element_text(colour = "#4b5563", size = 10),
          panel.grid.minor = element_blank())
}