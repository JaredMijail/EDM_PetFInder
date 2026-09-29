
suppressMessages(library(tidyverse)); options(OutDec=",")
d <- readr::read_csv(here::here("data","petfinder_enriquecido_final.csv"), show_col_types = FALSE)
cat("valores de etiquetas:\n"); for (v in c("Type","Gender","MaturitySize","FurLength","Vaccinated","Dewormed","Sterilized","Health","AdoptionSpeed")) cat(" ", v, "->", paste(unique(d[[v]]), collapse=" | "), "\n")
cat("\nrelatos sin sentimiento -> n:", sum(is.na(d$sentiment_score)),
    "| desc_char_len en esos casos:", paste(round(summary(d$desc_char_len[is.na(d$sentiment_score)]),1), collapse=" / "), "\n")
cat("correlacion longitud-sentimiento (solo reporte interno):", round(cor(d$desc_char_len, d$sentiment_magnitude, use="complete.obs"),3), "\n")
for (v in c("Breed1","Breed2","Color1","Color2","Color3","State","Name","RescuerID","Description")) {
  tb <- sort(table(as.character(d[[v]])), decreasing = TRUE)
  n <- length(tb); tot <- sum(tb)
  top10 <- sum(tb[1:min(10,n)])
  raras <- names(tb)[tb/tot < 0.01]
  cat(sprintf("%-12s niveles=%4d  top10=%.1f%%  raras(<1%%)=%3d (%.1f%% de los anuncios)  max=%s (%.1f%%)\n",
      v, n, 100*top10/tot, length(raras), 100*sum(tb[raras])/tot, names(tb)[1], 100*tb[1]/tot))
}
cat("\nName vacios:", sum(d$Name==""), "| unicos:", length(unique(d$Name)), "\n")
cat("AdoptionSpeed:", paste(sprintf("%s=%.1f%%", names(table(d$AdoptionSpeed)), 100*prop.table(table(d$AdoptionSpeed))), collapse=" "), "\n")
cat("Color1:", paste(sprintf("%s=%.1f%%", names(table(d$Color1)), 100*prop.table(table(d$Color1))), collapse=" "), "\n")
cat("Color2:", paste(sprintf("%s=%.1f%%", names(table(d$Color2)), 100*prop.table(table(d$Color2))), collapse=" "), "\n")
cat("Color3:", paste(sprintf("%s=%.1f%%", names(table(d$Color3)), 100*prop.table(table(d$Color3))), collapse=" "), "\n")
cat("MaturitySize:", paste(sprintf("%s=%.1f%%", names(table(d$MaturitySize)), 100*prop.table(table(d$MaturitySize))), collapse=" "), "\n")
cat("FurLength:", paste(sprintf("%s=%.1f%%", names(table(d$FurLength)), 100*prop.table(table(d$FurLength))), collapse=" "), "\n")
cat("Health:", paste(sprintf("%s=%.1f%%", names(table(d$Health)), 100*prop.table(table(d$Health))), collapse=" "), "\n")
cat("total_faces max:", max(d$total_faces), "| total_labels rango:", range(d$total_labels), "| total_colors rango:", range(d$total_colors), "\n")
cat("PhotoAmt:", paste(sprintf("%d=%.1f%%", as.integer(names(table(d$PhotoAmt)))[1:8], 100*prop.table(table(d$PhotoAmt))[1:8]), collapse=" "), "\n")
cat("Quantity:", paste(sprintf("%d=%.1f%%", as.integer(names(table(d$Quantity)))[1:6], 100*prop.table(table(d$Quantity))[1:6]), collapse=" "), "\n")
cat("Fee >0:", sprintf("%.1f%%", 100*mean(d$Fee>0)), "| Fee mediana>0:", median(d$Fee[d$Fee>0]), "| max:", max(d$Fee), "\n")
cat("VideoAmt>0:", sprintf("%.1f%%", 100*mean(d$VideoAmt>0)), "| max:", max(d$VideoAmt), "\n")
cat("Age: 0-6m", sprintf("%.1f%%", 100*mean(d$Age<=6)), "| >24m", sprintf("%.1f%%", 100*mean(d$Age>24)), "\n")
