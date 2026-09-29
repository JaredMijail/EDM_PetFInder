
suppressMessages(library(tidyverse))
options(OutDec = ",")
datos <- readr::read_csv(here::here("data","petfinder_enriquecido_final.csv"), show_col_types = FALSE)

skew <- function(x){x<-x[!is.na(x)];n<-length(x);m<-mean(x);s<-sd(x);n/((n-1)*(n-2))*sum(((x-m)/s)^3)}
kurt <- function(x){x<-x[!is.na(x)];n<-length(x);m<-mean(x);s<-sd(x);n*(n+1)/((n-1)*(n-2)*(n-3))*sum(((x-m)/s)^4)-3*(n-1)^2/((n-2)*(n-3))}
moda <- function(x){x<-x[!is.na(x)];ux<-unique(x);ux[which.max(tabulate(match(x,ux)))]}
f <- function(x,d=2) format(round(x,d), big.mark=".", decimal.mark=",", nsmall=d, scientific=FALSE)

num <- c("Age","Quantity","Fee","PhotoAmt","VideoAmt","sentiment_score","sentiment_magnitude",
         "total_labels","total_faces","total_colors","desc_char_len","desc_word_count")
cat("### CUANTITATIVAS\n")
for (v in num) {
  x <- as.numeric(datos[[v]]); q <- quantile(x, c(.25,.75), na.rm=TRUE); iqr <- diff(q)
  z <- (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
  cat(sprintf("%-19s n=%5d na=%4d  mean=%9s med=%9s moda=%8s min=%8s max=%9s Q1=%8s Q3=%9s cv=%6s skew=%7s kurt=%8s outIQR=%5d(%.1f%%) outZ=%4d  ceros=%.1f%%\n",
    v, sum(!is.na(x)), sum(is.na(x)), f(mean(x,na.rm=TRUE)), f(median(x,na.rm=TRUE)), f(as.numeric(moda(x))),
    f(min(x,na.rm=TRUE)), f(max(x,na.rm=TRUE)), f(q[1]), f(q[2]),
    f(100*sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE),1), f(skew(x),2), f(kurt(x),2),
    sum(x<q[1]-1.5*iqr | x>q[2]+1.5*iqr, na.rm=TRUE), 100*mean(x<q[1]-1.5*iqr | x>q[2]+1.5*iqr, na.rm=TRUE),
    sum(abs(z)>3, na.rm=TRUE), 100*mean(x==0, na.rm=TRUE)))
}

cat("\n### CUALITATIVAS\n")
cat_vars <- c("PetID","Name","Type","Breed1","Breed2","Gender","Color1","Color2","Color3","MaturitySize",
              "FurLength","Vaccinated","Dewormed","Sterilized","Health","Quantity","State","RescuerID",
              "Description","AdoptionSpeed")
for (v in cat_vars) {
  x <- as.character(datos[[v]]); tb <- sort(table(x, useNA="no"), decreasing=TRUE)
  n <- length(x); nu <- length(tb); pm <- tb[which.max(tb)]
  raros <- sum(tb/n < 0.01)
  cat(sprintf("%-15s n=%5d unicos=%6d moda=%-10s (%5.1f%%) raras(<1%%)=%4d  top5=%s\n",
      v, n, nu, substr(names(tb)[1],1,10), 100*pm/n, raros,
      paste(sprintf("%s:%.1f%%", substr(names(tb)[1:5],1,12), 100*tb[1:5]/n), collapse=" ")))
}
cat("\n### ordinales ordenados\n")
for (v in c("AdoptionSpeed","MaturitySize","FurLength","Health","Vaccinated","Sterilized","Dewormed","Gender","Type")) {
  print(table(datos[[v]], useNA="ifany"))
}
cat("\n### faltantes\n"); print(colSums(is.na(datos)))
cat("\n### duplicados: filas", sum(duplicated(datos)), "| PetID", sum(duplicated(datos$PetID)), "\n")
cat("\n### dim:", nrow(datos), "x", ncol(datos), "| memoria:", format(object.size(datos), units="MB"), "\n")
print(rownames(installed.packages())[rownames(installed.packages()) %in% c("e1071","patchwork","gridExtra","gt","moments","cowplot","ggpubr","scales","knitr","kableExtra")])
