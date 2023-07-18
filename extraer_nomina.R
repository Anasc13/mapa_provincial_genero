library(tidyverse)
library(pdftools)
library (stringr)
library(tabulizer)
library(purrr)
library(jsonlite)
library(rlist)
library(rvest)

web <- "https://www.salta.gob.ar/public/descargas/nomina/Nomina-Provincial-de-Autoridades-Gobierno-Salta.pdf"
nomina <-  pdf_text(web) %>% str_split("\n")

#elimino paginas
head(nomina[[]], 66)
nomina[[1]] <- NULL
nomina <- nomina[-c(1:5)]
nomina <- nomina[-c(48:100)]

tabla <- data.frame(matrix(unlist(nomina)))
write.csv2(tabla, "data.csv")

###intento de funcion
nombres <- read.csv2(file = "nombres.csv", sep = ";", header = T)
nombres$Nombres <- str_to_upper(nombres$Nombres)
head(nombres)

#ministros
ministros <- read.csv2(file = "data.csv", sep = ";", header = T)
cargos <- left_join (ministros, nombres, by="Nombres")

#gobernacion vice
write.csv2(tabla, "data.csv")

regmatches(tabla, gregexpr(pattern, tabla))

str_extract(tabla,"^Gobernador[\\nSAENZ$]")

gobernacion <-  nomina[[1]][c(9,10)]
vicegobernacion <-  nomina[[10]][c(7,8)]

pattern <- c("SECRETARÍA")

grep(pattern, tabla)


