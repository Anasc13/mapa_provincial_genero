library(pdftools)
library (stringr)
library(tabulizer)
library(purrr)
library(jsonlite)
library(rlist)
library(rvest)

web <- "https://www.salta.gob.ar/public/descargas/nomina/Nomina-Provincial-de-Autoridades-Gobierno-Salta.pdf"
nomina <-  pdf_text(web) %>% split_pdf("\n")

#elimino paginas
nomina[[1]] <- NULL
nomina <- nomina[-c(1:5)]
nomina <- nomina[-c(48:100)]

data <- write.csv2(nomina, "data.csv")

###intento de funcion
ministro <- function(archivo) {  
  resultado_1 <- rlist::list.search(archivo,grepl('MINISTERIO', .),'character')
  return(resultado_1)
}
ministro(nomina)

length(tabla) # longitud de la lista
longitud <- lengths(tabla) # longitud de cada elemento de la lista

for (j in 1:longitud){
  ministerio <- str_extract_all(tabla, "MINISTERIO DE", tabla[[j]])
}


tabla <- data.frame(matrix(unlist(nomina)))
ministerio <- str_extract(tabla, "MINISTERIO DE ")

data <- write.csv2(tabla, "data.csv")

#resultado_2 <- rlist::list.select(archivo, 'Ministro')
#lista <- list(p1= list(nombre=resultado_1), 
#              p2=list(nombre=resultado_2))


head(nomina[[]], 66)

gobernacion <-  nomina[[1]][c(9,10)]
vicegobernacion <-  nomina[[10]][c(7,8)]

ministerios <-  str_subset(tabla, "^\\MINISTERIO DE")
varones <- str_subset(aux, "^\\Secretario\\:")
mujeres <-  str_subset(aux, "^\\Secretaria\\:")


