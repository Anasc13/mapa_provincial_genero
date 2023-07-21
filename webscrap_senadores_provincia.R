library(rvest)
library(tidyverse)
library(xml2)

webpage <- read_html("https://senadosalta.gob.ar/senadores/nomina-de-senadores-alfabetico/")

results <- html_table(webpage)
class(results)

sendores <- data.frame(results) %>%
  rename("Orden" = "X1", "nombre_apellido"="X2" ,"Departamento"="X3") %>%
  drop_na() %>% 
  separate(nombre_apellido,into= c("Apellidos", "Nombres"),  sep = ",", remove=T) %>% 
  mutate (Departamento=str_to_upper(Departamento)) %>% 
  mutate(Departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Departamento))) %>% 
  mutate (Nombres=str_to_upper(Nombres)) %>% 
  mutate(Nombres = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Nombres))) %>% 
  separate(Nombres,into= c("Nombres_1","Nombres"),  sep = " ", remove=T) %>% 
  select(-("Nombres_1"))
  
#AGREGO BASE NOMBRES PARA AGREGAR COLUMNA SEXO
nombres <- read.csv2(file = "nombres.csv", sep = ";", header = T)
nombres$Nombres <- str_to_upper(nombres$Nombres)
head(nombres)

senadores_sexo <- left_join (sendores, nombres, by="Nombres") %>% 
  select(-("Cod"))

senadores_sexo[14,5] <- "M"
senadores_sexo[11,5] <- "M"

table(senadores_sexo$Sexo)
write.csv2(senadores_sexo, "senadores_sexo.csv")
  
