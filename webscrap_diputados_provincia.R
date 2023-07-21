library(rvest)
library(tidyverse)
library(xml2)

webpage <- read_html("https://www.diputadosalta.gob.ar/web/diputados")
results <- html_table(webpage)
class(results)

diputados <- data.frame(results) %>%
  select(-("Foto")) %>% 
  rename("nombre_apellido"="Apellido.y.Nombre", "Periodo_actual"= "Período.actual") %>%
  separate(nombre_apellido,into= c("Apellidos", "Nombres"),  sep = ",", remove=T) %>% 
  mutate(Nombres=str_to_upper(Nombres)) %>% 
  mutate(Nombres=chartr("ÁÉÍÓÚ", "AEIOU", toupper(Nombres))) %>% 
  mutate(Bloque=str_to_upper(Bloque)) %>% 
  mutate(Bloque=chartr("ÁÉÍÓÚ", "AEIOU", toupper(Bloque))) %>% 
  mutate(Departamento=str_to_upper(Departamento)) %>% 
  mutate(Departamento=chartr("ÁÉÍÓÚ", "AEIOU", toupper(Departamento))) %>% 
  separate(Nombres,into= c("Nombres_1","Nombres","Nombres_2"),  sep = " ", remove=T) %>% 
  select(-("Nombres_1"))

#AGREGO BASE NOMBRES PARA AGREGAR COLUMNA SEXO
nombres <- read.csv2(file = "nombres.csv", sep = ";", header = T)
nombres$Nombres <- str_to_upper(nombres$Nombres)
head(nombres)

diputados_sexo <- left_join (diputados, nombres, by="Nombres") %>% 
  select(-("Cod"))

diputados_sexo[1,7] <- "M"
diputados_sexo[21,7] <- "F"
diputados_sexo[20,7] <- "F"
diputados_sexo[32,7] <- "F"
diputados_sexo[49,7] <- "F"
diputados_sexo[59,7] <- "F"

table(diputados_sexo$Sexo)
write.csv2(diputados_sexo, "diputados_sexo.csv")
