library(rvest)
library(tidyverse)
library(xml2)

webpage <- read_html("https://buenosaires.gob.ar/areas/registrocivil/nombres/busqueda/imprimir.php")

results <- html_table(webpage)
class(results)

nombres <- data.frame(results) %>%
  rename("Sexo" = "Nombres.1", "circular"="Nombres.2" ,"origen"="Nombres.3") %>%
  filter(Nombres!="Nombre") %>% 
  select(-(circular),-(origen))

write.csv2(nombres, "nombres.csv")
