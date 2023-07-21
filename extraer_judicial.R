library(tidyverse)
library(pdftools)
library (stringr)
library(purrr)
library(rlist)


web_13 <- "C:/Users/Usuario/Dropbox/OVcM/BASES_ANUALES/Mapa genero/Originales/judicial/2013.pdf"
web_14 <- "C:/Users/Usuario/Dropbox/OVcM/BASES_ANUALES/Mapa genero/Originales/judicial/2014.pdf"
web_15 <- "C:/Users/Usuario/Dropbox/OVcM/BASES_ANUALES/Mapa genero/Originales/judicial/2015.pdf"
web_16 <- "C:/Users/Usuario/Dropbox/OVcM/BASES_ANUALES/Mapa genero/Originales/judicial/2016.pdf"
web_17 <- "C:/Users/Usuario/Dropbox/OVcM/BASES_ANUALES/Mapa genero/Originales/judicial/2017.pdf"
web_18 <- "C:/Users/Usuario/Dropbox/OVcM/BASES_ANUALES/Mapa genero/Originales/judicial/2018.pdf"
web_19 <- "C:/Users/Usuario/Dropbox/OVcM/BASES_ANUALES/Mapa genero/Originales/judicial/2019.pdf"
web_20 <- "C:/Users/Usuario/Dropbox/OVcM/BASES_ANUALES/Mapa genero/Originales/judicial/2020.pdf"
web_21 <- "C:/Users/Usuario/Dropbox/OVcM/BASES_ANUALES/Mapa genero/Originales/judicial/2021.pdf"
web_22 <- "C:/Users/Usuario/Dropbox/OVcM/BASES_ANUALES/Mapa genero/Originales/judicial/2022.pdf" 


nomina <-pdf_text(web_13) %>% str_split("\n")
nomina <-pdf_text(web_14) %>% str_split("\n")
nomina <-pdf_text(web_15) %>% str_split("\n")
nomina <-pdf_text(web_16) %>% str_split("\n")
nomina <-pdf_text(web_17) %>% str_split("\n")
nomina <-pdf_text(web_18) %>% str_split("\n")
nomina <-pdf_text(web_19) %>% str_split("\n")
nomina <-pdf_text(web_20) %>% str_split("\n")
nomina <-pdf_text(web_21) %>% str_split("\n")
nomina <-pdf_text(web_22) %>% str_split("\n")

#elimino paginas
nomina[[1]] <- nomina[[1]][-c(1:7)]
nomina[[1]] <- nomina[[1]][-c(28:30)]
nomina[[1]] <- nomina[[1]][-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)]

#armo BD
judicial_22 <- data.frame(matrix(unlist(nomina))) %>% 
  separate(col = 1, into = c("CARGO","Mujeres","Varones","Total","MujeresP","VaronesP"),
           sep = "(?=\\s[[:digit:]])") %>% 
  select("CARGO","Mujeres","Varones") %>% 
  filter(CARGO != str_remove(CARGO,"[:lower:]")) %>% 
  mutate(CARGO = str_remove_all(CARGO,"[:digit:]")) %>% 
  mutate(Mujeres = as.integer(Mujeres)) %>% 
  mutate(Varones = as.integer(Varones)) %>% 
  mutate(Mujeres = str_trim(Mujeres)) %>% 
  mutate(Varones = str_trim(Varones)) %>% 
  mutate(CARGO = str_trim(CARGO)) %>% 
  mutate(Año = paste(2022))

#Consolido todo los años- arreglo errores de categorias
judicial <- bind_rows(judicial_13,judicial_14,judicial_15,judicial_16,judicial_17,judicial_18,judicial_19,
                      judicial_20,judicial_21,judicial_22) %>% 
  mutate(Categoria = case_when(CARGO == "Ministras/os" ~ "Magistrados",
                               CARGO == "Camaristas"  ~ "Magistrados",
                               CARGO == "Juezas/ces" ~ "Magistrados",
                               CARGO == "Juezas/ces de Paz" ~ "Magistrados",
                               CARGO == "Secretarias/os de Corte" ~ "FUNCIONARIAS/OS",
                               CARGO == "Secretarias/os de Cámara (da instancia)" ~ "FUNCIONARIAS/OS",
                               CARGO == "Secretarias/os de Juzgado (ra instancia)" ~ "FUNCIONARIAS/OS",
                               CARGO == "Secretarias/os de Juzgado de Paz" ~ "FUNCIONARIAS/OS",
                               CARGO == "Otras/os Funcionarias/os" ~ "FUNCIONARIAS/OS",
                               CARGO == "Máxima categoría" ~ "PERSONAL ADMINISTRATIVO",
                               CARGO == "Otras categorías" ~ "PERSONAL ADMINISTRATIVO",
                               CARGO == "Personal de Servicio" ~ "PERSONAL DE SERVICIO")) %>% 
  mutate(Categoria =str_to_lower(Categoria)) %>% 
  mutate(CARGO = case_when(CARGO == "Ministras/os" ~ "ministerio",
                           CARGO == "Camaristas"  ~ "camaras",
                           CARGO == "Juezas/ces" ~ "juzgados",
                           CARGO == "Juezas/ces de Paz" ~ "juzgados de paz",
                           CARGO == "Secretarias/os de Corte" ~ "secretarias",
                           CARGO == "Secretarias/os de Cámara (da instancia)" ~ "secretarias (segunda)",
                           CARGO == "Secretarias/os de Juzgado (ra instancia)" ~ "secretarias (primera)",
                           CARGO == "Secretarias/os de Juzgado de Paz" ~ "secretarias (juzgados de paz)",
                           CARGO == "Otras/os Funcionarias/os" ~ "otros funcionarios",
                           CARGO == "Máxima categoría" ~ "maxima categoria",
                           CARGO == "Otras categorías" ~ "otras categorias",
                           CARGO == "Personal de Servicio" ~ "personal de servicio"))
           

write.csv2(judicial, "judicial.csv")

