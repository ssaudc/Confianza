#Carga de bases de WVS del año 2013 y 2018 de Ecuador

#Carga de librerias

library(haven)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(lubridate)
library(tidyverse)
library(here)
library(cowplot)
library(patchwork)
library(openxlsx)
library(foreign)

#Carga de bases de WVS

Wave_7_Ecuador <- read_dta("BASES LIDE/WVS_Wave_7_Ecuador_Stata_v5.0.dta")
Wave_6_Ecuador<- read_dta("BASES LIDE/WV6_Data_Ecuador_Stata_v20201117.dta")

#-------------------------------------------------------------------------

# Union de bases de datos

Wave_6_Ecuador$N_REGION_ISO <- as_factor(Wave_6_Ecuador$N_REGION_ISO)
Wave_7_Ecuador$N_REGION_ISO <- as_factor(Wave_7_Ecuador$N_REGION_ISO)
Wave_6_Ecuador$V102 <- as_factor(Wave_6_Ecuador$V102)
Wave_7_Ecuador$Q58 <- as_factor(Wave_7_Ecuador$Q58)
Wave_6_Ecuador$V103 <- as_factor(Wave_6_Ecuador$V103)
Wave_7_Ecuador$Q59 <- as_factor(Wave_7_Ecuador$Q59)
Wave_6_Ecuador$V104 <- as_factor(Wave_6_Ecuador$V104)
Wave_7_Ecuador$Q60 <- as_factor(Wave_7_Ecuador$Q60)
Wave_6_Ecuador$V105 <- as_factor(Wave_6_Ecuador$V105)
Wave_7_Ecuador$Q61 <- as_factor(Wave_7_Ecuador$Q61)
Wave_6_Ecuador$V106 <- as_factor(Wave_6_Ecuador$V106)
Wave_7_Ecuador$Q62 <- as_factor(Wave_7_Ecuador$Q62)
Wave_6_Ecuador$V115 <- as_factor(Wave_6_Ecuador$V115)
Wave_7_Ecuador$Q71 <- as_factor(Wave_7_Ecuador$Q71)
Wave_6_Ecuador$V181 <- as_factor(Wave_6_Ecuador$V181)
Wave_7_Ecuador$Q142 <- as_factor(Wave_7_Ecuador$Q142)
Wave_6_Ecuador$V113 <- as_factor(Wave_6_Ecuador$V113)
Wave_7_Ecuador$Q69 <- as_factor(Wave_7_Ecuador$Q69)

#Selección de preguntas que se van a utilizar para analizar el nivel de confianza en las diferentes instituciones

wave7 <- Wave_7_Ecuador%>%
  select(cf= "Q58",
         "reg"= N_REGION_ISO,
         cv="Q59",
         cp="Q60",
         c_primera="Q61",
         c_dif_reg="Q62",
         ano="A_YEAR",
         c_gob= "Q71",
         p_trabajo="Q142",
         c_policia="Q69")

wave6<-Wave_6_Ecuador%>%
  select(cf="V102" ,
         "reg"= N_REGION_ISO,
         cv="V103",
         cp="V104",
         c_primera="V105",
         c_dif_reg="V106",
         ano="V262",
         c_gob = "V115",
         p_trabajo="V181",
         c_policia="V113")

wvs_total <- bind_rows(wave6, wave7)
#---------------------------------------------------------------------------

# Colapsamos las respuestas de las preguntas en 2 categorias

#-----------------------------------------------------------------------
#Variable de confianza en el gobierno

#Agrupar c_gob en dos categorias 

wvs_total$c_gob<- as_factor(wvs_total$c_gob)
wvs_total$confianza_gobierno <- ifelse(wvs_total$c_gob %in% c("Quite a lot", "A great deal"),
                                       "confío", "no confío")

#Creamos una variable binaria, con la confianza en el gobierno

wvs_total <- wvs_total %>%
  mutate(confianza_gobieno_b = as.integer(c_gob %in% c("Quite a lot", "A great deal")))
#--------------------------------------------------------------------------------------

#Variable de confinaza en la familia

#agrupar cf en dos categorias 

wvs_total$cf<- as_factor(wvs_total$cf)
wvs_total$confianza <- ifelse(wvs_total$cf %in% c("Trust completely", "Trust somewhat"),
                              "confío", "no confío")

#Creamos una variable binaria, con la confianza

wvs_total <- wvs_total %>%
  mutate(confianza_binaria = as.integer(cf %in% c("Trust completely", "Trust somewhat")))
#--------------------------------------------------------------------------------------

#Variable de confinaza en personas de las diferentes regiones 


#agrupar c_dif_reg en dos categorias 

wvs_total$c_dif_reg<- as_factor(wvs_total$c_dif_reg)
wvs_total$confianza_en_otra_reg <- ifelse(wvs_total$c_dif_reg %in% c("Trust completely", "Trust somewhat"),
                                          "confío", "no confío")

#Creamos una variable binaria, con la confianza en personas de otras regiones

wvs_total <- wvs_total %>%
  mutate(confianza_en_otra_reg_b = as.integer(c_dif_reg %in% c("Trust completely", "Trust somewhat")))

#---------------------------------------------------------------------------------
# Agrupar p_trabajo en dos categorias

wvs_total$p_trabajo<- as_factor(wvs_total$p_trabajo)
wvs_total$miedo_perder_empleo <- ifelse(wvs_total$p_trabajo %in% c("Very much", "A great deal"),
                                        "confío", "no confío")

#Creamos una variable binaria, con el miedo de las personas a perder su trabajo

wvs_total <- wvs_total %>%
  mutate(miedo_perder_empleo = as.integer(p_trabajo %in% c("Very much", "A great deal")))

#Agrupar c_policia en dos categorias 

wvs_total$c_policia<- as_factor(wvs_total$c_policia)
wvs_total$confianza_policia <- ifelse(wvs_total$c_policia %in% c("Quite a lot", "A great deal"),
                                      "confío", "no confío")

#Creamos una variable binaria, con la confianza en la policia

wvs_total <- wvs_total %>%
  mutate(confianza_policia_b = as.integer(c_policia %in% c("Quite a lot", "A great deal")))



#--------------------------------------------------------------------------------------

#Limpieza de bases de datos 


#filtrando por Provincia y renombre de variables

wvs_uio_gye <- wvs_total%>%
  filter(reg %in% c("EC-G Guayas","EC-P Pichincha"))%>%
  mutate(reg=fct_recode(reg,
                        "Guayas"="EC-G Guayas",
                        "Pichincha"="EC-P Pichincha"))

#Relaizamos una grupación de la varible de provicias


wvs_c<- wvs_total%>%
  mutate(region = fct_collapse(reg,
                               "Sierra"=c("EC-A Azuay","EC-B Bolivar","EC-F Canar",
                                          "EC-C Carchi","EC-X Cotopaxi","EC-H Chimborazo",
                                          "EC-L Loja","EC-I Imbabura","EC-P Pichincha",
                                          "EC-T Tungurahua"),
                               "Costa"=c("EC-O El Oro","EC-E Esmeraldas","EC-G Guayas",
                                         "EC-R Los Rios","EC-M Manabi","EC-SE Santa Elena",
                                         "EC-SD Santo Domingo de los Tsachilas",
                                         "EC-SD Santo Domingo de los Tsachilas"),
                               "Oriente"=c("EC-S Morona Santiago","EC-N Napo","EC-D Orellana",
                                           "EC-Y Pastaza",
                                           "EC-U Sucumbios","EC-Z Zamora Chinchipe")))

#Relaizamos una grupación de la varible de provicias sin todas las del oriente

wvs_cs<- wvs_total%>%
  filter(reg %in% c("EC-A Azuay","EC-B Bolivar","EC-F Canar",
                    "EC-C Carchi","EC-X Cotopaxi","EC-H Chimborazo",
                    "EC-L Loja","EC-I Imbabura","EC-P Pichincha",
                    "EC-T Tungurahua","EC-O El Oro","EC-E Esmeraldas","EC-G Guayas",
                    "EC-R Los Rios","EC-M Manabi","EC-SE Santa Elena",
                    "EC-SD Santo Domingo de los Tsachilas",
                    "EC-SD Santo Domingo de los Tsachilas"))%>%
  mutate(region = fct_collapse(reg,
                               "Sierra"=c("EC-A Azuay","EC-B Bolivar","EC-F Canar",
                                          "EC-C Carchi","EC-X Cotopaxi","EC-H Chimborazo",
                                          "EC-L Loja","EC-I Imbabura","EC-P Pichincha",
                                          "EC-T Tungurahua"),
                               "Costa"=c("EC-O El Oro","EC-E Esmeraldas","EC-G Guayas",
                                         "EC-R Los Rios","EC-M Manabi","EC-SE Santa Elena",
                                         "EC-SD Santo Domingo de los Tsachilas",
                                         "EC-SD Santo Domingo de los Tsachilas")))