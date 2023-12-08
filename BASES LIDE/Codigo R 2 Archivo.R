#Carga de librerias y bases
library(haven)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(lubridate)
library(tidyverse)
library(here)
Wave_7_Ecuador <- read_dta("BASES LIDE/WVS_Wave_7_Ecuador_Stata_v5.0.dta")
Wave_6_Ecuador<- read_dta("BASES LIDE/WV6_Data_Ecuador_Stata_v20201117.dta")

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

wave7 <- Wave_7_Ecuador%>%
  select(cf= "Q58",
         "reg"= N_REGION_ISO,
         cv="Q59",
         cp="Q60",
         c_primera="Q61",
         c_dif_reg="Q62",
         ano="A_YEAR")

wave6<-Wave_6_Ecuador%>%
  select(cf="V102" ,
         "reg"= N_REGION_ISO,
         cv="V103",
         cp="V104",
         c_primera="V105",
         c_dif_reg="V106",
         ano="V262")
wvs_total <- bind_rows(wave6, wave7)

#agrupar cf en dos categorias 

wvs_total$cf<- as_factor(wvs_total$cf)
wvs_total$confianza <- ifelse(wvs_total$cf %in% c("Trust completely", "Trust somewhat"),
                              "confío", "no confío")

#Creamos una variable binaria, con la confianza

wvs_total <- wvs_total %>%
  mutate(confianza_binaria = as.integer(cf %in% c("Trust completely", "Trust somewhat")))

#################################################################################
wvs_total$c_dif_reg<- as_factor(wvs_total$c_dif_reg)
wvs_total$confianza_en_otra_reg <- ifelse(wvs_total$c_dif_reg %in% c("Trust completely", "Trust somewhat"),
                              "confío", "no confío")


wvs_total <- wvs_total %>%
  mutate(confianza_en_otra_reg_b = as.integer(c_dif_reg %in% c("Trust completely", "Trust somewhat")))
#################################################################################
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

#Calcular la media y error estandar por provincia 
cf_p <- wvs_uio_gye %>% 
  group_by(ano, reg) %>% 
  summarize(mean = mean(as.numeric(confianza_binaria), na.rm = TRUE), 
            se = sd(as.numeric(confianza_binaria), na.rm = TRUE)/sqrt(n()))

#Calcular la media y error estandar por region
cf_r <- wvs_c %>% 
  group_by(ano, region) %>% 
  summarize(mean = mean(as.numeric(confianza_binaria), na.rm = TRUE), 
            se = sd(as.numeric(confianza_binaria), na.rm = TRUE)/sqrt(n()))

###############################################################################
#Graficos por provincia Pichincha y Guayas

# Gráfico 2013

cf_p_2013 <- cf_p %>% 
  filter(ano == 2013) %>%
  filter(complete.cases(reg, mean, se)) %>%
  ggplot(aes(x = reg, y = mean, fill = reg)) + 
  geom_bar(stat = "identity", width = 0.3,) +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width = 0.1) +
  geom_text(aes(label = scales::percent(mean, digits =1)), 
            color = "grey20", size = 4, vjust = -2) +
  labs(title = "2013", x = "", y = "") + 
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_fill_manual(values = c("lightcoral", "lightblue")) 
print(cf_p_2013)

# Gráfico 2018

cf_p_2018 <- cf_p %>% 
  filter(ano == 2018) %>%
  filter(complete.cases(reg, mean, se)) %>%
  ggplot(aes(x = reg, y = mean, fill = reg)) + 
  geom_bar(stat = "identity", width = 0.3) +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width = 0.1) +
  geom_text(aes(label = scales::percent(mean, digits =1)), 
            color = "grey20", size = 4, vjust = -2) +
  labs(title = "2018", x = "", y = "") + 
  scale_y_continuous(limits = c(0, 1.5))
print(cf_p_2018)
################################################################################
#Graficos por region sierra y costa

# Gráfico 2013

cf_r_2013 <- cf_r %>% 
  filter(ano == 2013) %>%
  filter(complete.cases(region, mean, se)) %>%
  ggplot(aes(x = region, y = mean, fill = region)) + 
  geom_bar(stat = "identity", width = 0.3,) +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width = 0.1) +
  geom_text(aes(label = scales::percent(mean, digits =1)), 
            color = "grey20", size = 4, vjust = -2) +
  labs(title = "2013", x = "", y = "") + 
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_fill_manual(values = c("lightcoral", "lightblue","green")) 
print(cf_r_2013)

# Gráfico 2018

cf_r_2018 <- cf_r %>% 
  filter(ano == 2018) %>%
  filter(complete.cases(region, mean, se)) %>%
  ggplot(aes(x = region, y = mean, fill = region)) + 
  geom_bar(stat = "identity", width = 0.3,) +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width = 0.1) +
  geom_text(aes(label = scales::percent(mean, digits =1)), 
            color = "grey20", size = 4, vjust = -2) +
  labs(title = "2018", x = "", y = "") + 
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_fill_manual(values = c("lightcoral", "lightblue","green")) 
print(cf_r_2018)

################################################################################

cf_r_2 <- wvs_c %>% 
  group_by(ano, region) %>% 
  summarize(mean = mean(as.numeric(confianza_en_otra_reg_b), na.rm = TRUE), 
            se = sd(as.numeric(confianza_en_otra_reg_b), na.rm = TRUE)/sqrt(n()))


#Graficos por region sierra y costa

# Gráfico 2013

cf_r2_2013 <- cf_r_2 %>% 
  filter(ano == 2013) %>%
  filter(complete.cases(region, mean, se)) %>%
  ggplot(aes(x = region, y = mean, fill = region)) + 
  geom_bar(stat = "identity", width = 0.3,) +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width = 0.1) +
  geom_text(aes(label = scales::percent(mean, digits =1)), 
            color = "grey20", size = 4, vjust = -2) +
  labs(title = "2013", x = "", y = "") + 
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_fill_manual(values = c("lightcoral", "lightblue","green")) 
print(cf_r2_2013)

# Gráfico 2018

cf_r2_2018 <- cf_r_2 %>% 
  filter(ano == 2018) %>%
  filter(complete.cases(region, mean, se)) %>%
  ggplot(aes(x = region, y = mean, fill = region)) + 
  geom_bar(stat = "identity", width = 0.3,) +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width = 0.1) +
  geom_text(aes(label = scales::percent(mean, digits =1)), 
            color = "grey20", size = 4, vjust = -2) +
  labs(title = "2018", x = "", y = "") + 
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_fill_manual(values = c("lightcoral", "lightblue","green")) 
print(cf_r2_2018)


