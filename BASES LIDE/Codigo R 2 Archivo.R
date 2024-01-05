#Carga de librerias y bases
library(haven)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(lubridate)
library(tidyverse)
library(here)
install.packages("cowplot")
library(cowplot)
library(patchwork)
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
Wave_6_Ecuador$V115 <- as_factor(Wave_6_Ecuador$V115)
Wave_7_Ecuador$Q71 <- as_factor(Wave_7_Ecuador$Q71)
Wave_6_Ecuador$V181 <- as_factor(Wave_6_Ecuador$V181)
Wave_7_Ecuador$Q142 <- as_factor(Wave_7_Ecuador$Q142)
Wave_6_Ecuador$V113 <- as_factor(Wave_6_Ecuador$V113)
Wave_7_Ecuador$Q69 <- as_factor(Wave_7_Ecuador$Q69)
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

#Agrupar c_gob en dos categorias 

wvs_total$c_gob<- as_factor(wvs_total$c_gob)
wvs_total$confianza_gobierno <- ifelse(wvs_total$c_gob %in% c("Quite a lot", "A great deal"),
                              "confío", "no confío")

#Creamos una variable binaria, con la confianza en el gobierno

wvs_total <- wvs_total %>%
  mutate(confianza_gobieno_b = as.integer(c_gob %in% c("Quite a lot", "A great deal")))


#agrupar cf en dos categorias 

wvs_total$cf<- as_factor(wvs_total$cf)
wvs_total$confianza <- ifelse(wvs_total$cf %in% c("Trust completely", "Trust somewhat"),
                              "confío", "no confío")

#Creamos una variable binaria, con la confianza

wvs_total <- wvs_total %>%
  mutate(confianza_binaria = as.integer(cf %in% c("Trust completely", "Trust somewhat")))


#agrupar c_dif_reg en dos categorias 

wvs_total$c_dif_reg<- as_factor(wvs_total$c_dif_reg)
wvs_total$confianza_en_otra_reg <- ifelse(wvs_total$c_dif_reg %in% c("Trust completely", "Trust somewhat"),
                              "confío", "no confío")

#Creamos una variable binaria, con la confianza en personas de otras regiones

wvs_total <- wvs_total %>%
  mutate(confianza_en_otra_reg_b = as.integer(c_dif_reg %in% c("Trust completely", "Trust somewhat")))

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
#analizis de variable de confianza en la familia

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
#analizis de variable de confianza en la familia

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
##################################################################################
library(cowplot)
library(patchwork)

combined_plot <- cf_r_2013 + cf_r_2018 +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Comparación de 2013 y 2018",
                  subtitle = "Confianza en la familia",
                  theme = theme(
                    plot.title = element_text(hjust = 0, color = "grey20", face = "bold", size = 14),
                    plot.caption = element_text(hjust = 0, color = "grey30", face = 'italic')
                  ))
print(combined_plot)

################################################################################

#Analisis de la variable de confianza en otras regiones 
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

####################################################################################

#Analizis de la variable de confianza sobre el gobierno
cf_r_3 <- wvs_c %>% 
  group_by(ano, region) %>% 
  summarize(mean = mean(as.numeric(confianza_gobieno_b), na.rm = TRUE), 
            se = sd(as.numeric(confianza_gobieno_b), na.rm = TRUE)/sqrt(n()))


#Graficos por region sierra y costa

# Gráfico 2013

cf_r3_2013 <- cf_r_3 %>% 
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
print(cf_r3_2013)

# Gráfico 2018

cf_r3_2018 <- cf_r_3 %>% 
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
print(cf_r3_2018)

# Union de los dos graficos

combined_plot_3 <- cf_r3_2013 + cf_r3_2018 +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Confianza de los ecuatorianos en el gobierno",
                  subtitle = "cuantas personas confian en el gobierno",
                  caption = "Las cifras representan la cantidad de personas en porcentaje que no confian en el gobierno ecuatoriano.
Las barras representan intervalos de confianza del 95%. Fuente:Encuesta Mundial de Valores (WVS) olas 2013 y 2018. 
Elaborado por: Laboratorio de Investigación para el Desarrollo del Ecuador (LIDE)",
                  theme = theme(
                    plot.title = element_text(hjust = 0, color = "grey20", face = "bold", size = 14),
                    plot.caption = element_text(hjust = 0, color = "grey30", face = 'italic')
                  ))
print(combined_plot_3)
################################################################################

#Analisis de la variable del miedo a perder el trabajo

cf_r_4 <- wvs_c %>% 
  group_by(ano, region) %>% 
  summarize(mean = mean(as.numeric(miedo_perder_empleo), na.rm = TRUE), 
            se = sd(as.numeric(miedo_perder_empleo), na.rm = TRUE)/sqrt(n()))


#Graficos por region sierra y costa

# Gráfico 2013

cf_r4_2013 <- cf_r_4 %>% 
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
print(cf_r4_2013)

# Gráfico 2018

cf_r4_2018 <- cf_r_4 %>% 
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
print(cf_r4_2018)

############################################################################

cf_r_5 <- wvs_c %>% 
  group_by(ano, region) %>% 
  summarize(mean = mean(as.numeric(confianza_policia_b), na.rm = TRUE), 
            se = sd(as.numeric(confianza_policia_b), na.rm = TRUE)/sqrt(n()))

cf_r_total <- wvs_c %>% 
  group_by(ano, region) %>% 
  summarize(mean_cf = mean(as.numeric(confianza_binaria), na.rm = TRUE), 
            se_cf = sd(as.numeric(confianza_binaria), na.rm = TRUE)/sqrt(n()),
            mean_otra_reg = mean(as.numeric(confianza_en_otra_reg_b), na.rm = TRUE), 
            se_otra_reg = sd(as.numeric(confianza_en_otra_reg_b), na.rm = TRUE)/sqrt(n()),
            mean_gob = mean(as.numeric(confianza_gobieno_b), na.rm = TRUE), 
            se_gob = sd(as.numeric(confianza_gobieno_b), na.rm = TRUE)/sqrt(n()),
            mean_police = mean(as.numeric(confianza_policia_b), na.rm = TRUE), 
            se_police = sd(as.numeric(confianza_policia_b), na.rm = TRUE)/sqrt(n()))
          
####
install.packages("openxlsx")
cf_selected <- cf_r_total %>%
  select(ano,region,mean_cf,mean_gob,mean_otra_reg,mean_police)
print(cf_selected)
write.csv(cf_selected, "C:/Users/User/Git hub/Confianza/BASES LIDE/cf_selected_3.csv", row.names = FALSE,col.names = TRUE, sep = ";")


