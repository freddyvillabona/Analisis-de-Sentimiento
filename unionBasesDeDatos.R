# PREPROCESAMIENTO DE DATOS
#
# Una vez extraidos los datos y teniendo una exploraciÃÂ³n previa de su contenido, serÃÂ¡ necesario realizar un preprocesamiento en el cual se seleccionarÃÂ¡ lo que sea de nuestra convenencia para el estudio especifico que se ha planteado
#

library("utf8"); 
library("NLP")
library("tm");
library("RColorBrewer");
library("dplyr");         # Con funciones auxiliares para manipular y transformar datos. En particular, el operador %>% permite escribir funciones mÃÂ¡s legibles para seres humanos.
library("tidyr");
library("readr");         # Facilita leer y escribir documentos.
library("SnowballC");
library("wordcloud2");    # Nube de palabras
library("ggplot2");       # GrÃÂ¡ficos
library("wordcloud");
library("tidyverse")
library("tidytext")
# Cargando base de datos y agregando etiqueta

#venteel23_19.csv
BD1 <- read.csv("venteel23_19.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="19Enero")

#TodaVzlaEnCabildo_19.csv
BD2 <- read.csv("todavzlaencambildo_19.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="19Enero")

#nosvemosel23_20.csv
BD3 <- read.csv("nosvemosel23_20.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="20Enero")

#edgar_zambrano_20.csv
BD4 <- read.csv("edgar_zambrano_20.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="20Enero")

#sala_constitucional_21.csv
BD5 <- read.csv("sala_constitucional_21.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="21Enero")

#cotiza_21.csv
BD6 <- read.csv("cotiza_21.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="21Enero")

#fuerte_tiuna_21.csv
BD7 <- read.csv("fuerte_tiuna_21.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="21Enero")

#la_candelaria_21.csv
BD8 <- read.csv("la_candelaria_21.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="21Enero")

#tsj_de_maduro_22.csv
BD9 <- read.csv("el_tsj_21.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="21Enero")

#tsj_de_maduro_22.csv
BD10 <- read.csv("tsj_de_maduro_22.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="22Enero")

#juan_guaido_23.csv
BD11 <- read.csv("juan_guaido_23.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="23Enero")

#nicolas_maduro_23.csv
BD12 <- read.csv("nicolas_maduro_23.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="23Enero")

#VenezuelaYElMundoConMaduro_24.csv
BD13 <- read.csv("VenezuelaYElMundoConMaduro_24.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="24Enero")

#tibisay_lucena_24.csv
BD14 <- read.csv("tibisay_lucena_24.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="24Enero")

#padrino_lopez_24.csv
BD15 <- read.csv("padrino_lopez_24.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="24Enero")

#guaido_24.csv
BD16 <- read.csv("guaido_24.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="24Enero")

#el_mundo_con_venezuela_25.csv
BD17 <- read.csv("el_mundo_con_venezuela_25.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="25Enero")

#wiston_25.csv
BD18 <- read.csv("wiston_25.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="25Enero")

#zc_25.csv
BD19 <- read.csv("zc_25.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="25Enero")


#jg_25.csv
BD20 <- read.csv("jg_25.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="25Enero")

#maria_corina_25.csv
BD21 <- read.csv("maria_corina_25.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="25Enero")

#an_25.csv
BD22 <- read.csv("an_25.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="25Enero")

#Vzla_y_EEUU_26.csv
BD23 <- read.csv("Vzla_y_EEUU_26.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="26Enero")

#ramo_verde_26.csv
BD24 <- read.csv("ramo_verde_26.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="26Enero")

#Plaza_Alfredo_Sadel_26.csv
BD25 <- read.csv("Plaza_Alfredo_Sadel_26.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="26Enero")

#juan_guaido_26.csv
BD26 <- read.csv("juan_guaido_26.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="26Enero")

#golpe_de_estado_26.csv
BD27 <- read.csv("golpe_de_estado_26.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="26Enero")

#canciller_26.csv
BD28 <- read.csv("canciller_26.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="26Enero")

#arreaza_26.csv
BD29 <- read.csv("arreaza_26.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="26Enero")

#jose_luis_silva_silva_27.csv
BD30 <- read.csv("jose_luis_silva_silva_27.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="27Enero")

#VzlaEnAsamblea_27.csv
BD31 <- read.csv("VzlaEnAsamblea_27.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="27Enero")

#tsj_de_maduro_22.csv
BD32 <- read.csv("pdvsa_28.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="28Enero")

#consul_de_venezuela_28.csv
BD33 <- read.csv("consul_de_venezuela_28.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="28Enero")

#diosdado_28.csv
BD34 <- read.csv("diosdado_28.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="28Enero")

#FANBActivaPorLaPatria_29.csv
BD35 <- read.csv("FANBActivaPorLaPatria_29.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="29Enero")

#pedro_carreno_29.csv
BD36 <- read.csv("pedro_carreno_29.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="29Enero")

#asamblea_nacional_29.csv
BD37 <- read.csv("asamblea_nacional_29.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="29Enero")

#Fiscal_general_29.csv
BD38 <- read.csv("Fiscal_general_29.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="29Enero")

#saab_29.csv
BD39 <- read.csv("saab_29.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="29Enero")

BD40 <- read.csv("mario_silva_30.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="30Enero")

#la_hojilla_30.csv
BD41 <- read.csv("la_hojilla_30.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="30Enero")

#nico_30.csv
BD42 <- read.csv("nico_30.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="30Enero")

#juan_guaido_30.csv
BD43 <- read.csv("juan_guaido_30.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="30Enero")

#manifestantes_30.csv
BD44 <- read.csv("manifestantes_30.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="30Enero")

# Unir las bases de datos en una... 

BD <- rbind(BD1,BD2,BD3,BD4,BD5,BD6,BD7,BD8,BD9,BD10,BD11,BD12,BD13,BD14,BD15,BD16,BD17,BD18,BD19,BD20,BD21,BD22,BD23,BD24,BD25,BD26,BD27,BD28,BD29,BD30,BD31,BD32,BD33,BD34,BD35,BD36,BD37,BD38,BD39,BD40,BD41,BD42,BD43,BD44)


write.csv(BD, file="BDENTERA2.csv")
