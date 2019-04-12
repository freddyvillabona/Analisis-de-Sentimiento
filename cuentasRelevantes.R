library("tidyverse")
library("tidytext")
library("tm")
library("lubridate")
library("zoo")
library("scales")
library("dplyr")

# Análisis de Sentimiento a cuentas de Twitter relevantes, previamente identificada en los análisis previos 
tuits <- read.csv("cuentas_Relevantes.csv", stringsAsFactors = F, fileEncoding = "latin1")
 
# A criterio de investigador, se seleccion las 5 cuentas mas relevantes para realizar el análisis de sentimiento
tuits <- df[1:2500,]

# Tema para los gráficos
tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

afinn <- read.csv("lexico_afinn_SIN_ACENTOS.csv", stringsAsFactors = F, fileEncoding = "latin1") %>%
  tbl_df()

afinn

# Fechas
tuits <- 
  tuits %>%
  separate(created, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Periodo", "Mes", "Dia"), sep = "-",
           remove = FALSE) %>%
  mutate(Fecha = dmy(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text)) 

# ANALISIS DE SENTIMIENTO / PUNTUACION POR TUIT
tuits_afinn <- 
  tuits %>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Tendencias" = etiqueta)

tuits <-
  tuits_afinn %>%
  group_by(id) %>%
  summarise(Puntuacion_tuit = mean(Puntuacion)) %>%
  left_join(tuits, ., by = "id") %>% 
  mutate(Puntuacion_tuit = ifelse(is.na(Puntuacion_tuit), 0, Puntuacion_tuit)) %>% 
  rename("Tendencias" = etiqueta)

# Explorando los datos, (CANTIDADES DE TERMINOS CON SENTIMIENTO POSITIVO Y NEGATIVO ENCONTRADO EN CADA UNO DE LOS DOCUMENTOS Y ETIQUETADOS POR SUS TENDENCIAS)

# Total
tuits_afinn %>%
  count(Tendencias)

# Únicas
tuits_afinn %>% 
  group_by(Tendencias) %>% 
  distinct(Palabra) %>% 
  count()

# palabras positivas y negativas más usadas por cada tendencia

map(c("Positiva", "Negativa"), function(sentimiento) {
  tuits_afinn %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(Tendencias) %>%
    count(Palabra, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Palabra, n, fill = Tendencias) +
    geom_col() +
    facet_wrap("Tendencias", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento) +
    tema_graf
})
