library("tidyverse")
library("tidytext")
library("tm")
library("lubridate")
library("zoo")
library("scales")
library("dplyr")

# Tema para los gráficos

tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

#tuits <- read.csv("BD_TOTAL.csv", stringsAsFactors = F, fileEncoding = "latin1") %>%
 # tbl_df()

tuits1 <- read.csv("BDLIMPIEZA2noche.csv", comment.char="#", encoding="ISO-8859-13") %>%
  tbl_df()

tuits <- read.csv("BDENTERA2.csv", comment.char="#", encoding="ISO-8859-13") %>%
 rename(texto = text) 
  

tuits <- data.frame(tuits1,tuits)
tuits <- select(tuits,text,screenName,favoriteCount,retweetCount,created,id,etiqueta)

# Sumar los favoritos con RT y  almacenarlos en la variable interaccion 
tuits$interaccion = rowSums (tuits[ , 3:4])

tuits <- select(tuits,id,created,etiqueta)
tuits <- cbind(tuits,tuits1)

# SELECCIONAR VARIABLES DE TRABAJO
tuits <- select(tuits,id,created,etiqueta,text)

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

conteo_palabras <- tuits_afinn %>% 
  group_by(Tipo) %>% 
  count(Palabra) 

conteo_palabras[1:20, ]

conteo_palabras %>%
  group_by(Tipo) %>%
  top_n(20) %>%
  ggplot(aes(reorder(Palabra, n), n, fill = Tipo)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~Tipo, scales = "free_y") +
  labs(y = "Cantidad por palabra", x = "Palabras o terminos") +
  coord_flip()

mapeo <- tuits_afinn %>% 
select(Tipo,Tendencias,Puntuacion)

ggplot(mapeo, aes(index, factor(Tendencias, levels = sort(unique(Tendencias), decreasing = TRUE)), fill = Puntuacion)) +
  geom_tile(color = "white") +
  scale_fill_gradient2() +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Chapter Progression", y = "Chapter") +
  ggtitle("Sentiment of Harry Potter and the Philosopher's Stone",
          subtitle = "Summary of the net sentiment score as you progress through each chapter") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")
#
#
#
# Los terminos "lucha" y "Maduro" no necesariamente representan para el léxico en español termirnos referentes a algo NEGATIVO o POSITIVO respectivamente, por lo cual serán sacadas de nuestro analisis para evitar sesgos mayores en los resultados.... 

tuits_afinn <-
  tuits_afinn %>%
  filter(Palabra != "lucha")%>%
  filter(Palabra != "maduro")

#Graficamos nuevamente

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

tuits_afinn_fecha <-
  tuits_afinn %>%
  group_by(id) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(Tendencias, Dia) %>%
  summarise(Media = mean(Puntuacion))
tuits_afinn_fecha

# Gráficamos nuevamente excluyendo las palabras y terminos "maduro" y "lucha"

conteo_palabras <- tuits_afinn %>% 
  group_by(Tipo) %>% 
  count(Palabra) 

conteo_palabras[1:20, ]

conteo_palabras %>%
  group_by(Tipo) %>%
  top_n(20) %>%
  ggplot(aes(reorder(Palabra, n), n, fill = Tipo)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~Tipo, scales = "free_y") +
  labs(y = "Cantidad por palabra", x = "Palabras o terminos") +
  coord_flip()


#Comparando sentimientos positivos y negativos

tuits_afinn %>%
  count(Tendencias, Tipo) %>%
  group_by(Tendencias) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Tendencias, Proporcion, fill = Tipo) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  tema_graf +
  theme(legend.position = "top")


# Bloxplots (diagrama caja y bigotes)

tuits %>%
  ggplot() +
  aes(Tendencias, Puntuacion_tuit, fill = Tendencias) +
  geom_boxplot() +
  tema_graf

# Día 3 y 4 de Noviembre, quedando por revisar la presencia de (NA) 
tuits %>%
  mutate(Dia = factor(Dia)) %>% 
  ggplot() +
  aes(Dia, Puntuacion_tuit, fill = Tendencias) +
  geom_boxplot(width = 1) +
  facet_wrap(~Tendencias) +
  tema_graf +
  theme(legend.position = "none")

# Se pueden analizar las tendencias de sentimientos usando las funciones de densidad de las puntuaciones

tuits %>%
  ggplot() +
  aes(Puntuacion_tuit, color = Tendencias) +
  geom_density() +
  facet_wrap(~Tendencias) +
  tema_graf

# Tendencias a través del tiempo (3 y 4 de noviembre) 

tuits %>%
  ggplot() +
  aes(Puntuacion_tuit, color = Tendencias) +
  geom_density() +
  facet_grid(Tendencias~Dia) +
  tema_graf

# Ahora se clasificaran las cuentas con la mayor cantidad de interacciones, es decir las cuentas de las cuales provinieron los mensajes con la mayor cantidad de interacciones con los usuarios para el momento de la extracción.

tuits_relevantes <- select(tuits,text,screenName,interaccion,Dia,Hora)%>% 
   arrange(desc(interaccion))

cuentasRelevantes <- tuits_relevantes[1:1000, ]
attach(cuentasRelevantes)

plot(interaccion,main="Nivel de interacción de los documentos",ylab="Interacción",xlab="Cantidad de documentos")

View(cuentasRelevantes)

write.csv(cuentasRelevantes, file="cuentas_Relevantes.csv")

