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

##1##############################################################################
#venteel23_19.csv
BD1 <- read.csv("venteel23_19.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="19Enero")

df1 <- select(BD1,text)
texto1 <- df1$text
docs1 <- Corpus(VectorSource(texto1))
mtd1 <- TermDocumentMatrix(docs1)
m1 <- as.matrix(mtd1)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)

##2##############################################################################
#TodaVzlaEnCabildo_19.csv
BD2 <- read.csv("todavzlaencambildo_19.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="19Enero")

df2 <- select(BD2,text)
texto2 <- df2$text
docs2 <- Corpus(VectorSource(texto2))
mtd2 <- TermDocumentMatrix(docs2)
m2 <- as.matrix(mtd2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)

##3##############################################################################
#nosvemosel23_20.csv
BD3 <- read.csv("nosvemosel23_20.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="20Enero")

df3 <- select(BD3,text)
texto3 <- df3$text
docs3 <- Corpus(VectorSource(texto3))
mtd3 <- TermDocumentMatrix(docs3)
m3 <- as.matrix(mtd3)
v3 <- sort(rowSums(m3),decreasing=TRUE)
d3 <- data.frame(word = names(v3),freq=v3)

##4##############################################################################
#edgar_zambrano_20.csv
BD4 <- read.csv("edgar_zambrano_20.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="20Enero")
df4 <- select(BD4,text)
texto4 <- df4$text
docs4 <- Corpus(VectorSource(texto4))
mtd4 <- TermDocumentMatrix(docs4)
m4 <- as.matrix(mtd4)
v4 <- sort(rowSums(m4),decreasing=TRUE)
d4 <- data.frame(word = names(v4),freq=v4)

##5##############################################################################
#sala_constitucional_21.csv
BD5 <- read.csv("sala_constitucional_21.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="21Enero")
df5 <- select(BD5,text)
texto5 <- df5$text
docs5 <- Corpus(VectorSource(texto5))
mtd5 <- TermDocumentMatrix(docs5)
m5 <- as.matrix(mtd5)
v5 <- sort(rowSums(m5),decreasing=TRUE)
d5 <- data.frame(word = names(v5),freq=v5)

#cotiza_21.csv
BD6 <- read.csv("cotiza_21.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="21Enero")
df6 <- select(BD6,text)
texto6 <- df6$text
docs6 <- Corpus(VectorSource(texto6))
mtd6 <- TermDocumentMatrix(docs6)
m6 <- as.matrix(mtd6)
v6 <- sort(rowSums(m6),decreasing=TRUE)
d6 <- data.frame(word = names(v6),freq=v6)


#fuerte_tiuna_21.csv
BD7 <- read.csv("fuerte_tiuna_21.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="21Enero")
df7 <- select(BD7,text)
texto7 <- df7$text
docs7 <- Corpus(VectorSource(texto7))
mtd7 <- TermDocumentMatrix(docs7)
m7 <- as.matrix(mtd7)
v7 <- sort(rowSums(m7),decreasing=TRUE)
d7 <- data.frame(word = names(v7),freq=v7)

#la_candelaria_21.csv
BD8 <- read.csv("la_candelaria_21.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="21Enero")
df8 <- select(BD8,text)
texto8 <- df8$text
docs8 <- Corpus(VectorSource(texto8))
mtd8 <- TermDocumentMatrix(docs8)
m8 <- as.matrix(mtd8)
v8 <- sort(rowSums(m8),decreasing=TRUE)
d8 <- data.frame(word = names(v8),freq=v8)

#tsj_de_maduro_22.csv
BD9 <- read.csv("el_tsj_21.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="21Enero")
df9 <- select(BD9,text)
texto9 <- df9$text
docs9 <- Corpus(VectorSource(texto9))
mtd9 <- TermDocumentMatrix(docs9)
m9 <- as.matrix(mtd9)
v9 <- sort(rowSums(m9),decreasing=TRUE)
d9 <- data.frame(word = names(v9),freq=v9)

#tsj_de_maduro_22.csv
BD10 <- read.csv("tsj_de_maduro_22.csv", comment.char="#", encoding="ISO-8859-13") %>%
  mutate(etiqueta ="22Enero")
df10 <- select(BD10,text)
texto10 <- df10$text
docs10 <- Corpus(VectorSource(texto10))
mtd10 <- TermDocumentMatrix(docs10)
m10 <- as.matrix(mtd10)
v10 <- sort(rowSums(m10),decreasing=TRUE)
d10 <- data.frame(word = names(v10),freq=v10)


################################################################################
################################################################################
################################################################################
d <- rbind(d1,d2,d3,d4,d5)

datos <- d[with(d, order(-d$freq)), ]

datos[1:20, ]
datos[1:20, ] %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = freq)) + 
  coord_flip() + 
  labs(title = "Veinte palabras más frecuentes en los documentos extraidos de Twitter",  x = "Terminos", y = "Número de usos en el total de los docuemntos")
