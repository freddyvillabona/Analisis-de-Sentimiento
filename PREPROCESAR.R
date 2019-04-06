# PREPROCESAMIENTO DE DATOS
#
# Una vez extraidos los datos y teniendo una exploración previa de su contenido, será necesario realizar un preprocesamiento en el cual se seleccionará lo que sea de nuestra convenencia para el estudio especifico que se ha planteado
#

library("utf8"); 
library("NLP")
library("tm");
library("RColorBrewer");
library("dplyr");         # Con funciones auxiliares para manipular y transformar datos. En particular, el operador %>% permite escribir funciones más legibles para seres humanos.
library("tidyr");
library("readr");         # Facilita leer y escribir documentos.
library("SnowballC");
library("wordcloud2");    # Nube de palabras
library("ggplot2");       # Gráficos
library("wordcloud");
# Importar datos previamente extraidos a través del API de Twitter 
#tws <- read.csv("~/R/extraccion.csv", comment.char="#", encoding="ISO-8859-13")
tws <- read.csv("BDENTERA2.csv", comment.char="#", encoding="ISO-8859-13")

tws[1:3, ]
# Detalles de la base de datos
str(tws)
attach(tws)

# Se seleccionan los documentos que contienen los textos (Cada documento es representado por un Tweet)
docs <- data.frame(text)
attach(docs)
docs = text
docs = as.character(docs)
# Se inicia la limpieza de información no útil para nuestro estudio
#
# remover RT
docs = gsub("( RT | via ) ( (?:\\b\\W*@\\w+)+)", " ", docs)


# Eliminar direcciones web
docs = gsub("http\\S*", " ", docs)

# remueve links
docs = gsub("http\\w+", " ", docs)

# remover @OTRASCUENTAS
docs = gsub("@\\w+", " ", docs)

# remueve simbolos de puntuación
docs = gsub("[[:punct:]]", " ", docs)

# remove números
docs = gsub("[[:digit:]]", " ", docs)

# remueve saltos de línea y tabulaciones
docs = gsub("[[:cntrl:]]", "", docs)





# Corrección de errores de acentuación por teclado...
docs = gsub("à", "á", docs)
docs = gsub("è", "é", docs)
docs = gsub("ì", "í", docs)
docs = gsub("ò", "ó", docs)
docs = gsub("ù", "ú", docs)



docs <- tolower(docs)
docs <- removeNumbers(docs)
docs <- removePunctuation(docs)
docs <- removeWords(docs, words = stopwords("spanish"))
docs <- removeWords(docs,c("rt"))
docs <- stripWhitespace(docs)

## INCONVENIENTE : En vista de presentar inconvenientes con los acentos queda pendiente resolver la codificación.... 

docs = gsub("á", "a", docs)
docs = gsub("é", "e", docs)
docs = gsub("í", "i", docs)
docs = gsub("ó", "o", docs)
docs = gsub("ú", "u", docs)
docs = gsub("ñ", "n", docs)

sw <- readLines("stopwords.txt",encoding="latin1")
sw = iconv(sw, to="ASCII//TRANSLIT")
docsCORPUS <- Corpus(VectorSource(docs))
docsCORPUS = tm_map(docsCORPUS, removeWords, sw)

# REGRESAR CORPUS A DATA FRAME
docs <- data.frame(text = sapply(docsCORPUS, as.character), stringsAsFactors = FALSE)


# GUARDAR LOS DATOS DE TEXTO CON SU LIMPIEZA REALIZADA
#write.csv(docs, file="LIMPIEZA.csv",fileEncoding = "latin1")
write.csv(docs, file="BDLIMPIEZA2noche.csv")




# Utilizando la función Corpus(), indicamos la fuente de nuestro texto
docs <- Corpus(VectorSource(docs))

# Verificar que el corpus se haya creado correctamente 
VCorpus(VectorSource(docs))

# Para verificar que el archivo se cargó correctamente, se procede con la función inspect() y mostrar el contenido del documento 1 al 3
#inspect(docs[1:7])

# Mostrar el contenido del documento 1 y la cantidad de caracteres que contiene 
#inspect(docs[[1]])

#meta(docs[[1]])

# Mostrar lo que contiene el primer documento
#docs[[1]][1]

# Cantidad total de documentos
length (tws)

sw <- readLines("stopwords.txt",encoding="latin1")
sw = iconv(sw, to="ASCII//TRANSLIT")
docs = tm_map(docs, removeWords, sw)


# Se contruye la matriz term-document, la cual es una tabla que contiene la frecuencia de las palabras. Esta matriz es el insumo principal para la construcciÃ³n de la nube de palabras.
mtd <- TermDocumentMatrix(docs)

# Muestra la cantidad de terminos totales y los documentos, además muestra el termino con más apariciones (cantidad)
mtd

m <- as.matrix(mtd)

# Conteo de terminos y mostrados en forma decreciente
##
# En ésta primera muestra de los datos recopilados, se encuentran direcciones web, caracteres, emoticons, entre otros. 
#
v <- sort(rowSums(m),decreasing=TRUE)

# Se asigna nombre a las variables que representan los terminos y sus respectivas frecuancias en cada termino
d <- data.frame(word = names(v),freq=v)


# Listamos los terminos mas frecuentes encontrados en la totalidad de documentos extraidos de Twitter
d1 = d[1:5, ]      # 5 mas frecuentes
d2 = d[1:10, ]     # 10 mas frecuentes
d3 = d[1:20, ]     # 20 mas frecuentes
d4 = d[1:50, ]     # 50 mas frecuentes
d5 = d[1:100, ]     # 50 mas frecuentes
d6 = d[1:1000, ]
# Gráfico con las 20 palabras mas frecuentes 
d[1:20, ] %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = freq)) + 
  coord_flip() + 
  labs(title = "Veinte palabras más frecuentes en los 85688 documentos extraidos de Twitter",  x = "Terminos", y = "Número de frecuencias en el total de los documentos")

# Gráfico con las 10 palabras mas frecuentes 
d[1:10, ] %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = freq)) + 
  coord_flip() + 
  labs(title = "Veinte palabras más frecuentes en los documentos extraidos de Twitter",  x = "Terminos", y = "Número de usos en el total de los docuemntos")

# Gráfico con las 20 palabras mas frecuentes 
d %>%
  mutate(perc = (freq/sum(freq))*100) %>%
  .[1:20, ] %>%
  ggplot(aes(word, perc)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = round(perc, 2))) + 
  coord_flip() +
  labs(title = "Veinte palabras más frecuentes en los documentos extraidos de Twitter",  x = "Terminos", y = "Porcentaje de uso en el total de los docuemntos")


# Gráfico con las 10 palabras mas frecuentes 
d %>%
  mutate(perc = (freq/sum(freq))*100) %>%
  .[1:10, ] %>%
  ggplot(aes(word, perc)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = round(perc, 2))) + 
  coord_flip() +
  labs(title = "Diez palabras más frecuentes en los documentos extraidos de Twitter",  x = "Terminos", y = "Porcentaje de uso en el total de los docuemntos")


# Nube de palabras con el paquete worcloud2
wordcloud2(data = d1, size = 0.5, shape = "cloud", color="random-dark", ellipticity = 1)

wordcloud2(data = d2, size = 0.5, shape = "cloud", color="random-dark", ellipticity = 1)

wordcloud2(data = d3, size = 0.9, shape = "cloud", color="random-dark", ellipticity = 2)

wordcloud2(data = d4, size = 0.9, shape = "cloud", color="random-dark", ellipticity = 2)

wordcloud2(data = d6, size = 0.9, shape = "cloud", color="random-dark", ellipticity = 1)

# El termino "Venezuela" por ser utilizado como busqueda, terminos como "los", "la", "el", entre otros, son los que más apariciones tienen.

# Se requiere realizar una limpieza de los datos, con los documentos y terminos que serán valiosos para el estudio de Analisis de Sentimiento, donde se deben descartar, caracteres especiales, emoticons, direcciones web, entre otros.... y centrarse netamente en los terminos que permitan generar conclusiones. 

