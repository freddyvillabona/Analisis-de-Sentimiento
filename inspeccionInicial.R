library("utf8");          # Codificación UTF-8.
library("base64enc"); 
library("twitteR");       # Paquete para conectar con el API de Twitter.
library("NLP")
library("tm");            # Específico para minería de textos.
library("RColorBrewer");
library("dplyr");         # Con funciones auxiliares para manipular y transformar datos. En particular, el operador %>% permite escribir funciones más legibles para seres humanos.

# CARGAR BASE DE DATOS

tws <- read_csv("venezuela.csv")

str(tws[1:4])

attach(tws)
texto <- tws$text

df <- twListToDF(texto)
names(df) 

#La extracción nos proporciona 16 variables, entre las principales los datos textuales correspondientes a cada Tweet, ID del usuario, hora, fecha, entre otros..

#Tweets en texto (almacenamos los Tweets en la variable texto)

texto <- df$text
#ID USUARIOS: Cada usuario de Twitter está identificado por un Codigo ID (mostrar los 10 primeros)

id_usuarios <- df$id
id_usuarios[1:10]

#NOMBRE DE USUARIO: Cada usuario de Twitter está logueado por un nombre de usuario (mostrar los 10 primeros)

nombre_deUsuario <- df$screenName
nombre_deUsuario [1:10]

#Codificación UTF-8

texto <- iconv(texto, "ISO-8859-13")
texto <- iconv(texto, to="ASCII//TRANSLIT")
#Utilizando la función Corpus(), indicamos la fuente de nuestro texto

texto <- iconv(texto, "UTF-8")
texto <- iconv(texto, to="ASCII//TRANSLIT")
#Utilizando la función Corpus(), indicamos la fuente de nuestro texto

docs <- Corpus(VectorSource(texto))
VCorpus(VectorSource(docs))

#Para verificar que el archivo se cargó correctamente, se procede con la función inspect() / Se muestran los primeros 100 documentos de los 2000 en total.

#Mostrar el contenido del documento 1 al 100
inspect(docs[1:100])

#Mostrar el contenido del documento 1 al 3
inspect(docs[1:3])

#Mostrar el contenido del documento 1 y la cantidad de caracteres que contiene
inspect(docs[[1]])

#Muestra la cantidad de filas y columnas presentes en el data frame
meta(docs[[1]])

#Cantidad total de documentos
length (tws)

meta(docs)
## data frame with 0 columns and 2000 rows
#Se contruye la matriz term-document, la cual es una tabla que contiene la frecuencia de las palabras. Esta matriz es el insumo principal para la construcciÃ³n de la nube de palabras.

mtd <- TermDocumentMatrix(docs)
#Muestra la cantidad de terminos totales y los documentos, además muestra el termino con más apariciones (cantidad)

mtd
## <<TermDocumentMatrix (terms: 8257, documents: 2000)>>
## Non-/sparse entries: 22999/16491001
## Sparsity           : 100%
## Maximal term length: 65
## Weighting          : term frequency (tf)
m <- as.matrix(mtd)
#Conteo de terminos y mostrados en forma decreciente. En ésta primera muestra de los datos recopilados, se encuentran direcciones web, caracteres, emoticons, entre otros.

v <- sort(rowSums(m),decreasing=TRUE)
#Se asigna nombre a las variables que representan los terminos y sus respectivas frecuancias en cada termino

d <- data.frame(word = names(v),freq=v)
#Listamos los 20 terminos mas frecuentes encontrados en la totalidad de documentos extraidos de Twitter

d[1:20, ]

#Gráfico con las 20 palabras mas frecuentes

d[1:20, ] %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = freq)) + 
  coord_flip() + 
  labs(title = "Veinte palabras más frecuentes en los documentos extraidos de Twitter",  x = "Terminos", y = "Número de usos en el total de los docuemntos")
 de los docuemntos")
