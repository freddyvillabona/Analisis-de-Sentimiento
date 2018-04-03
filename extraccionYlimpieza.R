# Cargar las librerias

library("utf8")
library("ROAuth");
library("base64enc");
library("twitteR");
library("streamR");
library("ROAuth");
library("streamR");
library("tm")
library("RColorBrewer")
library("wordcloud")


# Cargar parámetros de configuración API TWITTER

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
options(httr_oauth_cache=T)

# Credenciales TWITTER

consumer_key <- "	"
consumer_secret <- " "
access_token <- " "
access_secret <- " "

# Autenticación API TWITTER

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

credentials_file <- "my_oauth.Rdata"
if (file.exists(credentials_file)){
  load(credentials_file)
} else {
  cred <- OAuthFactory$new(consumerKey = consumer_key, consumerSecret =
                             consumer_secret, requestURL = reqURL, accessURL = accessURL, authURL = authURL)
  cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  save(cred, file = credentials_file)
}

# Recolecta tweets con palabra, frases y nombre de usuario
tweets <- searchTwitter("venezuela", n=1000, lang="es")

#tweets = userTimeline("freddyvillabona", 1000)
tweets

# Recolecta tweets con palabra, frases y nombre de usuario en ubucación especifica 
#tweets <- searchTwitter("venezuela",n=100,lang="es",geocode='42.375,-71.1061111,10mi')

# Tweets a data fragme
df = twListToDF(tweets)

# Tweets en texto 
txt = df$text

# Arreglo de datos #

# remover RT
txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt)

# remover @OTRASCUENTAS
txtclean = gsub("@\\w+", "", txtclean)

# remueve simbolos de puntuación
txtclean = gsub("[[:punct:]]", "", txtclean)

# remove números
txtclean = gsub("[[:digit:]]", "", txtclean)


txtclean = gsub("ã¡", "a", txtclean)
txtclean = gsub("ã©", "e", txtclean)
txtclean = gsub("ã³", "o", txtclean)
txtclean = gsub("ãº", "u", txtclean)
txtclean = gsub("ã±", "ñ", txtclean)
txtclean = gsub("ã¨", "e", txtclean)
txtclean = gsub("ã²", "o", txtclean)
txtclean = gsub("ã", "i", txtclean)
txtclean = gsub("ç", "c", txtclean)
txtclean = gsub("ã±", "ñ", txtclean)
txtclean = gsub("Ã", "ñ ", txtclean)
txtclean = gsub("á", "a", txtclean)
txtclean = gsub("é", "e", txtclean)
txtclean = gsub("í", "i", txtclean)
txtclean = gsub("ó", "o", txtclean)
txtclean = gsub("ú", "u", txtclean)

# remueve links
txtclean = gsub("http\\w+", "", txtclean)
##### fin limpieza de datos #####

# construye un corpus
corpus = Corpus(VectorSource(txtclean))

# convierte a minúsculas
corpus = tm_map(corpus, tolower)

# remueve palabras vacías 
corpus = tm_map(corpus, removeWords, c(stopwords("spanish"), "freddyvillabona"))

# carga archivo de palabras vacías personalizada y lo convierte a ASCII

sw <- read.csv("C:/Users/Freddy/Desktop/workR/palabras.txt", sep="", encoding="utf-8")

sw = iconv(sw, to="ASCII//TRANSLIT")

# remueve palabras vacías personalizada
corpus = tm_map(corpus, removeWords, sw)

# remueve espacios en blanco adicionales
corpus = tm_map(corpus, stripWhitespace)

# crea una matriz de términos
tdm <- TermDocumentMatrix(corpus)

# convierte a una matriz
m = as.matrix(tdm)

# conteo de palabras en orden decreciente
wf <- sort(rowSums(m),decreasing=TRUE)

# crea un data frame con las palabras y sus frecuencias
dm <- data.frame(word = names(wf), freq=wf)

# grafica la nube de palabras (wordcloud)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"),scale=c(5, 1),min.freq =5)
