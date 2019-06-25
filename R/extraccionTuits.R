# Cargar las librerias

library("utf8");
library("ROAuth");
library("base64enc");
library("twitteR");
library("streamR");
library("ROAuth");
library("streamR");
library("tm");

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

# Autenticación (API TWITTER)

options(httr_oauth_cache=TRUE)

setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                    access_token = accessToken, access_secret = accessSecret)

# Consulta a la API
tweets <- searchTwitter("venezuela", n=1000, lang="es",since="2019-01-19", until="2019-01-20")

tweets

# Tweets a data fragme
df = twListToDF(tweets)

# Guardar extracción en CSV y txt
write.csv(df, file="venezuela.csv")
write.csv(df, file="venezuela.txt")


