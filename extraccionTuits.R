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
tweets <- searchTwitter("venezuela", n=1000, lang="es",since="2019-01-19", until="2019-01-20")

tweets

# Tweets a data fragme
df = twListToDF(tweets)

# Guardar extracción en CSV y txt
write.csv(df, file="venezuela.csv")
write.csv(df, file="venezuela.txt")


