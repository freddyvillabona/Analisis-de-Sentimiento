library("tidyverse")
library("tidytext")
library("tm")
library("cluster")
library("NLP")
library("tm");
library("dplyr");         # Con funciones auxiliares para manipular y transformar datos. En particular, el operador %>% permite escribir funciones mÃ¡s legibles para seres humanos.
library("tidyr");
library("wordcloud2");    # Nube de palabras
library("ggplot2");       # Gráficos
library("wordcloud");
library("ggpubr")
library("plotly")
library("lubridate");
library("zoo");
library("scales");

tuits <- read.csv("a.csv", comment.char="#", encoding="ISO-8859-13") %>%
  rename(texto = text) 

tuits <- read.csv("b.csv", comment.char="#", encoding="ISO-8859-13") %>%
  rename(texto = text) 

#tuits <- data.frame(tuits1,tuits)
tuits <- select(tuits,texto,screenName,favoriteCount,retweetCount,created)

tuits$interaccion = rowSums (tuits[ , 3:4])
selected <- which(tuits$interaccion >= 0)

dates <- strptime(tuits$created, format="%Y-%m-%d")

plot(x=dates, y=tuits$interaccion, type="b", col="red",
     xlab="Date", ylab="Nivel de interacción", main="Nivel de interacción (usuarios relevantes)")
colors <- rainbow(10)[1:length(selected)]
points(dates[selected], tuits$interaccion[selected],
       pch=21, col="blue",lty=n)
text(tuits$interaccion[4929], col="grey", cex=.9)
abline=T

###### SEPARAR FECHAS
tuits <- 
  tuits %>%
  separate(created, into = c("anio", "mes","dias"), sep = "-") %>%
  separate(dias, into = c("dia", "hora"), sep = " ")

# ORDENAR BASE DE DATOS DE MAYOR A MENOR, TOMANDO EN CUENTA LA VARIABLE INTERACCIÓN
datos <- tuits[with(tuits, order(-tuits$interaccion)), ]
datos1 <- select(tuits,screenName,interaccion)
datos1 <- datos1[with(datos1, order(-datos1$interaccion)), ]

datos1 <- datos1[1:20, ]

#suma
m <- aggregate(datos1$interaccion, by=list(usuario=datos1$screenName), FUN=sum)

piepercent<- round(100*m$x/sum(m$x), 1)
pie(m$x,labels = m$usuario, main="Cuentas con mayor cantidad de interaciones (primer periodo)",col = rainbow(length(m$x)))

## GRÁFICA CON GGPIS
#usuario NOMBRES DE USUARIO
#x CANTIDAD DE INTERACCIONES
plot_ly(m, labels = ~usuario, values = ~x ,type = "pie",textposition = 'outside',textinfo = 'label+value', colors = "dark") %>%
  layout(title = 'Cuentas con mayor cantidad de interacciones (primer periodo)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

