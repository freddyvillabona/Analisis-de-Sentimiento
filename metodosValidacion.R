library("tidyverse")
library("tidytext")
library("tm")
library("lubridate")
library("zoo")
library("scales")
library("dplyr")
library("cluster")
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
library("factoextra")

cr <- read.csv("cuentas_Relevantes719.csv", comment.char="#", encoding="ISO-8859-13") %>%
  tbl_df()

cr <- read.csv("cuentas_Relevantes.csv", comment.char="#", encoding="ISO-8859-13") %>%
  tbl_df()


cr <- select(cr,text,interaccion)
attach(cr)


# Selecciono la variable texto y se convierte en corpus
docs <- text
Corpusdocs <- Corpus(VectorSource(docs))

MT <- DocumentTermMatrix(Corpusdocs)
#MT <- TermDocumentMatrix(Corpusdocs)


mt2 <- as.matrix(MT)
mt3 <- mt2

df <- as.data.frame(mt3)
mt3<-t(df)

mt4 <- mt3 %>% rowSums() %>% sort(decreasing = TRUE)
View(mt4)



k1 <-kmeans(mt4,1,10)
k2 <-kmeans(mt4,2,10)
k3 <-kmeans(mt4,3,10)
k4 <-kmeans(mt4,4,10)

c <- dist(mt4)

# Calcular el numero optimo de cluster
library(factoextra)
datos <- scale(mt4)
fviz_nbclust(datos, FUNcluster = kmeans, method = "wss", k.max = 8) +
  labs(title = "Número óptimo de clusters")


silk4<- silhouette(k4$cluster,dist=c)

plot(silk4,border=blues9)
View(MT)
p<-cbind(mt4,k3$cluster)



fviz_cluster(k3, p,a.rm = FALSE,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),geom = c("point", "text"),pointsize = 1.5,textsize=5,labelsize = 10,
             main = "Partitioning Clustering Plot"
)



library(igraph)
library(tidygraph)
library(ggraph)

fviz_cluster(k3, c , ellipse.type = "convex",show.clust.cent = TRUE,ellipse = TRUE, ellipse.alpha = 0.2,
             repel = TRUE) +
  theme_bw() +
  labs(title = "Análisis de conglomerados") +
  theme(legend.position = "none")


library(Ckmeans.1d.dp)
x <- rnorm(mt4)
k<-3
result <- Ckmeans.1d.dp(x,k)
plot(result)
#k <- max(result$cluster)

result <- Ckmeans.1d.dp(x,k,mt4)
plot(result, main = "Agrupamiento ponderado (n términos)", ylab="Frecuencias/término",xlab="n", sub = "A=red , B=black , C=")


plot(x, col=result$cluster, pch=result$cluster, cex=1.5,
     main="Agrupación Univariante con k-medias estimada",
     sub=paste("Número de clusters estimado", k))
abline(h=result$centers, col=1:k, lty="dashed", lwd=2)
legend("topright", paste("Cluster", 1:k), col=1:k, pch=1:k, cex=1.5, bty="n")


res <- Ckmeans.1d.dp(x, k=3,  result$cluster)

plot(res)

TemasPC16b <- cbind(cluster =res$cluster,mt4)


g1<-TemasPC16b[res$cluster==1, ]
g2<-TemasPC16b[res$cluster==2, ]
g3<-TemasPC16b[res$cluster==3, ]
#g4<-TemasPC16b[res$cluster==4, ]

t1<- as.data.frame(g1)
t2<- as.data.frame(g2)
t3<- as.data.frame(g3)
#t4<- as.data.frame(g4)

t1<- select(t1,mt4)
t1<- as.data.frame(t1)

t2<- select(t2,mt4)
t2<- as.data.frame(t2)

t3<- select(t3,mt4)
t3<- as.data.frame(t3)

t11<-t(t1)
t22<-t(t2)
t33<-t(t3)


t111<-data.frame(Palabra=names(t1),frecuencia=g1)
t222<-data.frame(Palabra=names(t2),frecuencia=g2)
t333<-data.frame(Palabra=names(t3),frecuencia=g3)

d1 <- select(t111, -frecuencia.cluster,-Palabra)
write.csv(d1, file="d1.csv")
d1 <- read.csv("~/R/R/TUITS/d1.csv")

d2 <- select(t222, -frecuencia.cluster,-Palabra)
write.csv(d2, file="d2.csv")
d2 <- read.csv("~/R/R/TUITS/d2.csv")

d3 <- select(t333, -frecuencia.cluster,-Palabra)
write.csv(d3, file="d3.csv")
d3 <- read.csv("~/R/R/TUITS/d3.csv")
#d4 <- select(t4, -frecuencia.cluster)

d11 <- d1[1:23, ]
d22 <- d2[1:30, ]
d33 <- d3[1:40, ]
wordcloud2(d11, size = 0.5, shape = "cloud", color="random-dark", ellipticity = 1)
wordcloud2(d22, size = 0.5, shape = "cloud", color="random-dark", ellipticity = 1)
wordcloud2(d33, size = 0.5, shape = "cloud", color="random-dark", ellipticity = 1)
