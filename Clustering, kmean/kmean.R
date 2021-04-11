data(wine,package='rattle')
head(wine)

wine.stand<-scale(wine[-1])  #estandarizacion
head(wine.stand)

k.means.fit<-kmeans(wine.stand, 4) #elegiminos NUM de clusters a ojo.
attributes(k.means.fit)
k.means.fit$centers #los centros de los diferentes alcoholes
k.means.fit$cluster #agrupaciones con cada uno de los datos, cuidado que R da diferentes valores dependiendo del la pantalla de salida
k.means.fit$size #numero de miembros dentro de cada cluster
wssplot<-function(data, nc=200, seed=1234){
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){ #aqui comienza el contador, de 2 a 15
    set.seed(seed)  #ajuste el avance en seed
    wss[i]<-sum(kmeans(data,centers = i)$withinss) #a wss le metemos el contador
  } #i es un contador
  plot(1:nc,wss,type='b',xlab='number of clusters',ylab = 'within groups sum of squares')
}   #peso por desviaciones, con numero de control de 15 y un avance de 1234
#wss, es la multiplicacion de filas y matrices, para sacar la varianza
wssplot(wine.stand, nc=12)
library(cluster)
clusplot(wine.stand, k.means.fit$cluster, main="2D representation of the cluster solution", color=TRUE, shade =TRUE, label=2, lines=0) #con dos comillas el titulo

table(wine[,1],k.means.fit$cluster)

d<-dist(wine.stand, method="euclidean")
H.fit<-hclust(d,method = "ward.D")
plot(H.fit)

rect.hclust(H.fit, k=3, border="red")
rect.hclust(H.fit, k=5, border="green")
rect.hclust(H.fit, k=8, border="blue")