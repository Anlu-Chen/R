##comando para decir que voy a usar los datos para hacer calculos
datalibra("USArrests")

##visualizacion
USArrests

##si solo vamos a usar esta base de datos la fijamos
attach("USArrests")

library(cluster)
library(magrittr)
#library de los coleguis
library(factoextra)

#·mi fichero
#adimensionalizamos
#lo ponemos asi porque no podemos poner funciones en la misma linea
#%>% tiene la misma funcion que +

my.data<-USArrests %>% #%>% es el comando de seguir poniendo cosas
  na.omit() %>% #todos los na se omitan
  scale() #escalamiento (te convierte los numeros en comparables)

##para ver una previsualizacion de que pinta tiene la base de datos
#n=10 es el numero de filas que te va a enseñar
head(my.data,n=10)

##distancia entre numeros
#stand=TRUE para que no se pare en la primera distancia sino que lo extienda a todas las distancias
#"pearson"
#para calcular una matriz de distancia entre las filas de una matriz de datos. 
#Comparado con la función dist () estándar, soporta medidas de distancia basadas en la correlación incluyendo los métodos “pearson”, “kendall” y “spearman”.
res.dist<-get_dist(USArrests,stand=TRUE, method="pearson")

#lo mostramos
res.dist

##mapa de calor
#colores rojos es donde hay mucha hostilidad y azul es una hostilidad muy baja
#hay muchos estados como nueva york.
#podemos establecer relacion entre diferentes estados.
#para visualizar una matriz de distancia.
#rojo:distancias grandes, lo que quiere decir que un sitio se ha matado mucho y otro mucho y el azul que la matanza es similar
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#se la aplicamos a my_data , por el metodo de kmedias
#vemos que el valor de 4 clusters te da probabilidad mas alta por lo que es el valor optimo
fviz_nbclust(my.data, kmeans, method = "gap_stat")

#cluster de kmedias
set.seed(123)
km.res <- kmeans(my.data, 3, nstart = 25) #3 =nºclusters , nstart=25 iteraciones

#plot de los datos
fviz_cluster(km.res, data = my.data, ellipse.type = "convex",pallete="jco",ggtheme = theme_minimal())

#DENDOGRAMA
res.hc<-USArrests %>%
  scale()%>%
  dist(method="euclidean")%>%
  hclust(method="ward.D2")

fviz_dend(res.hc,k=4,cex=0.5, k_colors=c("#2E9FDF","#00AFBB","#E7B800","#FC4E07"),color_labels_by_k=TRUE, rect=TRUE)

#############################################

#matriz de gradiente (como si fuese derivada)
gradient.color<-list(low="steelblue",high="white")
iris[,-5] %>%
  get_clust_tendency(n=50,gradient=gradient.color)
#no va a haber cambio en la tendencia ya que es la derivada de las variables
#$hopkins_stat cercano a 1 es muy buen analisis

#########################################################

#cargamos otra libreria de los pivitos
install.packages("NbClust")
library("NbClust")

#Para ver el numero de cluster optimos
#esta tecnica es mucho mejor
rest.nbclust<-USArrests %>%
  scale() %>%
    NbClust(distance="euclidean",
    min.nc = 2,max.nc=10,
    method="complete",index ="all")



#te saca el histograma--> de las conclusiones y por lo tanto, segun el histograma el numero de cluster optimo es 2
#que ha sido la solucion 
factoextra::fviz_nbclust(rest.nbclust)

#dendograma
set.seed(123)
my.data <- scale(iris[, -5])
res.hc <- eclust(my.data, "hclust", k = 3, graph = FALSE)
fviz_dend(res.hc, rect = TRUE, show_labels = FALSE)



