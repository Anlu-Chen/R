#Construimos la rejilla base por simetria
q2 = matrix(0,18,18)#es una matriz de 18X18 llena de "0"
q2[8:9,8:9] = .3#como estamos entre corchetes seleccionamos los puntos 8 a 9 y sustituimos 0, por 0.3
q2[17:18,17:18] = 1/2# lo mismo que antes en los puntos 17 a 18 sustituimos los ceros por 0.5
q1 <- q2[,18:1]#hacemos que q1 se forme con q2 tomando un vector vacio, y los valores 0.5 pasan a las posiciones 1 y 2 al poner 18,1, en vez de 1,18
top <- cbind(q2,q1)#unimos ambas vectores(matrices)de forma que se aprovecha la simetria matricial
z <- rbind(top,top[18:1,])# launión annterior era por columnas y ahora esta es por filas, pero patiendo de "top"
#podeis comprobar marcando "z", que ahora los valores de 0.3 y 0.5 están en lados iniciales y en los simétricos de la matriz

library(rgl)#hay que descargarla
x<-(1:36)/36
y<-(1:36)/36
#x e y son divisiones equidistantes de 1/36, de 1 a 36, es decir, el último número es 1
open3d()
#visor 3D que tiene R
rgl.surface(x, y, z, color="blue", alpha=0.75, smooth=TRUE)
#creamos las superficie de las patas de apoyo y la base, se unen los puntos con una superficie que los contiene
rgl.surface(x, y, matrix(1/2,36,36), color="red", front="lines", back="lines")
#creamos una malla, que es la que caerá encima de los postes,minimizando su energía, es una rejilla de 1/2
#de 36x36 rejillas

nr <- nrow(z)
nc <- ncol(z)
N <- nr*nc
bvec <- matrix(z,N,1,byrow=FALSE)
#bvec, es un vector columna, 1x 36x36 que tiene valores de z
Laplacian2D <- function(lx,ly, nx,ny){
  hx <- lx/(nx-1)
  hy <- ly/(ny-1)# estamos creando un algoritmo para calcular el gradiente en los puntos de la rejilla nx ny
  tr_x<-c(2, -1, rep(0, nx-2))
  tr_y<-c(2,-1, rep(0,ny-2))# creamos el vector de coordenadas 2, -1, y repetimos (0,ni-2)
  TX<-toeplitz(tr_x)/(hx^2)
  TY<-toeplitz(tr_y)/(hy^2)# Toeplitz crea un vector vertical del vector creado antes dividido por (lx/nx-1)^2
  lx<-diag(nx)
  ly<-diag(ny)
  L<-kronecker(Tx,ly)+kronecker(lx,Ty)
  return(L)}
#primero obtenemos lx y ly que son las diagonales principal e inversa de la matriz
#kronecker multiplica los vectores entre paréntesis  