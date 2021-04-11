#Creamos una función
#Se define una función, el óptimo se ubica en (0,0)

Func_3<- function(x) {
  x[1]^2 + x[2]^2
}

#Definición de la derivadas
derivada <- function(x) {
  c(2*x[1], 2*x[2])
}

#Definición del método del gradiente
gradient_descent <- function(func, derv, start, step=0.05, tol=1e-8) {
  pt1 <- start
  grdnt <- derv(pt1)
  pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
  
  while (abs(func(pt1)-func(pt2)) > tol) {
    pt1 <- pt2
    grdnt <- derv(pt1)
    pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
    print(func(pt2)) #regresa el valor en el punto
  }
  pt2 # Regresa el valor óptimizado
}

#Método del Gradiente Descendente
resultados4 <- gradient_descent(Func_3, derivada,c(runif(1,-3,3), runif(1,-3,3)), 0.05,1e-8)

#Resumen de resultados
print(resultados4) 
print(Func_3(resultados4)) 

#Gráfica de contorno
x4 <- seq(-3, 3, length.out=100)
y4 <- seq(-3, 3, length.out=100)
z4 <- Func_3(expand.grid(x4, y4))
z4<-as.numeric(unlist(z4))
z4<-matrix(z4,length(x4),length(y4))
contour(x4, y4, matrix(z4, length(x4)), xlab="x",ylab="y")
#Dibujamos el punto óptimo
points(resultados4[1], resultados4[2], col="red", pch=19)
#Dibujamos el cuadrado del óptimo
rect(resultados4[1]-0.2, resultados4[2]-0.2, resultados4[1]+0.2, resultados4[2]+0.2, lwd=2)