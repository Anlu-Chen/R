#Creamos una funci�n
#Definici�n de una funci�n Rosenbrock, el optimo de ubica en (1,1)

rosenbrock <- function(v)
{ 
  #Valores de a=1, b=100
  (1 - v[1])^2 + 100 * (v[2] - v[1]*v[1])^2
}

# Identificaci�n del m�nimo local por el m�todo Nelder-Mead 
resultado3 <- optim(c(runif(1,-3,3), runif(1,-3,3)),rosenbrock,NULL,method="Nelder-Mead",control=c( maxit=100,reltol=1e-8,alpha=1.0, beta=0.5,gamma=2.0))

#Revisi�n de resultados
print(resultado3$par) 
print(resultado3$value) 
print(resultado3$counts)

#Gr�fica de cortorno
x3<- seq(-3, 3,length.out = 100)
x3
y3<- seq(-3, 3,length.out = 100)
y3
z3<- rosenbrock(expand.grid(x=x3, y=y3))
z3<-as.numeric(unlist(z3))
z3<-matrix(z3,length(x3),length(y3))
z3
contour(x3, y3, matrix(log10(z3), length(x3)), xlab="Valores de X",ylab="Valores de Y")
#Punto �ptimo
points(resultado3$par[1], resultado3$par[2], col="red", pch=19)
#�bicaci�n del punto �ptimo
rect(resultado3$par[1]-0.2, resultado3$par[2]-0.2, resultado3$par[1]+0.2, resultado3$par[2]+0.2, lwd=2)