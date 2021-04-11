#Creamos una función
#Ejemplo 1-Mínimo de una función

Func_1<-function(x)
{
  x^2 
}

resultado<-optimize(Func_1,c(-5,5),maximum=FALSE,tol=1e-8) 

#Vemos los resultados
print(resultado$minimum)
print(resultado$objective)

#Generamos el rango de valores para x y y=f(x)
x<-seq(-5,5,length.out=100)
y<-Func_1(x)

#Graficamos identificando el mínimo
plot(x,y,xlab="x",ylab="f(x)",col="4")
points(resultado$minimum,resultado$objective, col="red",pch=19)
rect(resultado$minimum-0.3, resultado$objective-0.7, resultado$minimum+0.3,resultado$objective+0.7)

###############################

#Ejemplo 2
#Creamos una función
Func_2<-function(x)
{
  x^2+6*sin(x*pi*2)
}

#Optimizamos
resultado2_1<-optimize(Func_2,c(-5,5),lower="-1",upper="3",maximum=FALSE,tol=.Machine$double.eps^0.25) 
resultado2<-optimize(Func_2,c(-5,5),maximum=FALSE,tol=1e-8)
#Vemos los resultados
print(resultado2$minimum)
print(resultado2$objetive)

#Rango de calores para nuestras gráficas 
x2<-seq(-5,5,length.out=100)
y2<-Func_2(expand.grid(x2)) 

#Graficamos
plot(x2,as.matrix(y2)[,1],xlab="x",ylab="f(x)",col="4")
points(resultado2$minimum,resultado2$objective, col="red",pch=19)
rect(resultado2$minimum-0.3, resultado2$objective-0.7, resultado2$minimum+0.3,resultado2$objective+0.7)

points(resultado2_1$minimum,resultado2_1$objective,col="green",pch=19)
rect(resultado2_1$minimum-0.3,resultado2_1$objective-0.7,resultado2_1$minimum+0.3,resultado2_1$objective+0.7)

############################

