#Función Rosenbrock
#La función es f(x,y)=(1-x)^2+100*(y-x^2)^2

nx<-21
ny<-21

x<-seq(-5,5,length=nx)
y<-seq(-5,5,length=ny)

#Se genera la salida de los valores en z

z<-outer(x,y,function(x,y)(1-x)^2+100*(y-x^2)^2)

#Escala de colores
hgt <- 0.25 * (z[-nx,-ny] + z[-1,-ny] + z[-nx,-1] + z[-1,-1])
hgt <- (hgt - min(hgt))/ (max(hgt) - min(hgt))

#Se construye la gráfica en perspectiva con cierto ángulo para visualizarla
persp(x, y, z, col =cm.colors(10)[floor(9* hgt + 1)],theta = 35,phi=30)