ploter<- function(u,iter)
{  
  v<-vector(length=iter-1)
  w<-vector(length=iter-1)
  for(i in 1:iter-1)
  {
    v[i]=u[i+1]
    w[i]=u[i]
  }
  plot(v,w)
}

lcg<-function(iter,a,x0,m)
{  
  u<-vector(length = iter)
  u[1]<-x0/m
  for (i in 1:iter)
  {
    x1<-(a*x0)%%m
    x0<-x1
    u[i+1]<-x1/m
  }
  hist(u,col="light cyan", breaks=50)
  ploter(u,iter)
}  
iter=10000
lcg(iter, 16807, 1, 2^31-1)


############################

#Definimos la funcion que vamos a plotear
ploter<-function(u,iter)
{
  v<-vector(length = iter - 1)
  w<-vector(lenght = iter - 1)
  for(i in 1:iter-1)
  {
    v[i]=u[i+1]
    w[i]=u[i]
  }
  plot(v,w)
}
#A continuacion queremos crear una funcion aleatoria y variable
lcg<-function(iter,a,x0,m)
{
  u<-vector(length=iter)
  u[1]<-x0/m
  for (i in 1:iter)
    #El doble %% sirve para expresar el m?dulo
  {x1<-(a*x0 )%%m
  x0<-x1
  #A continuacion metemos el variador de la u
  u[i+1]<-x1/m
  }
  hist(u, col="light cyan", breaks=50)
  ploter(u,iter)
}
#A continuacion tenemos que definir las condiciones de contorno
#En primer lugar el numero de iteraciones
iter = 100000
#A continuacion llamamos a la funcion
lcg(iter,16807,1,2^31 - 1)

#A continuacion vamos a eliminar ruido mediante "cat"
cat("\n")

lcg(iter, 40692,1,21474833399)
box<-function(mean, variance)
{
  u1<-runif(5000)
  r<--2*log(u1)
  u2<-runif(5000)
  th<-2*pi*u2
  z1<-sqrt(r)*cos(th)
  z2<-sqrt(r)*sin(th)
  z<-vector(length = 10000)
  i=1
  j=1
  while(i<=10000)
  {
    z[i]=z1[j]
    i=i+1
    z[i]=z2[j]
    j=j+1
    i=i+1
  }
  z<-z*variance+mean
  hist(z,main="box-muller", breaks=50)
  cat(mean(z),"",var(z),"\n")
}
system.time(box(5,5))


Brownian<-function() # This is a function to generate Browninan with drift 0.06 and diffusion 0.3
{
  paths<-10
  count<-5000
  interval<-5/count
  sample<-matrix(0,nrow=(count+1),ncol=paths)
  for(i in 1:paths)
  {
    sample[1,i]<-5
    for(j in 2:(count+1))
    {
      sample[j,i]<-sample[j-1,i]+interval*0.06+((interval)^.5)*rnorm(1)*0.3
    }
  }	
  cat("E[W(2)] = ",mean(sample[2001,]),"\n")
  cat("E[W(5)] = ",mean(sample[5001,]),"\n")
  matplot(sample,main="Brownian",xlab="Time",ylab="Path",type="l")
}

StandardBrownian<-function() # This is a function to generate Standard Browninan with drift 0 and diffusion 1
{
  paths<-10
  count<-5000
  interval<-5/count
  sample<-matrix(0,nrow=(count+1),ncol=paths)
  for(i in 1:paths)
  {
    sample[1,i]<-0
    for(j in 2:(count+1))
    {
      sample[j,i]<-sample[j-1,i]+((interval)^.5)*rnorm(1)
    }
  }	
  cat("E[W(2)] = ",mean(sample[2001,]),"\n")
  cat("E[W(5)] = ",mean(sample[5001,]),"\n")
  matplot(sample,main="Standard Brownian",xlab="Time",ylab="Path",type="l")
}
StandardBrownian()
Brownian()


#Movimiento browniano geometrico
GeometricBrownian<-function()
{
  paths<-10
  count<-5000
  interval<-5/count
  mean<-0.06
  sigma<-0.3
  sample<-matrix(0,nrow=(count+1),ncol=paths)
  for(i in 1:paths)
  {
    sample[1,i]<-100
    for(j in 2:(count+1))
    {
      sample[j,i]<-sample[j-1,i]*exp(interval*(mean-((sigma)^2)/2)+((interval)^.5)*rnorm(1)*sigma) #Expression for Geometric Brownian Motion
    }
  }	
  cat("E[W(2)] = ",mean(sample[2001,]),"\n")
  cat("E[W(5)] = ",mean(sample[5001,]),"\n")
  matplot(sample,main="Geometric Brownian",xlab="Time",ylab="Path",type="l")
}
GeometricBrownian()

#precios de opciones asiaticas

Asian<-function()
{
  paths<-10
  count<-5000
  C<-0
  S<-0
  K<-80
  interval<-5/count
  r<-0.05
  mean<-0.06
  sigma<-0.3
  sample<-matrix(0,nrow=(count+1),ncol=paths)
  for(i in 1:paths)
  {
    sample[1,i]<-100
    S<-0
    for(j in 2:(count+1))
    {
      S<-S+sample[j-1,i]
      sample[j,i]<-sample[j-1,i]*exp(interval*(mean-((sigma)^2)/2)+((interval)^.5)*rnorm(1)*sigma) #Expression for Geometric Brownian Motion
    }
    S<-S+sample[count+1,i]
    S<-S/(count+1)
    C<-C+(exp(-r*5)*max((S-K),0))
  }
  C<-C/paths
  cat("The Asian Option Price is :::",C)
  cat("\n")
}

Asian()

