set.seed(5623)
t_llegada <- (1:8)
t_servicio <- (1:6)
pllegada <- c(0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125)
pservicio <- c(0.1,0.2,0.3,0.25,0.1,0.05)
sim <- NULL

##Se dimensionan las variables:
min.llegada <- numeric(20)
min.salida <- numeric(20)
cuenta.servicio <- numeric(20)
tiempo.cola <- numeric(20)
tiempo.sistema <- numeric(20)
tservidor.disponible <- numeric(20)
tservicio.llegada<-numeric(20)


##Se realiza el conteo para el primer cliente:

tllegadas <- sample(t_llegada,size=1,replace = TRUE,prob=pllegada) ## Saca una muestra del tiempo entre llegadas
tservicio <- sample(t_servicio,size=1,replace = TRUE,prob=pservicio)## Saca una muestra del tiempo del servicio

min.llegada[1]<- 0
cuenta.servicio[1]<- 0
min.salida[1]<-cuenta.servicio[1]+tservicio[1]
tiempo.cola[1] <- cuenta.servicio[1]-min.llegada[1]
tiempo.sistema[1] <- tiempo.cola[1]+tservicio[1]
tservidor.disponible[1] <- 0

###Se guarda en data frame "Sim"
nuevo <- data.frame (cliente=1,Tiempo_desde_ultima_llegada=0,Tiempo_servicio=tservicio [1],minuto_llegada=min.llegada[1],minuto_inicio_del_servicio=cuenta.servicio[1], minuto_salida_del_cliente=min.salida[1]) 
sim <- rbind(sim, nuevo)

##Se construlle el sistema para los demás clientes 
for (c in 2:20){
  tllegadas[c] <- sample(t_llegada,size=1,replace = TRUE,prob=pllegada)
  tservicio[c] <- sample(t_servicio,size=1,replace = TRUE,prob=pservicio)
  
  min.llegada[c]<- tllegadas[c]+min.llegada[c-1]
  tservicio.llegada[c]<-tservicio[c]-tllegadas[c]
  
  if(min.llegada[c]>min.salida[c-1]){
    cuenta.servicio[c]<-min.llegada[c]
  }else{cuenta.servicio[c]<-min.salida[c-1]}
  
  min.salida[c]<-cuenta.servicio[c]+tservicio[c]
  tservidor.disponible[c] <-cuenta.servicio[c]-min.salida[c-1]
  tiempo.cola[c] <- cuenta.servicio[c]-min.llegada[c]
  tiempo.sistema[c] <- tiempo.cola[c]+tservicio[c]
  
  nuevo <- data.frame (cliente=c,Tiempo_desde_ultima_llegada=tllegadas[c],Tiempo_servicio=tservicio [c],
                       minuto_llegada=min.llegada[c],minuto_inicio_del_servicio=cuenta.servicio[c], minuto_salida_del_cliente=min.salida[c]) 
  sim <- rbind(sim, nuevo)
}

print(sim)

T.Prom.de.espera <- mean(tiempo.cola)## Es el tiempo promedio de espera de un cliente en la cola.
T.disponible <- sum(tservidor.disponible) ##Es el tiempo que el servidor pasa desocupado.
T.Prom.de.servicio <- mean(tservicio)## Es el tiempo promedio de cada servicio en el servidor. 
T.Prom.del.cliente <- mean(tiempo.sistema)##Es el tiempo promedio que un cliente pasa en todo el sistema, tomando en cuenta tiempo de cola y de servicio.
T.entre.llegadaas <- mean(tllegadas)##es el tiempo promedio entre llegadas del sistema. 


tasa.de.servicio<-1/(mean(tservicio))##Es la cantidad promedio de servicios por minuto
tasa.de.llegada<-20/min.llegada[20] ##Es la cantidad promedio de llegadas por minuto
factor.de.utilizacion<-tasa.de.llegada/tasa.de.servicio ##Representa que tan utilizado se encuentra el sistema y la probabilidad de esperar para ser atendido.

result <- data.frame (Tiempo.de.promedio.espera = T.Prom.de.espera, 
                      Tiempo.servidor.disponible =T.disponible, 
                      Tiempo.promedio.servicio = T.Prom.de.servicio, 
                      Tiempo.promedio.de.cliente.en.sistema=T.Prom.del.cliente,
                      Tiempo.promedio.llegada=T.entre.llegadaas,
                      Utilizacion.del.Sistema=factor.de.utilizacion)

##Se traspone la matriz y se muestra
print(t(result))

