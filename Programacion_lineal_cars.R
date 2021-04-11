#fabricante de coches, modela A y B
#projecciones a largo plazo
#100->A,, 80->B,, max produccion 200A y 170B
#al menos 200 coches diarios, venta A, perdidas de 2000
#venta de B ganancias de 5000. cuantos a maximizar

func.ojt<-c(5000,-2000)
constr<-matrix(c(1,1,1,0,0,1),ncol=2,byrow=TRUE)
constr.dir<-c('=','>=','>=')
constr.rhs<-c(200,80,100)
mod<-lp("max",func.ojt, constr,constr.dir, constr.rhs, compute.sens=TRUE)
mod$solution
mod$objval

#####################

#Una empresa quiere maximizar el beneficio vendiendo 2 productos a y b
#venden a 25 y 30 eur. hay materia proma para producir 1800
#A requiere 20 uds, B requiere 12 uds.
#Cada producto necesita 4min para montaje de las 8h diarias.
#cumpliendo estas restricciones maximizar el beneficio

func.ojt<-c(25,30)
constr<-matrix(c(20,12,4,4),ncol=2,byrow=TRUE)
constr.dir<-c('<=','<=')
constr.rhs<-c(1800,480)
mod<-lp("max",func.ojt, constr,constr.dir, constr.rhs, compute.sens=TRUE)
mod$solution
mod$objval

#otra manera de hacero
objetivos<-c(25,20)
A<-matrix(c(20,12,4,4), ncol = 2, byrow=TRUE)
recursos<-1800
tiempo<-(8*60)
const.dir<-c('<=','<=')
const.rhs<-c(recursos,tiempo)
optimo<-lp(direction='max', objetivos,A,const.dir,const.rhs)
optimo$solution
optimo$objval
