#Un pastelero dispone de 150 kg de harina, 22 kg de azúcar y 27.5 kg de mantequilla para elaborar dos tipos de pasteles (A y B).
#Cada caja de pasteles de tipo A requiere 3 kg de harina, 1 kg de azúcar y 1 kg de mantequilla y su venta le reporta un bene???cio de 20 euros.
#Cada caja de pasteles de tipo B requiere 6 kg de harina, 0.5 kg de azúcar y 1 kg de mantequilla y su venta le reporta un bene???cio de 30 euros.
#¿Cuántas cajas de cada tipo debe elaborar el pastelero de manera que se maximicen sus ganancias? (Se supone en principio que también puede
#elaborar cajas incompletas, es decir, que no se trata de un problema de programación entera.) 

library(lpSolve)

#parametros del problema
coef <- c(20,30)
A <- matrix(c(3,1,1,6,0.5,1),ncol=2)
b <- c(150,22,27.5)
dir <- rep('<=',3)

#Solucion
solucion <-lp('max', coef, A, dir, b)

solucion$objval #maximo beneficio

solucion$solution #optimo de produccion

#programacion entera
solucion <- lp('max', coef, A, dir, b, all.int=TRUE)
solucion$objval
solucion$solution

###################################

#Problema de transporte
cost <- matrix(c(3, 2, 2, 3, 4, 2, 2, 3, 6, 4, 2, 2, 8, 5, 3, 4, 9, 5, 3, 2), ncol=5)
direction <- 'min'
row.signs <- rep('=', 4)
row.rhs <- c(30, 80, 10, 60)
col.signs <- rep('=', 5)
col.rhs <- c(10, 50, 20, 80, 20)
lp.transport(cost, direction, row.signs, row.rhs, col.signs, col.rhs)
lp.transport(cost, direction, row.signs, row.rhs, col.signs, col.rhs)$solution #para sacar la solucion de todas

###################################

#Problema de asignacion de tareas
cost <- matrix(c(15, 9, 10, 10, 15, 12, 9, 10, 8), nrow=3)
lp.assign(cost)
lp.assign(cost)$solution

##################################

library(linprog) 
#parametros del problema
coef <- c(20, 30) 
A <- matrix(c(3, 1, 1, 6, 0.5, 1), ncol=2) 
b <- c(150, 22, 27.5) 
dir <- rep('<=', 3)


# Solucion 
solucion <- solveLP(coef, b, A, maximum=TRUE, dir) 
summary(solucion)

solveLP(coef, b, A, maximum=TRUE, dir, verbose=4)





