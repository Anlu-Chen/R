fn <- function(para)
{# Vector of the parameters
  matrix.A <- matrix(para, ncol=2) 
  x <- matrix.A[,1] 
  y <- matrix.A[,2] 
  f.x <- (x^2+y-11)^2+(x+y^2-7)^2
  return(f.x)
}
  
par <- c(1,1)

xy <- as.matrix(expand.grid(seq(-5,5,length = 101),seq(-5,5,length = 101)))
colnames(xy) <- c("x", "y")
df <- data.frame(fnxy = fn(xy), xy)

library(lattice) 

wireframe(fnxy ~ x*y, data = df, shade = TRUE, drape=FALSE,
          scales = list(arrows = FALSE),
          screen = list(z=-240, x=-70, y=0))



