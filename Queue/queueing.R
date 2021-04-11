# Load the package
library(queueing)
# Create the inputs for the model.
i_mm1 <- NewInput.MM1(lambda=2, mu=3)
# Optionally check the inputs of the model
CheckInput(i_mm1)
# Create the model
o_mm1 <- QueueingModel(i_mm1)
# Print on the screen a summary of the model
print(summary(o_mm1), digits=2)

gTitle <- "Distribution function of w and wq"
fw <- o_mm1$FW
fwq <- o_mm1$FWq
n <- 10
ty <- "l"
ylab <- "FW(t), FWq(t)"
xlab <- "t"
cols <- c("black", "red")
leg <- c("FW(t)", "FWq(t)")

curve(fw, from=0, to=n, type=ty, ylab=ylab, xlab=xlab, col=cols[1], main=gTitle)
curve(fwq, from=0, to=n, type=ty, col=cols[2], add=T)
legend("bottomright", leg, lty=c(1, 1), col=cols)

L_f_aux <- function(x){L (QueueingModel(NewInput.MM1(lambda=x, mu=1, n=-1)))}
Lq_f_aux <- function(x){Lq (QueueingModel(NewInput.MM1(lambda=x, mu=1, n=-1)))}
Lqq_f_aux <- function(x){Lqq(QueueingModel(NewInput.MM1(lambda=x, mu=1, n=-1)))}
L_f <- function(v){sapply(v, L_f_aux)}
Lq_f <- function(v){sapply(v, Lq_f_aux)}
Lqq_f <- function(v){sapply(v, Lqq_f_aux)}

gt <- "L, Lq and Lqq"
ylab <- "L, Lq, Lqq"
xlab <- "rho"
n <- 100
to <- 0.99
ty <- "l"
lty <- c(1, 1, 1)
cols <- c("blue", "red", "green")
leg <- c("L", "Lq", "Lqq")

curve(L_f, from=0, to=to, n=n, ylab=ylab, xlab=xlab, col=cols[1], type=ty, main=gt)
curve(Lq_f, from=0, to=to, n=n, col=cols[2], add=T, type=ty)
curve(Lqq_f, from=0, to=to, n=n, col=cols[3], add=T, type=ty)
legend("topleft", leg, lty=c(1, 1, 1), col=cols)

x <- seq(from=0, to=0.99, by=0.01)
Lqq_f(x) - L_f(x)

W_f_aux <- function(x){W (QueueingModel(NewInput.MMC(lambda=x, mu=1.01, c=x)))}
Wq_f_aux <- function(x){Wq (QueueingModel(NewInput.MMC(lambda=x, mu=1.01, c=x)))}

W_f <- function(v){sapply(v, W_f_aux)}
Wq_f <- function(v){sapply(v, Wq_f_aux)}
gt <- "W and Wq"
ylab <- "W, Wq"
xlab <- "lambda, c"
n <- 14
ty <- "l"
leg <- c("W", "Wq")
lty <- c(1, 1, 1)
cols <- c("blue", "red")
curve(W_f, from=1, to=n, n=n, ylab=ylab, xlab=xlab , col=cols[1], type=ty, main=gt)
curve(Wq_f, from=1, to=n, n=n, col=cols[2], add=T, type=ty)
legend("topright", leg, lty=lty, col=cols)


o_mm2 <- QueueingModel(NewInput.MMC(lambda=2, mu=3, c=2))
o_mm2k <- QueueingModel(NewInput.MM1K(lambda=2, mu=3, k=5))
CompareQueueingModels(o_mm1, o_mm2, o_mm2k)

.
.
.
.
.
.
.
.
.
.



