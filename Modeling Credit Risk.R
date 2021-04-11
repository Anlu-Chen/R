lossrealization<-function (n,PD,EAD,LGD,rho){
  # Keep track of the loss in this portfolio .
  totalloss <- 0
  # Draw a normal random variable for the # systematic factor .
  sf <- rnorm(1)
  # Loop through all obligors to see if they # go into default .
  for(obligor in 1:n){
    # Draw specific factor .
    of <- rnorm(1)
    # Asset value for this obligor .
    x <- sqrt(rho)*sf + sqrt(1-rho)*of
    # Critical threshold for this obligor .
    c <- qnorm(PD[obligor])
    # check for default .
    if(x < c){
      totalloss <- totalloss + EAD[obligor] * LGD[obligor];
    }
  }
  return(totalloss)
}

n <- 100 # The number of obligors in the portfolio.
runs <- 5000 # Number of realizations.
# Run a number of realizations .
EAD <- rep (1,n)
LGD <- rep (1,n)
PD <- rep (0.25,n)
rho <- 0.2
losses <- c()
for(run in 1:runs){
  # Add a new realiza/on of the loss variable .
  losses <- c(losses,lossrealization(n,PD,EAD,LGD,rho))
}
# Output : normalised histogram .
hist(losses, freq =FALSE , main =" Histogram of Loss ", xlab =" Loss ", ylab =" Density ");


alpha <- 0.95 # Alpha level
# Sort losses and select right value
losses <- sort(losses)
j <- floor(alpha*runs)
var_value <- losses[j]
# Select the losses that are larger than VaR
largelosses <- losses[losses >= var_value]
# Output TCE
ES <- mean (largelosses)



