#Cadena de Markov
library(markovchain)
weatherStates<-c("sunny", "cloudy", "rain")
byRow<-TRUE
weatherMatrix<-matrix(data=c(0.7,0.2,0.1,
                             0.3,0.4,0.3,
                             0.2,0.45,0.35), byrow=byRow, nrow=3,
                      dimnames=list(weatherStates, weatherStates))  # creamos la matriz
mcWeather <- new("markovchain", states = weatherStates, byrow = byRow, 
                 transitionMatrix = weatherMatrix, name = "Weather")
defaultMc <- new("markovchain")
plot(mcWeather)  # Representamos graficamente la matriz de transferencia
mcList <- new("markovchainList", markovchains = list(mcWeather, defaultMc), 
              name = "A list of Markov chains")

# Empezamos con un nublado
initialState <- c(0, 1, 0)
after2Days <- initialState * (mcWeather * mcWeather)
after2Days
after7Days <- initialState * (mcWeather ^ 7)
after7Days
round(after7Days, 3)

# Pedimos que saque la matriz
print(mcWeather)

# lo convertimos en un dataframe (tabla)
mcDf <- as(mcWeather, "data.frame")
mcNew <- as(mcDf, "markovchain")
mcDf

mcIgraph <- as(mcWeather, "igraph")
myMatr<-matrix(c(.1,.8,.1,.2,.6,.2,.3,.4,.3), byrow=TRUE, ncol=3)
myMc<-as(myMatr, "markovchain")
myMc  # le llama unnamed markovchain, nos lo ha redondeado
plot(myMc)




weatherStates <- c("sunny", "cloudy", "rain")
byRow <- TRUE
weatherMatrix <- matrix(data = c(0.7, 0.2, 0.1, 
                                 0.3, 0.4, 0.3, 
                                 0.2, 0.4, 0.4), 
                        byrow = byRow, nrow = 3, 
                        dimnames = list(weatherStates, weatherStates))
mcWeather <- new("markovchain", states = weatherStates, 
                 byrow = byRow, transitionMatrix = weatherMatrix, 
                 name = "Weather")      
weathersOfDays <- rmarkovchain(n = 365, object = mcWeather, t0 = "sunny")
