par(mfrow=c(1,1))
#############################################
## Probabilities in the Normal Distribution #
###### Biostatistics - ESALQ - 2021.2 #######
#### Beatriz Rodrigues Estevam 11911810 #####
#############################################

?pnorm
# ?qnorm
# ?dnorm

## Cumulative distribution function (CDF)
# value -CDF-> Probability of randomly obtaining an equal or lesser result 


### BASICS ###
## Non- standardized 
# P(X<=10), N(5, 9)
pnorm(10, 5, 3) # X value, mean, standard deviation 
# p(x>10), N(5, 9)
1 - pnorm(10, 5, 3)
# P(X>34), N(30, 16)
a = pnorm(34, 30, sqrt(16))
1 - a 

### STANDARDIZARION ###
## Z-value 
Zvalue = function(x, mean, sd){
  (x-mean)/sd 
}
Zvalue(x = 30, mean = 30, sd = 4)
#Zvalue(x = 34, mean = 30, sd = 4)
#Zvalue(x = 15, mean = 30, sd = 4)
## Standard deviation 
# P(-1.2 < Z < 0.1)  N(0, 1)
a = pnorm(0.1) # X value (upper limit) (Zvalue)
b = pnorm(-1.2) # X value (lower limit) (Zvalue)
a
b
a - b


### INTERVALS ###
## Non- standardized 
# P(92 <= X <= 95), N(100, 20)
a = pnorm(95, 100, sqrt(20)) # X value (upper limit), mean, standard deviation (sqrt(variance))
b = pnorm(92, 100, sqrt(20)) # X value (lower limit), mean, standard deviation 
a - b
# P(34 < X < 42), N(30, 16)
a = pnorm(42, 30, sqrt(16))
b = pnorm(34, 30, sqrt(16))
a - b


### AREA/PROBABILITY -> X and Z values 
# N(75, 25)
qnorm(0.25 , 75 , 5) # % area/probability, mean, standard deviation
# N(0, 1)
qnorm(0.50 , 0 , 1) # area/probability, mean, standard deviation

### Graph ###
graph = function(mean, sd, min, max){
  curve(dnorm(x=x,mean = mean, sd = sd), xlim=c(min, max), lwd=2,col="blue",ylab="f(x)",
      main="Gráfico de X~N(mean, sigma2)")
  abline(v=mean)
}
graph(mean=15, sd=1, min=10, max=20)

##################
### AUTOMATION ###
##################
## Z-value & Probability 
Pz = function(x, mean, sd){
  Z = (x-mean)/sd 
  P_z = pnorm(Z)
  return(data.frame(row.names = c('Zvalue', 'Pz'), c(Z, P_z)))
}
#P(X<49), N(60, 100)
Pz(x=49, mean=60, sd=10)


## Graphs & Z-value & Probability
# Input Non- standardized  data
# x = seq(from=-20, to=95, by=0.25)  # Non-andom sample
x = sort(sample(5000, 10000, replace = TRUE)) # Random sample
x[10000] 
mean(x)

#Function
Pz = function(x, sd, max, min){
  mean=mean(x=x)
  Z = (x-mean)/sd 
  P_z = pnorm(Z)
  
  par(mfrow=c(1,2))
  curve(dnorm(x=x,mean = mean, sd = sd), xlim=c(min, max), lwd=2,col="red",ylab="f(x)",
        main="X~N(mean, sigma2)")
  abline(v=mean)
  
  plot(x = Z, dnorm(x=Z, mean = 0, sd = 1), xlim=c(-10, 10), type='l', lwd=2,
       col="blue",ylab="f(x)", main="Y~N(mean=0, sigma2=1)")
  abline(v=0)
  return(Z[10000]) # To return Z-value 
  #return (P_z[10000]) # To return Probability
}

# Test
Pz(x=x, sd=10, min = 2450, max= 2600)







