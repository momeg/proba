#install.packages('randtoolbox')

#library(randtoolbox)

MersenneTwister <- function(n,p=1,graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(list(x=x,s=x[n]))
}

Sobol <- function(n,p) 
{
  return(round(sobol(n,p)*(2^31-1)))
}

#Question 1

RANDU <- function (k, graine)
{
	x <- rep(0,k)
	for (i in 1:k) {
		graine <- (graine*65539)%%(2^31)
		x[i] <- graine
	}
	return(x)
}

StandardMinimal<- function (k, graine)
{
  x <- rep(0,k)
  for (i in 1:k) {
    graine <- (graine*16807)%%((2^31) -1)
    x[i] <- graine
  }
  return(x)
}

#Question 2.1

hist( RANDU(1000, 200)) # mieux que StandMin
hist ( StandardMinimal(1000, 200))
hist(Sobol(1000,1)) #Le meilleur
hist(unlist(MersenneTwister(1000, 1, 200)))

#Question 2.2
u <- RANDU(1000, 200)
plot(u[1:(1000-1)], u[2:1000])
u <- StandardMinimal(1000, 200)
plot(u[1:(1000-1)], u[2:1000])
u <- Sobol(1000,1)
plot(u[1:(1000-1)], u[2:1000]) # Le plus naze
u <- unlist(MersenneTwister(1000, 1, 200))
plot(u[1:(1000-1)], u[2:1000])

#Question 3

binary <- function(x)
{
  if((x<2^31)&(x>0))
    return( as.integer(rev(intToBits(as.integer(x)))) )
  else{
    if((x<2^32)&(x>0))
      return( c(1,binary(x-2^31)[2:32]) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}


Frequency <- function(x, nb)
{
  s <- 0
  for( i in 1:length(x)){
    b <- binary(x[i])
    for (j in 1:nb){
      s <- s+ (2*b[32+1-j])-1
    }
  }
  sobs = abs(s)/ sqrt(nb*length(x))
  Pvaleur = 2*(1-pnorm(sobs))
  return ( Pvaleur)
}

set.seed(200)
x <- sample.int(100, 100)

a <- rep(0,100)
b <- rep(0,100)
c <- rep(0,100)

for (i in 1:100) {
  a[i] <- Frequency( RANDU(1000, x[i]), 32)
  b[i] <- Frequency( StandardMinimal(1000, x[i]), 32)
  c[i] <- Frequency(unlist(MersenneTwister(1000, 1, x[i])), 32)
}
Frequency(Sobol(1000,1), 32)

#hist(a)
#hist(b)
#hist(c)

#print(mean(a))
#print(mean(b))
#print(mean(c))

#Question 4

Runs <- function(x,nb)
{

  #Pretest
  n <- length(x)*nb
  nbtrue <- 0
  for( i in 1:length(x)){
    b <- binary(x[i])
    for (j in 1:nb){
      nbtrue <- nbtrue+b[32+1-j];
    }
  }
  pi = nbtrue/n

  
  if ( abs(pi- (1/2))>= 2/sqrt(n)){
    return(0)
  }
  

  #Statistique

  #Cas général
  r<-1
  if ( length(x) != 1)
  {
    for( i in 1:(length(x)-1)){
      b <- binary(x[i])
      u <- binary(x[i+1])
      nex <- u[32+1-nb]
      for (j in 2:nb){
        if(b[32+1-j] != b[32+2-j]){
          r<-r+1
        }
        
        #Cas du dernier bit du vecteur
        if(b[32] != next ){
          r<-r+1
        }
      }
    }
  }
  
  # Cas du dernier vecteur
  b<- binary(x[length(x)])
  for (j in 2:nb){
    if(b[32+1-j] !=b[32+2-j] ){
      r<-r+1
    }
  }
  #Calcul Pvaleur
  
  PValeur = 2*(1-pnorm((abs(r-2*pi*n*(1-pi)))/(2*sqrt(n)*pi*(1-pi))))

    
  return ( PValeur)

}

# Test Runs

set.seed(200)
x <- sample.int(100, 100)

a <- rep(0,100)
b <- rep(0,100)
c <- rep(0,100)

for (i in 1:100) {
  a[i] <- Runs( RANDU(1000, x[i]), 32)
  b[i] <- Runs( StandardMinimal(1000, x[i]), 32)
  c[i] <- Runs(unlist(MersenneTwister(1000, 1, x[i])), 32)
}
#print(Runs(Sobol(1000,1), 32))

#hist(a)
#hist(b)
#hist(c)

#print(mean(a))
#print(mean(b))
#print(mean(c))


#q5

set.seed(200)
x <- sample.int(100, 100)

a <- rep(0,100)
b <- rep(0,100)
c <- rep(0,100)

for (i in 1:100) {
  a[i] <- order.test(RANDU(1000, 200), d=4, echo=FALSE)$p.value
  b[i] <- order.test(StandardMinimal(1000, 200),d=4, echo=FALSE)$p.value
  c[i] <- order.test(as.vector(MersenneTwister(1000, 1, 200)$x),d=4, echo=FALSE)$p.value
}

print(mean(a))
print(mean(b))
print(mean(c))
print(order.test(Sobol(1000, 1), d=4, echo = FALSE)$p.value)

