#q6
# th de la limite centrale
LoiBinomiale <- function(n,p,graine)
{
  k<-0
  somme <- 0 
  set.seed(graine)
  u <- runif(1)
  while(u > somme){
    k = k + 1
    somme = somme +bin(k-1,n,p) # k ou k-1 !!!
  }
  return (k)
}

 bin<- function(k,n,p){
   c = factorial(n)/(factorial(k)*factorial(n-k))
   return ( c*(p^k)*((1-p)^(n-k)))
 }
 
 
 for (i in 1:1000) {
   a[i] <- LoiBinomiale(100,0.9,i)
 }
 
 hist(a) # ressemble un peu a une gaussienne quand n: nb de reqlisations est grand 
 plot(table(a))
 
 # Question 7
 
LoiNormale <-function (n, p, graine){
  
  set.seed(graine)
  u <- runif(1)
  set.seed(graine/2)
  v <- runif(1)
  x <- sqrt(-log(u))*cos(2*pi*v)
  x <- n*p + x*sqrt(n*p*(1-p))
  return (x)
}
 
for (i in 1:1000) {
  b[i] <- LoiNormale(100,0.5,i)
}

hist(b) #une gaussienne youpi

#Question 8

FileMM1 <- function( lambda, mu, D)
{
  tota= 0
  arrivee = c()
  depart = c()
  file = 0
  while (tota < D)
  {
    t = rexp ( lambda)
    tota = tota + t
    if ( tota <= D)
    {
      arrivee[file] = tota
      file = file +1
    }  
  }
  
  totd = arrivee[1]
  i = 1;
  
  while ( totd < D && file >0)
  {
    d = rexp(mu)
    if ( totd >= arrivee[i]){
      totd = totd + d 
    }
    else
    {
      totd = d + arrivee [i]
    }
    
    if (totd <= D )
    {
      depart [i] <- totd
      i = i+1
      file = file -1
    }
  }
  res = list(arr= arrivee,dep=depart)
  return (res)
}

res = FileMM1(30000,10,10000)
print(res$arr)
print(res$dep)

#Question 9

EtatFile <- function( depart, arrivee)
{
  d = 1
  a = 1
  etat = c()
  etat[1] = 0
  i = 2
  while ( d <= length(depart) && a <=length(arrivee) )
  {
    if ( depart[d] < arrivee [a]){
      etat[i] = etat[i-1] -1
      d = d+1
    }
    else
    {
      etat[i] = etat[i-1] +1
      a = a+1
    }
    i = i+1
  }
  
  if ( d > length(depart))
  {
    while ( a <= length (arrivee) ){
      etat[i] = etat[i-1] +1
      i = i+1
      a = a +1
    }
  }  
  else
  {
    while ( d <= length (depart) ){
      etat[i] = etat[i-1] +1
      i = i+1
      d = d+1
    }
  }
  return (etat)
}

a = EtatFile ( res$dep, res$arr)
print(a)
plot(a)