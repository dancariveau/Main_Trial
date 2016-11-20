#Week2 

g = 9.8 ## meters per second

h0 = 56.67

v0 = 0

n = 25

tt = seq(0,3.4,len=n) 
y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1) #create the fake data

#Now we act as if we didn't know h0, v0 and -0.5*g and use regression to estimate these. We can rewrite the model as y = b0 + b1 t + b2 t^2 + e and obtain the LSE we have used in this class. Note that g = -2 b2.

X = cbind(1,tt,tt^2) #matrix of observation (y above)

A = solve(crossprod(X))%*%t(X) #RSS
solve(t(X)%*%X)%*%(t(X)%*%y)

betahat <- solve(crossprod(X))%*%crossprod(X,y) 
-2 * (A%*%y)[3]
A[3,3]
A

X <-cbind(rep(10000), tt, tt^2)

DD <- cbind(replicate(10000, (y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)))

            system.time((d1<-replicate(100, y=rnorm(n,sd=1))))
reps <- 50000
nexps <- 5
rate <- 0.1
set.seed(0)
system.time(
  x1 <- replicate(reps, sum(rexp(n=nexps, rate=rate)))
) # replicate
??rexp


generate random variables

testt <- function(g, ho, v0, tt){
  gg <- h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)
  gg
}

rep(gg, 1)

tesst<- function(g, h0, v0, tt){
  y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)
#create the fake data
  X = cbind(1,tt,tt^2)
  A = solve(crossprod(X))%*%t(X)
  v<- -2 * (A%*%y)[3]
}
rep(10, testt(g, h0, v0, tt))
ISS <- sapply(g, gg, h0=65, v0=0)

#Now we act as if we didn't know h0, v0 and -0.5*g and use regression to estimate these. We can rewrite the model as y = b0 + b1 t + b2 t^2 + e and obtain the LSE we have used in this class. Note that g = -2 b2.

B = 100000

g = 9.8 ## meters per second

n = 25

tt = seq(0,3.4,len=n) ##time in secs, t is a base function

X = cbind(1,tt,tt^2)

A = solve(crossprod(X))%*%t(X)

betahat = replicate(B,{    #replicate 10,000 times
  
  y = 56.67 - 0.5*g*tt^2 + rnorm(n,sd=1) # get the y values with random error
  
  betahats = -2*A%*%y #calculate rss
  
  return(betahats[3]) #
  
})

sqrt(mean( (betahat-mean(betahat) )^2)) #SE

x = father.son$fheight
x
