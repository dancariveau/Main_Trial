library(UsingR)

x = father.son$fheight

y = father.son$sheight

n = length(y)

#Now let's run a Monte Carlo simulation in which we take a sample of size 50 over and over again. Here is how we obtain one sample:

N = 50

index = sample(n,N)
index
sampledat = father.son[index,]

x = sampledat$fheight

y = sampledat$sheight

betahat = lm(y~x)$coef


betahat1 = replicate(100,{
  
  yy = lm(y~x)$coef
  yy[2]
  
})
