# Foraging Index
#obs: observed frequency of visits to item 1
#exp: expected frequency of visits to item 1 (i.e. resource abundance/availability)
# per bee species sum(obs) over all plants
fr <- function(obs, exp){
  p_obs <- obs/sum(obs)
  p_exp <- exp/sum(exp)
  out <- p_obs/p_exp
  out
}


electivity <- function(obs, exp){
  p_obs <- obs/sum(obs)
  p_exp <- exp/sum(exp)
  out <- (p_obs-p_exp)/(p_obs+p_exp)
}

jacobs <- function(obs, exp){
  p_obs <- obs/sum(obs)
  p_exp <- exp/sum(exp)
  out <- (p_obs-p_exp)/(p_obs+p_exp-2*p_obs*p_exp)
  out
}

#do not tell you if pollinators have general preferences over several items
#and which of this items are preferred or not

#The Chi test Pearson residuals (obs-exp/√exp) estimate the magnitude of preference or avoidance 
#for a given item based on deviation from expected value

chi_pref <- function(obs, exp, alpha = 0.05){
  chi <- chisq.test(obs, p = exp, rescale.p = TRUE)
  print(chi) #tells you if there is an overall preference. (sig = pref)
  res <- chi$residuals
  #res <- (obs-exp)/sqrt(exp) #hand calculation, same result.
  #calculate bonferoni Z-statistic for each plant.
  alpha <- alpha
  k <- length(obs) #number of plants (things observed)
  n <- sum(obs) #total number of observations
  p_obs <- obs/n #divides number of plants (things observed) by each observation value
  ak <- alpha/(2*k)
  Zak <- abs(qnorm(ak)) #area that is above/below 0.05 in a probability distribution 
  low_interval <- p_obs - (Zak*(sqrt(p_obs*(1-p_obs)/n))) #observation minus
  upper_interval <- p_obs + (Zak*(sqrt(p_obs*(1-p_obs)/n)))
  p_exp <- exp/sum(exp)
  sig <- ifelse(p_exp >= low_interval & p_exp <= upper_interval, "ns", "sig") # if expected value is greater than lower bound 
  #or less than upper bound, then NS
  plot(c(0,k+1), c(min(low_interval),max(upper_interval)), type = "n", 
       ylab = "Preference", xlab = "items", las = 1) #number of items plus/minus 1. plot intervals
  arrows(x0 = c(1:k), y0 = low_interval, x1 = c(1:k), y1 = upper_interval, code = 3
         ,angle = 90) #this adds the lines
  points(p_exp, col = "red") # excpected values (same for all metrics)
  out <- data.frame(chi_test_p = rep(chi$p.value, length(res)), 
                    chi_residuals = res, sig = sig)
  out
}

#wrap up for all indexes
preference <- function(obs, exp, alpha = 0.05){
  f <- fr(obs, exp)
  e <- electicity(obs, exp)
  #j <- jacobs(obs, exp)
  c <- chi_pref(obs, exp, alpha = alpha)
  out <- data.frame(exp = exp, obs = obs, fr = f, electicity = e, c)
  out
}

x <- preference(obs = sample(c(0:100),100), exp = sample(c(0:100),100))
pairs(x[,c(-5,-7)]) #more or less same patern, across indexes.

#But the indexes do not behave well in two situations. 
#First, when an item is not used, it is significanly un-preferred regardless of its abundance. 
#I don’t like that, because a very rare plant is expected to get 0 visits from a biological point of view. 
#This is an issue when building bonferroni intervals around 0, 
#which are by definition 0, and any value different from 0 appears as significant.

x <- preference(obs = c(0,25,200), exp = c(0.00003,50,100))

#The second issue is that if you have noise due to sampling (as always), 
#rare items are more likely to be wrongly assessed than common items.

#assume no preferences
x <- preference(obs = c(5,50,100), exp = c(5,50,100))


#now assume some sampling error of +-4 observations
x <- preference(obs = c(1,54,96), exp = c(5,50,100))

