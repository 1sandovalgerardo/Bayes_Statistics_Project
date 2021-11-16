####
library(ggplot2) # for graphs
library(rjags)
library(dplyr)
#library(fBasics)

#library(forecast)

## Test ssh use ##

#### Load Data ####
spy_path = paste(getwd(), '/SPY.csv', sep='')

spy_data = read.csv(spy_path)
head(spy_data)
View(spy_data)

#### Create Change Variables ####
spy_data = spy_data %>%
  dplyr::mutate(change1 = (Adj.Close - lag(Adj.Close, n=1))/ lag(Adj.Close, n=1),
                change3 = (Adj.Close - lag(Adj.Close, n=3))/ lag(Adj.Close, n=3),
                change5 = (Adj.Close - lag(Adj.Close, n=5))/ lag(Adj.Close, n=5),
                change10 = (Adj.Close - lag(Adj.Close, n=10))/ lag(Adj.Close, n=10),
                change0 = (Close - Open) / Open
                )
hist(spy_data$change3, breaks=50)

# I was thinking of analyzing the absalute values to make the logistic regression 
# work a bit easier
abs_data = spy_data %>%
  dplyr::mutate(change1 = abs((Adj.Close - lag(Adj.Close, n=1))/ lag(Adj.Close, n=1)),
                change3 = abs((Adj.Close - lag(Adj.Close, n=3))/ lag(Adj.Close, n=3)),
                change5 = abs((Adj.Close - lag(Adj.Close, n=5))/ lag(Adj.Close, n=5)),
                change10 = abs((Adj.Close - lag(Adj.Close, n=10))/ lag(Adj.Close, n=10))
                )


hist(abs_data$change3, breaks=50)

mean(na.omit(abs_data$change3))

#### Volume Variables ####
# Function to normalize volume
norm_volume = function(data, new_max, new_min){
  return(data-min(data)) / (max(data)-min(data)) * (new_max-new_min) + new_min
}

### Normalized Volume ###
spy_data$Norm_Volume = norm_volume(spy_data$Volume, 3, 1)

### Advance Decline Variables ####
# mutate is from dplyr
spy_data = spy_data %>%
  mutate(Volume_Dir = (change0/abs(change0))*Norm_Volume,
         Adv_Dec = cumsum(ifelse(is.na(Volume_Dir), 0, Volume_Dir)),
         AD_Change1 = (Adv_Dec - lag(Adv_Dec)) / lag(Adv_Dec),
         AD_Change3 = (Adv_Dec - lag(Adv_Dec,n=3)) / lag(Adv_Dec,n=3),
         AD_Change5 = (Adv_Dec - lag(Adv_Dec,n=5)) / lag(Adv_Dec,n=5)
         )
plot(spy_data$Adv_Dec, type='l', xlim=c(1000,5000))
plot(spy_data$AD_Change1, type='l', xlim=c(1000,5000), ylim=c(-1,1))

sd(spy_data$change1)
sd(spy_data$change3)
#### Create Response Variable ####
# We want to predict what the change will be from today over the 
# next 5 days.  So I shifted the change5 column to create a Response 
# column
spy_data$Response = lead(spy_data$change5, n=5)

check_values = subset(spy_data, select = c(Response, change5))
head(check_values, n=10)

#### STRUCTURE FOR JAGS ####
spy_data = na.omit(spy_data)
Y = spy_data$Response
X = subset(spy_data, select=c(Norm_Volume, change1, 
                                change3, Adv_Dec))
X1 = spy_data$Norm_Volume
X2 = spy_data$change1
X3 = spy_data$change3
X4 = spy_data$Adv_Dec

n = length(Y)
p = ncol(X)
data = list(Y=Y, X1=X1, X2=X2, X3=X3, X4=X4, n=n)#, p=p)
params = c('alpha', 'beta1', 'beta2', 'beta3', 'beta4')
names = c('alpha',"Norm_Volume", "change1", 'change3', 'Adv_Dec')

nBurn = 10000
nChains = 2
nSave = 4000
nThin = 5
nIter = ceiling((nSave*nThin)/nChains)

#### JAGS MODEL ####
model_string1 = textConnection("
  model{
  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(alpha + X1[i]*beta1+ X2[i]*beta2+ X3[i]*beta3+ X4[i]*beta4, tau1)
  }
  # Priors
  beta1 ~ dnorm(0, 0.001)
  beta2 ~ dnorm(0, 1/0.012)
  beta3 ~ dnorm(0, 1/0.019)
  beta4 ~ dnorm(0, 0.001)
  tau1 ~ dgamma(0.1, 0.1) 
  alpha ~ dnorm(0, 0.001)
  }")

model1 = jags.model(model_string1, data=data, n.chains=nChains, quiet=T)
update(model1, burn=nBurn, progress.bar='text')
samples1 = coda.samples(model1, variable.names=params, thin=nThin,
                        n.iter=nIter, progress.bar='text')
#### RESULTS ####
plot(samples1)
round(effectiveSize(samples1), 1)
sum1 = summary(samples1)
rownames(sum1$statistics) = names
rownames(sum1$quantiles) = names
sum1

####
### JAGS 2 ###
model_string2 = textConnection("
  model{
  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(alpha + X1[i]*beta1+ X2[i]*beta2+ X3[i]*beta3+ X4[i]*beta4, tau1)
  }
  # Priors
  beta1 ~ dnorm(0, 0.001)
  beta2 ~ dnorm(0, 0.001)
  beta3 ~ dnorm(0, 0.001)
  beta4 ~ dnorm(0, 0.001)
  tau1 ~ dgamma(0.1, 0.1) 
  alpha ~ dnorm(0, 0.001)
  }")

model2 = jags.model(model_string2, data=data, n.chains=nChains, quiet=T)
update(model2, burn=nBurn, progress.bar='text')
samples2 = coda.samples(model2, variable.names=params, thin=nThin,
                        n.iter=nIter, progress.bar='text')
#### RESULTS ####
plot(samples2)
round(effectiveSize(samples2), 1)
sum2 = summary(samples2)
rownames(sum1$statistics) = names
rownames(sum1$quantiles) = names
sum2

#### Part of Model ####
for(j in 1:n){
tau[1] ~ dgamma(0.1,0.1)
}

for(j in 2:n){
rho ~ runif(-1, 1)
}


#### Volume ####
volume = spy_data$Volume
intraday_change = (spy_data$Close - spy_data$Open) / spy_data$Open

plot(cumsum(volume*(intraday_change/abs(intraday_change))))

#



















##