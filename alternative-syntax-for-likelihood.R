library(rjags)

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

spy_data$Response = lead(spy_data$change5, n=5)

check_values = subset(spy_data, select = c(Response, change5))
head(check_values, n=10)

#### DATA STRUCTURE FOR JAGS ####
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

 #### JAGS ####
model_string2 = textConnection("
  model{
  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(alpha + X1[i]*beta1+ X2[i]*beta2+ X3[i]*beta3+ X4[i]*beta4, tau3)
  }
  # Priors
  beta1 ~ dnorm(0, taub*taue)
  beta2 ~ dnorm(0, taub*taue)
  beta3 ~ dnorm(0, taub*taue)
  beta4 ~ dnorm(0, taub*taue)
  
  taue ~ dgamma(0.1, 0.1) 
  taub ~ dgamma(0.1, 0.1)
  
  alpha ~ dnorm(0, 0.001)
  }")

model_string2_alt = textConnection("
  model{
  # Likelihood
  for(i in 1:n){
    for (j in 1:p){
      Y[i] ~ dnorm(alpha + inprod(X[i,], beta[]), taue
  }}
  # Priors
  beta1 ~ dnorm(0, taub*taue)
  beta2 ~ dnorm(0, taub*taue)
  beta3 ~ dnorm(0, taub*taue)
  beta4 ~ dnorm(0, taub*taue)
  
  taue ~ dgamma(0.1, 0.1) 
  taub ~ dgamma(0.1, 0.1)
  
  alpha ~ dnorm(0, 0.001)
  }")

model2 = jags.model(model_string2, data=data, n.chains=nChains, quiet=T)
update(model2, burn=nBurn, progress.bar='text')
samples2 = coda.samples(model2, variable.names=params, thin=nThin,
                        n.iter=nIter, progress.bar='text')