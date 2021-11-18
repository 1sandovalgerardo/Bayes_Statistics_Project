####
library(ggplot2) # for graphs
library(rjags)
library(dplyr)
library(cowplot)
library(tidyverse)
#library(fBasics)

#library(forecast)

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

#### JAGS MODEL 1 ####
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

#### JAGS MODEL 2 ####
model_string2 = textConnection("
  model{
  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(alpha + X1[i]*beta1+ X2[i]*beta2+ X3[i]*beta3+ X4[i]*beta4, taue)
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

#### JAGS MODEL 3  Bayesian Lasso ####
X5 = spy_data$change5
X6 = spy_data$AD_Change1
X7 = spy_data$AD_Change3
X8 = spy_data$AD_Change5
X9 = spy_data$change0
data = list(Y=Y, X1=X1, X2=X2, X3=X3, X4=X4, 
            X5=X5, X6=X6, X7=X7, X8=X8, X9=X9,n=n)#, p=p)

model_string3 = textConnection("model{
  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(alpha + X1[i]*beta1+ X2[i]*beta2+ X3[i]*beta3+ X4[i]*beta4, taue)
  }
  # Priors
  beta1 ~ ddexp(0, taub*taue)
  beta2 ~ ddexp(0, taub*taue)
  beta3 ~ ddexp(0, taub*taue)
  beta4 ~ ddexp(0, taub*taue)
  taue ~ dgamma(0.1, 0.1)
  taub ~ dgamma(0.1, 0.1)
  alpha ~ dnorm(0, 0.001)
}")
model3 = jags.model(model_string3, data=data, n.chains=nChains, quiet=T)
update(model3, burn=nBurn, progress.bar='text')
samples3 = coda.samples(model3, variable.names=params, thin=nThin,
                        n.iter=nIter, progress.bar='text')

#### RESULTS ####
plot(samples3)
round(effectiveSize(samples3), 1)
sum3 = summary(samples3)
rownames(sum3$statistics) = names 
rownames(sum3$quantiles) = names
sum3

#### COMPARING MODELS ####

# Break out each chain from the samples object
# and make them a data frame to easily access 
# the predictors.
model1_chain1 = as.data.frame(samples1[[1]])
model1_chain2 = as.data.frame(samples1[[2]])

model2_chain1 = as.data.frame(samples2[[1]])
model2_chain2 = as.data.frame(samples2[[2]])

model3_chain1 = as.data.frame(samples3[[1]])
model3_chain2 = as.data.frame(samples3[[2]])

head(model3_chain1)
dim(model3_chain1)==dim(samples3[[1]])

alphas1 = c(samples1[[1]][,1], samples1[[2]][,1])
beta1.1 = c(samples1[[1]][,2], samples1[[2]][,2])
beta2.1 = c(samples1[[1]][,3], samples1[[2]][,3])
beta3.1 = c(samples1[[1]][,4], samples1[[2]][,4])
beta4.1 = c(samples1[[1]][,5], samples1[[2]][,5])

alphas2 = c(samples2[[1]][,1], samples2[[2]][,1])
beta1.2 = c(samples2[[1]][,2], samples2[[2]][,2])
beta2.2 = c(samples2[[1]][,3], samples2[[2]][,3])
beta3.2 = c(samples2[[1]][,4], samples2[[2]][,4])
beta4.2 = c(samples2[[1]][,5], samples2[[2]][,5])

alphas3 = c(samples3[[1]][,1], samples3[[2]][,1])
beta1.3 = c(samples3[[1]][,2], samples3[[2]][,2])
beta2.3 = c(samples3[[1]][,3], samples3[[2]][,3])
beta3.3 = c(samples3[[1]][,4], samples3[[2]][,4])
beta4.3 = c(samples3[[1]][,5], samples3[[2]][,5])

alphasDF = data.frame(alphas1, alphas2, alphas3)
alphasDF = alphasDF %>% pivot_longer(names_to='Index',
                                     values_to='Value')
head(alphasDF)

## TO DO:  Make master dataframe and then melt it 
## this will allow me to easily overlap the density plots
## and assign appropriate labels

### Summary of marginal distributions of alpha and betas ###
# Similar to week 9, example 1
# alpha
ggplot(model1_chain1, aes(x=alpha)) +
  geom_density() +
  geom_density(data=model2_chain1, aes(x=alpha), col='blue') +
  geom_density(data=model3_chain1, aes(x=alpha), col='red')

# Beta 1
ggplot(model1_chain1, aes(x=beta1)) +
  geom_density() +
  geom_density(data=model2_chain1, aes(x=beta1), col='blue') +
  geom_density(data=model3_chain1, aes(x=beta1), col='red')
# Beta 2
ggplot(model1_chain1, aes(x=beta2)) +
  geom_density() +
  geom_density(data=model2_chain1, aes(x=beta2), col='blue') +
  geom_density(data=model3_chain1, aes(x=beta2), col='red')
# Beta 3
ggplot(model1_chain1, aes(x=beta3)) +
  geom_density() +
  geom_density(data=model2_chain1, aes(x=beta3), col='blue') +
  geom_density(data=model3_chain1, aes(x=beta3), col='red')
# Beta 4
ggplot(model1_chain1, aes(x=beta4)) +
  geom_density() +
  geom_density(data=model2_chain1, aes(x=beta4), col='blue') +
  geom_density(data=model3_chain1, aes(x=beta4), col='red')


#### START OF REQUESTED CODE ####
for(j in 1:p){
  s1 = c(samples1[[1]][,j], samples1[[2]][,j])
  s2 = c(samples2[[1]][,j], samples1[[2]][,j])
  s3 = c(samples3[[1]][,j], samples1[[2]][,j])
}


plot_list = list()

d1 = density(s1)
d2 = density(s2)
d3 = density(s3)

Prior <- c(rep("Uninformative Gaussian",length(d1$x)),
           rep("Gaussian shrinkage",length(d2$x)),
           rep("Bayesian LASSO",length(d3$x)))
x <- c(d1$x,d2$x,d3$x)
y <- c(d1$y,d2$y,d3$y)
d.data <- data.frame(x=x,y=y,Prior=Prior)

max.y <- max(y)
plot.title <- names[j]
g <- ggplot(d.data,aes(x=x,y=y,color=Prior))+geom_line()+
  labs(x=expression(beta),y="Posterior density")+ggtitle(plot.title)+
  ylim(c(0,max.y))+geom_vline(xintercept=0)
plot_list[[j]] <- g+theme(legend.position="none")

prow <- plot_grid(plotlist=plot_list,nrow=4)
legend <- get_legend(g+guides(color=guide_legend(reverse=TRUE,nrow=1))+
                     theme(legend.position="bottom"))
plot_grid(prow,legend,nrow=5,rel_heights = c(1,0.1))
### You may have to add "g" to get the plots to show
### in the markdown book.
g

#### Auto Correlation Plots ####
autocorr.plot(samples1)

effectiveSize(samples1)
effectiveSize(samples2)
effectiveSize(samples3)

geweke.diag(samples1)
geweke.diag(samples2)
geweke.diag(samples3)
#### Computing the posterior probabilities of each model ####


#

#



















##