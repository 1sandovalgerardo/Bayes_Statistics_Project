
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
      Y[i] ~ dnorm(alpha + inprod(X[i], beta[]), taue
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
