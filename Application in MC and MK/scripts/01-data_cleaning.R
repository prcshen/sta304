#### Preamble ####
# Author: Wenxuan Li
# Data: 26 April 2021
# Contact: wenxuan.li@mail.utoronto.ca???
# License: MIT
# Pre-requisites: 


library(kableExtra)
library(ggplot2)
library(tidyverse)

# PD for p_i: u,v randomly chosen from 1,...,k
# p_u^{t} \sim Unif[max(0,p_u^{t-1}-\epsilon_p), 
# min(p_u^{t-1}+\epsilon_p, p_u^{t-1}+p_v^{t-1})]
# then p_v^{t}=p_u^{t-1}+p_v{t-1}-p_u{t}
qq.p = function(x,u,v,eps.p){
  1/(min(x[u]+eps.p, x[u]+x[v]) - max(0, x[u]-eps.p))
}

# PD for f: Unif(max(-p_{min}^t)/(1-p_{min}^t,f-\epsilon_f))
qq.f = function(x,y,k,eps.f){
  1/(min(x[1]+eps.f, 1) - max(-min(y[2:(k+1)])/(1-min(y[2:(k+1)])), x[1]-eps.f))
}     

mhkis2 = function(eps = 0.05){
  # log of the joint distribution function for k = 2
  log.g = function(X,n11,n12,n22){
    n11*(log(X[2])+log(X[1]+(1-X[1])*X[2])) + 
      n12*log(2*X[2]*X[3]*(1-X[1]))+
      n22*(log(X[3])+log(X[1]+(1-X[1])*X[3]))
  }
  ### data simulation
  eps.p = eps
  k  = 2    # alleles 
  n  = 200  # sample size
  f  = 0.05 # true inbreeding coef
  # true allele f
  p1 = 0.25 
  p2 = 0.75
  
  p11 = p1*(f+(1-f)*p1)
  p22 = p2*(f+(1-f)*p2)
  p12 = 2*p1*p2*(1-f)
  n12 = round(p12*n)
  n11 = round(p11*n)
  n22 = n-n11-n12
  
  #### the algo
  # init values
  X    = rep(0,3)
  X[1] = runif(1)
  a = runif(1)
  b = runif(1)
  
  p1 = a/(a+b)
  p2 = b/(a+b)
  X[2] = p1
  X[3] = p2 # overdispersed value
  
  M  =10000
  B = 1000 # burn value
  
  eps.f =((k^2)*eps.p/((k-1)*(k-1-k*eps.p)))+0.0001
  
  thresholdaccept = 0
  flist = rep(0,M)
  
  for (i in 1:M) {
    Y = X
    
    r = sample(c(2,3),2) # propose p1 or p2
    u = r[1]
    v = r[2]
    
    Y[u] = runif(1,max(0,Y[u] - eps.p),min(Y[u]+eps.p,Y[u]+Y[v]))
    Y[v] = 1-Y[u] # p1+p2 = 1 when k=2
    
    U  = runif(1) # for accept/reject
    alpha = log.g(Y,n11,n12,n22) + log(qq.p(X,u,v,eps.p)) -
      log.g(X,n11,n12,n22) - log(qq.p(Y,u,v,eps.p))
    
    if(log(U) < alpha){
      X = Y
      # now we update the f when p' is accepted
      Z = X
      
      Z[1] = runif(1,max(-min(Y[2:3])/(1-min(Y[2:3])), X[1]-eps.f),min(X[1]+eps.f, 1))
      
      W = runif(1) # for accept/reject
      
      beta = log.g(Z,n11,n12,n22) + log(qq.f(X,Z,k,eps.f)) - 
        log.g(X,n11,n12,n22) - log(qq.f(Z,X,k,eps.f))
      
      if(log(W)<beta){
        X = Z
        thresholdaccept = thresholdaccept + 1
      }
    }
    flist[i] = X[1]
  }
  estmean = mean(flist[(B+1):M])
  se1 =  sd(flist[(B+1):M]) / sqrt(M-B)
  varfact = function(xxx) { 2 * sum(acf(xxx, plot=FALSE)$acf) - 1 }
  se2 = se1 * sqrt( varfact(flist[(B+1):M]) )
  ci = c(estmean - 1.96*se2, estmean + 1.96*se2)
  return(list(thresholdaccept/M, estmean, ci, flist, M, B, se2))
}

# testing epsilon for the best acceptance rate
set.seed(9999)
epslist  = seq(0.01,0.1,0.01)
acclist = meanlist = cilblist = ciublist= selist = rep(0,10) 
for (i in 1:10){
  result = mhkis2(epslist[i])
  acclist[i]  = result[[1]]
  meanlist[i] = result[[2]]
  cilblist[i] = result[[3]][1]
  ciublist[i] = result[[3]][2]
  selist[i]   = result[[7]]
}
results = cbind(epslist, acclist,meanlist,cilblist,ciublist, selist)
results = as.data.frame(results)
colnames(results) = c("Epsilon", "Acceptance Rate", "Mean", "Lower bound of CI", 
                      "Upper bound of CI", "Standard Error")
results1=results%>%
  select(1,2,6)
results1%>%
  knitr::kable(caption = "Acceptance rate by different epsilon p when k = 2")%>%
  kable_styling(latex_options="HOLD_position")

# when epsilon = 0.02 or 0.03 the algo seems to perform better
set.seed(9999)
result = mhkis2(0.03)
flist = result[[4]]
M = result[[5]]
B = result[[6]]
plot(flist[1:M], type = "l")
abline(h=0.05, col="red")

# mean
estmean = mean(flist[(B+1):M])
# 95$ CI
se2 = result[[7]]
ci = c(estmean - 1.96*se2, estmean + 1.96*se2)

mhkis6 = function(eps = 0.05){
  
  log.g = function(X,N){
    dens = 0
    k = length(X) - 1
    for (i in 1:k) {
      if (i < k){
        for (j in (i+1):k){
          dens = dens + N[i,j]*log(2*X[i+1]*X[j+1]*(1-X[1]))
        }
      }
      dens = dens + N[i,i]*log(X[i+1]*(X[1]+(1-X[1])*X[i+1]))
    }
    return(dens)
  }
  # data
  eps.p = eps
  n = 1000 # sample sizes
  k = 6    # alleles
  f = 0.05 # true inbreeding coefficient
  # allele frequencies k = 6
  k6 = c(0.02,0.06,0.075,0.085,0.21,0.55)
  P = matrix(nrow = 6, ncol = 6) 
  for (i in 1:6){
    for (j in 1:6){
      if (i==j){
        P[i,j] = k6[i]*(f+(1-f)*k6[i])
      }
      else {
        P[i,j] = 2*k6[i]*k6[j]*(1-f)
      }
    }
  }
  
  N = round(P*n)
  
  N[6,6] = 316
  
  X      = rep(0,7)
  X[1]   = runif(1)
  ps     = runif(6)
  X[2:7] = ps/sum(ps) 
  
  M = 10000
  B = 1000 
  
  eps.f =((k^2)*eps.p/((k-1)*(k-1-k*eps.p)))+0.0001
  
  thresholdaccept = 0
  flist = rep(0,M)
  
  for (m in 1:M) {
    Y = X
    
    r = sample(c(2,3,4,5,6,7),2)
    u = r[1]
    v = r[2]
    
    Y[u] = runif(1,max(0,Y[u] - eps.p),min(Y[u]+eps.p,Y[u]+Y[v]))
    Y[v] = X[u] + X[v] - Y[u]
    
    U  = runif(1) 
    alpha = log.g(Y,N) + log(qq.p(X,u,v,eps.p)) -
      log.g(X,N) - log(qq.p(Y,u,v,eps.p))
    
    if(log(U) < alpha){
      X = Y
      
      Z = X
      
      Z[1] = runif(1,max(-min(Y[2:7])/(1-min(Y[2:7])), X[1]-eps.f),min(X[1]+eps.f, 1))
      
      W = runif(1) 
      
      beta = log.g(Z,N) + log(qq.f(X,Z,k,eps.f)) - 
        log.g(X,N) - log(qq.f(Z,X,k,eps.f))
      
      if(log(W)<beta){
        X = Z
        thresholdaccept = thresholdaccept + 1
      }
    }
    flist[m] = X[1]
  }
  estmean = mean(flist[(B+1):M])
  se1 =  sd(flist[(B+1):M]) / sqrt(M-B)
  varfact = function(xxx) { 2 * sum(acf(xxx, plot=FALSE)$acf) - 1 }
  se2 = se1 * sqrt( varfact(flist[(B+1):M]) )
  ci = c(estmean - 1.96*se2, estmean + 1.96*se2)
  return(list(thresholdaccept/M, estmean, ci, flist, M, B, se2))
}

# testing epsilon for best acceptance rate
set.seed(9999)
epslist = c(seq(0.001,0.01,0.001), seq(0.011,0.02,0.001))
acclist = meanlist = cilblist = ciublist = selist = rep(0,20)
for (i in 1:20){
  tryCatch({
    result = mhkis6(epslist[i])
    acclist[i]  = result[[1]]
    meanlist[i] = result[[2]]
    cilblist[i] = result[[3]][1]
    ciublist[i] = result[[3]][2]
    selist[i]   = result[[7]]
  }, error=function(e){})
}
results = cbind(epslist, acclist,meanlist,cilblist,ciublist,selist)
results = as.data.frame(results)
colnames(results) = c("Epsilon", "Acceptance Rate", "Mean", "Lower bound of CI", 
                      "Upper bound of CI", "Standard Error")
results2=results%>%
  select(1,2,6)
results2%>%
  knitr::kable(caption = "Acceptance rate by different epsilon p when k = 6")%>%
  kable_styling(latex_options="HOLD_position")
  
  
set.seed(9999)
result = mhkis6(0.01)
flist = result[[4]]
M = result[[5]]
plot(flist[1:M], type = "l")
abline(h=0.05,col="red")
# mean
estmean = mean(flist[(B+1):M])
# 95$ CI
se2 = result[[7]]
ci = c(estmean - 1.96*se2, estmean + 1.96*se2)


n = 1000 # sample sizes
k = 6    # alleles
f = 0.05 # true inbreeding coefficient
k6 = c(0.02,0.06,0.075,0.085,0.21,0.55)
P = matrix(nrow = 6, ncol = 6) 
for (i in 1:6){
  for (j in 1:6){
    if (i==j){
      P[i,j] = k6[i]*(f+(1-f)*k6[i])
    }
    else {
      P[i,j] = 2*k6[i]*k6[j]*(1-f)
    }
  }
}
N = round(P*n)
N[6,6] = 316

#### the algo
# init values
set.seed(9999)
X      = rep(0,7)
X[1]   = runif(1)
ps     = runif(6)
X[2:7] = ps/sum(ps)

M = 10000
B = 1000 # burn value
flist=p1list=p2list=p3list=p4list=p5list=p6list=rep(0,M)

set.seed(9999)
# systematic-scan
for (m in 1:M) {
  Y = X
  
  pdfp1 = function(x){
    if(x < 0 || x > 1)
      return(0)
    else
      return(x*(X[1]+(1-X[1])*x)^N[1,1]*(x*(1-X[1]))^N[1,2])
  }
  c1 = integrate(pdfp1,0,1)[[1]]
  if (c1>0){
    cdfp1 = function(x,u){
      return(integrate(pdfp1,0,x)[[1]]/c1 - u);
    }
    Y[2] = uniroot(cdfp1, c(0,1), tol = 0.0001, u = runif(1))$root
  } else Y[2] = 1e-20
  
  
  pdfp2 = function(x){
    if(x < 0 || x > 1)
      return(0)
    else
      return(x*(X[1]+(1-X[1])*x)^N[2,2]*(x*(1-X[1]))^(N[1,2]+N[2,3]))
  }
  c2 = integrate(pdfp2,0,1)[[1]]
  if (c2>0){
    cdfp2 = function(x,u){
      return(integrate(pdfp2,0,x)[[1]]/c2 - u);
    }
    Y[3] = uniroot(cdfp2, c(0,1), tol = 0.0001, u = runif(1))$root
  } else Y[3] = 1e-20
  
  
  pdfp3 = function(x){
    if(x < 0 || x > 1)
      return(0)
    else
      return(x*(X[1]+(1-X[1])*x)^N[3,3]*(x*(1-X[1]))^(N[3,4]+N[2,3]))
  }
  c3 = integrate(pdfp3,0,1)[[1]]
  if (c3>0){
    cdfp3 = function(x,u){
      return(integrate(pdfp3,0,x)[[1]]/c3 - u);
    }
    Y[4] = uniroot(cdfp3, c(0,1), tol = 0.0001, u = runif(1))$root
  } else Y[4] = 1e-20
  
  
  pdfp4 = function(x){
    if(x < 0 || x > 1)
      return(0)
    else
      return(x*(X[1]+(1-X[1])*x)^N[4,4]*(x*(1-X[1]))^(N[3,4]+N[4,5]))
  }
  c4 = integrate(pdfp4,0,1)[[1]]
  if (c4>0){
    cdfp4 = function(x,u){
      return(integrate(pdfp4,0,x)[[1]]/c4 - u);
    }
    Y[5] = uniroot(cdfp4, c(0,1), tol = 0.0001, u = runif(1))$root
  } else Y[5] = 1e-20
  
  
  pdfp5 = function(x){
    if(x < 0 || x > 1)
      return(0)
    else
      return(x*(X[1]+(1-X[1])*x)^N[5,5]*(x*(1-X[1]))^(N[5,6]+N[4,5]))
  }
  c5 = integrate(pdfp5,0,1)[[1]]
  if (c5>0){
    cdfp5 = function(x,u){
      return(integrate(pdfp5,0,x)[[1]]/c5 - u);
    }
    Y[6] = uniroot(cdfp5, c(0,1), tol = 0.0001, u = runif(1))$root
  } else Y[6] = 1e-20
  
  
  pdfp6 = function(x){
    if(x < 0 || x > 1)
      return(0)
    else
      return(x*(X[1]+(1-X[1])*x)^N[6,6]*(x*(1-X[1]))^N[5,6])
  }
  c6 = integrate(pdfp6,0,1)[[1]]
  if (c6>0){
    cdfp6 = function(x,u){
      return(integrate(pdfp6,0,x)[[1]]/c6 - u);
    }
    Y[7] = uniroot(cdfp6, c(0,1), tol = 0.0001, u = runif(1))$root
  } else Y[7] = 1e-20
  
  
  psum = sum(Y[2:7])
  Y[2:7] = Y[2:7]/psum
  X = Y
  
  
  #update f 
  Z = X
  
  pdff = function(x){
    dens = 1
    if(x < -min(X[2:7])/(1-min(X[2:7])) || x > 1)
      return(0)
    else
      for (i in 1:6){
        if (i < k){
          for (j in (i+1):6) {
            dens = dens*(x+(1-x)*X[i+1])^N[i,i]*(1-x)^N[i,j]
          }
        }
      }
    dens
  }
  cf = integrate(pdff,-min(X[2:7])/(1-min(X[2:7])),1)[[1]]
  cdff = function(x,u){
    return(integrate(pdff,-min(X[2:7])/(1-min(X[2:7])),x)[[1]]/cf - u);
  }
  Z[1] = uniroot(cdff, c(-min(X[2:7])/(1-min(X[2:7])),1), tol = 0.0001, u = runif(1))$root
  X = Z
  
  p1list[m] = X[2]; p2list[m] = X[3]
  p3list[m] = X[4]; p4list[m] = X[5]
  p5list[m] = X[6]; p6list[m] = X[7]
  flist[m]  = X[1]
}
(estmean = mean(flist[(B+1):M]))

qq.f = function(x,y,k,eps.f){
  1/(min(x[1]+eps.f, 1) - max(-min(y[2:(k+1)])/(1-min(y[2:(k+1)])), x[1]-eps.f))
}

eps.p = eps = 0.05
log.g = function(X,N){
  dens = 0
  k = length(X) - 1
  for (i in 1:k) {
    if (i < k){
      for (j in (i+1):k){
        dens = dens + N[i,j]*log(2*X[i+1]*X[j+1]*(1-X[1]))
      }
    }
    dens = dens + N[i,i]*log(X[i+1]*(X[1]+(1-X[1])*X[i+1]))
  }
  return(dens)
}

# data simulation
n = 1000 # sample sizes
k = 6    # alleles
f = 0.05 # true inbreeding coefficient

k6 = c(0.02,0.06,0.075,0.085,0.21,0.55)

P = matrix(nrow = 6, ncol = 6) 
for (i in 1:6){
  for (j in 1:6){
    if (i==j){
      P[i,j] = k6[i]*(f+(1-f)*k6[i])
    }
    else {
      P[i,j] = 2*k6[i]*k6[j]*(1-f)
    }
  }
}

N = round(P*n)

N[6,6] = 316

#algo
# init values
X      = rep(0,7)
X[1]   = runif(1)
ps     = runif(6)
X[2:7] = ps/sum(ps) 

M = 10000
B = 1000 
eps.f =((k^2)*eps.p/((k-1)*(k-1-k*eps.p)))+0.0001

thresholdaccept = 0
flist = rep(0,M)

for (i in 1:M) {
  Y = X
  X[1] = runif(1,max(-min(Y[2:7])/(1-min(Y[2:7])), X[1]-eps.f),min(X[1]+eps.f, 1))
  
  Z = rep(0,7)
  Z[1] = runif(1)
  pz = runif(6)
  Z[2:7] = pz/sum(pz)
  W = Z
  Z[1] = runif(1,max(-min(W[2:7])/(1-min(W[2:7])), Z[1]-eps.f),min(Z[1]+eps.f, 1))
  
  U  = runif(1) # for accept/reject
  alpha = log.g(Z,N) + log(qq.f(X,Y,k,eps.f)) -
    log.g(X,N) - log(qq.f(Z,W,k,eps.p))
  
  if(log(U) < alpha){
    X = Z
    thresholdaccept = thresholdaccept + 1
  }
  
  flist[i] = X[1]
}
estmean = mean(flist[(B+1):M])
se1 =  sd(flist[(B+1):M]) / sqrt(M-B)
varfact = function(xxx) { 2 * sum(acf(xxx, plot=FALSE)$acf) - 1 }
se2 = se1 * sqrt( varfact(flist[(B+1):M]) )
ci = c(estmean - 1.96*se2, estmean + 1.96*se2)

plot(flist, type = 'l')
abline(h=0.05, col="red")

# importance sampling

n = 200
k = 2
h = function(f){
  return(f)
}

g = function(X){
  X[4]*(log(X[2])+log(X[1]+(1-X[1])*X[2])) + 
    X[5]*log(2*X[2]*X[3]*(1-X[1]))+X[6]*(log(X[3])+log(X[1]+(1-X[1])*X[3]))
}

f_a = function(X){
  return(runif(1,max(-min(X[2:3])/(1-min(X[2:3])), X[1]-eps.f),min(X[1]+eps.f, 1)))
  
}
eps.p = 0.02
eps.f =((k^2)*eps.p/((k-1)*(k-1-k*eps.p)))+0.0001
f = runif(10000)
a = runif(10000)
b = runif(10000)

p1 = a/(a+b)
p2 = b/(a+b)
p11 = p1*(f+(1-f)*p1)
p22 = p2*(f+(1-f)*p2)
p12 = 2*p1*p2*(1-f) 
n12 = round(p12*n)
n11 = round(p11*n)
n22 = round(p22*n)
X  = c(f,p1,p2,n11,n12,n22)

numlist = g(X)+log(f) - log(f_a(X))
denomlist = g(X) - log(f_a(X))

estimate_f_list = exp(numlist)/exp(denomlist)
plot(estimate_f_list[seq(1,10000,10)],type = 'l')
abline(h=0.05, col="red")
estimate_f = mean(exp(numlist))/mean(exp(denomlist))

#Maximum likelihood estimation procdure:

# first lets define the simplest form when k = 2, 
# compare accuracy by setting f = 0.05
# MLE numerical value 
# and the MCMC methods value 
# after all, determine if Monte carlo methods was good..

# We need to know that k[1] is denote as p1, 
# which is the frequency of allele A1
k2 = c(0.25,0.75) # sum is 1
f = 0.05
n = 200
#pij = 2*pi*pj*(1-f)
#pii = pi*(f+(1-f)*pi)
p1 = k2[1]
p2 = k2[2]
p11 = p1*(f+(1-f)*p1)
p22 = p2*(f+(1-f)*p2)
p12 = 2*p1*p2*(1-f)

# take notice that nij is the number of individuals having the genotype AiAj. They must be integers, which is why we need the method for removing the residue and increasing by one and then take the average to get the estimate. we see that if they are not rounded, we will get the precise 0.05 we need, but this is only an example.

n12 = round(p12*n)+1
n11 = round(p11*n)+1
n22 = round(p22*n)+1


festimate = 1-(2*n12*n)/((2*n11+n12)*(n12+2*n22))

festimate1 = festimate

n12 = round(p12*n)
n11 = round(p11*n)
n22 = round(p22*n)

festimate = 1-(2*n12*n)/((2*n11+n12)*(n12+2*n22))

festimate2 = festimate

festimate_final = (festimate1+festimate2)/2
festimate_final
# The reason we retest the approach used in the study is because, even if we apply the same formula, the way we deal with the remainder will have a significant effect on the estimate, which is why, even when we use the MCMC methods, there may still be some standard error.