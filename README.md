# bayes-linear-regression

---
title: "Assignment5_Laha"
author: "Laha Ale"
date: "4/14/2018"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#### data space
```{r}
library(MASS)

one <- function(x) rep(1,length(x))
id  <- function(x) x
a0 <- -0.3 # the true values (unknown to model)
a1 <-  0.5

sigma <- 0.2
precision  <- 1/sigma^2  # precision

make.X <- function(n) {
  runif(n,-1,1)
}

make.Y <- function(xs) {
  a0 + a1*xs + rnorm(length(xs),0,sigma)
}
alpha <- 2.0
v_m0 <- c(0. ,0.)
m_S0 <- 1/alpha*diag(2)
mv_size = 5e2 ## lower down this value if you machine is slow
prior <- mvrnorm(n=mv_size,mu = v_m0,Sigma = m_S0)

posterior <- prior
compute_posterior <- function(X, Y, m_old=v_m0, S_old=m_S0, phi= c(one, id),sample=4) {
  Phi <- sapply(phi, function(base) base(X))  # make design matrix
  
  if(length(X)==1)  # type hack, with just 1 point, R makes a vector, not a matrix
    Phi <- t(as.matrix(Phi))                               
  print(Phi)
  S_new <- solve(solve(S_old) + precision * t(Phi) %*% Phi)
  m_new <- S_new %*% (solve(S_old) %*% m_old + precision * t(Phi) %*% Y)
  v_m0 <- m_new
  m_S0 <- S_new
  #list(m=m_new, S=S_new)  # return the new updated parameters
  mvrnorm(n=mv_size,mu = m_new,Sigma = S_new)
}


alpha <- 2.0
m_0 <- c(0,0)         # we know the mean is (0,0), otherwise, center first
S_0 <- alpha*diag(2)  # relatively uninformative prior

set.seed(121) 




plot_lines_contour <- function(posterior,X=c(),Y=c(),sample=4,size=0){
  par(mfrow=c(1,2)) 
  
  
  bivn <- posterior
  bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 100)
  plot(0,type="n", xlim=c(-1,1),ylim=c(-1,1), xlab="w0", ylab="w1")
  # draw a red rectangle over the plot area
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
         "red")
  image(bivn.kde, xlim=c(-1,1),ylim=c(-1,1), add=T,col = rainbow(25))
  contour(bivn.kde,add = T,col=topo.colors(10,alpha = 0.1))
  abline(v=-0.3, h=0.5, lty=2) # mark the real values
  title(paste("train sample size=",size))
  
  plot(posterior[,1],posterior[,2],type="n",xlim=c(-1,1),ylim=c(-1,1),xlab = "x",ylab = "y")
  if(length(X)>0 & length(Y)>0){
    points(X    , Y    , pch=19, col="blue")
  }
  i<-1
  while (i <= sample){
    abline(posterior[i,], col="red")  
    #print(posterior[i,])
    i <- i+1
  }
  title(paste(sample,"samples of the regression"))
  #abline(c(-0.3,.5), col="green") 
  
}

plot_lines_contour(posterior = prior)

X <- make.X(1) # make some points
Y <- make.Y(X)

posterior<- compute_posterior(X, Y)
plot_lines_contour(posterior = posterior,X=X,Y=Y,size = 1)

X <- make.X(5) # make some points
Y <- make.Y(X)

posterior<- compute_posterior(X, Y)
plot_lines_contour(posterior = posterior,X=X,Y=Y,size = 5)

X <- make.X(30) # make some points
Y <- make.Y(X)

posterior<- compute_posterior(X, Y)
plot_lines_contour(posterior = posterior,X=X,Y=Y,size = 30)

```

