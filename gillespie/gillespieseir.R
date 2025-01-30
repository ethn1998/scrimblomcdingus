
#SEIR epidemic model
### Constants


dt <- 0.001 #Step size for determininstic numerical methods
popsize <- 1000 #Population size
t0 <- 0.0
tmax <- 60.0

lambda <- 3.0/popsize #Infection rate, traditionally normalized by popsize
sigma <- 0.1 #Incubation rate, higher number implies shorter incubation period
gamma <- 0.05 #Recovery rate

e0 <- 5.0*popsize/100 #Starting percentage of exposed
s0 <- popsize - e0
i0 <- 0
r0 <- 0

### Gillespie Algorithm
set.seed(20240715)

t <- t0
s <- s0
e <- e0
i <- i0
r <- r0

tvec <- c(t0)
svec <- c(s0)
evec <- c(e0)
ivec <- c(i0)
rvec <- c(r0)

while(t<tmax){
  p <- runif(1,0,1)
  R_lambda <- lambda*s*i #Infection rate
  R_sigma <- sigma*e #Incubation rate
  R_gamma <- gamma*i #Removal rate
  
  RTOT <- R_lambda + R_sigma + R_gamma #Total rate
  #print(sprintf("t:%.2f R_lambda:%.2f RTOT:%.2f",t,R_lambda,RTOT))

  #Reaction
  if (RTOT == 0.0){
    break
  } else if(p < R_lambda/RTOT){ #Infection
    s <- s-1
    e <- e+1
  } else if(p < (R_lambda + R_sigma)/RTOT){ #Incubation
    e <- e-1
    i <- i+1
  } else{ #Removal
    i <- i-1
    r <- r+1
  }
  
  #Update
  t <- t + rexp(1,RTOT)
  tvec <- append(tvec,t)
  svec <- append(svec,s)
  evec <- append(evec,e)
  ivec <- append(ivec,i)
  rvec <- append(rvec,r)
}

plot(tvec,svec,xlim=c(0.0,tmax),ylim=c(0,popsize),type="l",xlab="Time",ylab="Number of individuals",
     main="SEIR epidemic model",col="#d55e00")
lines(tvec,evec,col="#cc79a7")
lines(tvec,ivec,col="#009e73")
lines(tvec,rvec,col="#56b4e9")

### Tau leaping?



### Euler method

t <- 0.0
s <- s0
i <- i0
r <- r0

tvec <- c(0.0)
svec <- c(s0)
evec <- c(e0)
ivec <- c(i0)
rvec <- c(r0)

while(t<tmax){
  #Update
  t <- t + dt
  s <- s - dt*lambda*s*i
  e <- e + dt*lambda*s*i - dt*sigma*e
  i <- i + dt*sigma*e - dt*gamma*i
  r <- r + dt*gamma*i
  
  print(sprintf("t:%.3f s:%.1f e:%.1f i:%.1f r:%.1f",t,s,e,i,r))
  
  #Record
  tvec <- append(tvec,t)
  svec <- append(svec,s)
  evec <- append(evec,e)
  ivec <- append(ivec,i)
  rvec <- append(rvec,r)
}

lines(tvec,svec,xlim=c(0.0,tmax),ylim=c(0,popsize),type="l",xlab="Time",ylab="Number of individuals",
      col="#d55e00",lty=2)
lines(tvec,evec,col="#cc79a7",lty=2)
lines(tvec,ivec,col="#009e73",lty=2)
lines(tvec,rvec,col="#56b4e9",lty=2)
#lines(tvec,svec+ivec+rvec,col="black") #sanity test
legend("right",legend=c("S","E","I","R"),col=c("#d55e00","#cc79a7","#009e73","#56b4e9"),lty=1)

#Remark: numerical errors introduced breaks conservation of constant popsize. 

### Runge-Kutta method



