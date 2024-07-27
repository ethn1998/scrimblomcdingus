
#SIR epidemic model
### Constants


dt <- 0.02 #Step size for determininstic numerical methods
popsize <- 1000 #Population size
t0 <- 0.0
tmax <- 5.0

beta <- 6.0/popsize #Infection rate, probability of 
gamma <- 2.0 #Recovery rate

i0 <- 1.0*popsize/100 #Starting percentage of infected
s0 <- popsize - i0
r0 <- 0

### Gillespie Algorithm
set.seed(20101019)

t <- t0
s <- s0
i <- i0
r <- r0

tvec <- c(t0)
svec <- c(s0)
ivec <- c(i0)
rvec <- c(r0)

while(t<tmax){
  p <- runif(1,0,1)
  R_beta <- beta*s*i #Infection rate
  R_gamma <- gamma*i #Removal rate
  RTOT <- R_beta + R_gamma #Total rate
  #print(sprintf("t:%.1e, pi:%.2e",t,R_beta/RTOT)) #Diagnostics
  #Reaction
  if(p < R_beta/RTOT){
    s <- s-1
    i <- i+1
  } else{
    i <- i-1
    r <- r+1
  }
  #Update
  t <- t + rexp(1,RTOT)
  tvec <- append(tvec,t)
  svec <- append(svec,s)
  ivec <- append(ivec,i)
  rvec <- append(rvec,r)
}

plot(tvec,svec,xlim=c(0.0,tmax),ylim=c(0,popsize),type="l",xlab="Time",ylab="Number of individuals",
     main="SIR epidemic model",col="#d55e00")
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
ivec <- c(i0)
rvec <- c(r0)

while(t<tmax){
  #Update
  t <- t + dt
  s <- s - dt*beta*s*i
  i <- i + dt*beta*s*i - dt*gamma*i
  r <- r + dt*gamma*i
  
  #Record
  tvec <- append(tvec,t)
  svec <- append(svec,s)
  ivec <- append(ivec,i)
  rvec <- append(rvec,r)
}

lines(tvec,svec,xlim=c(0.0,tmax),ylim=c(0,popsize),type="l",xlab="Time",ylab="Number of individuals",
     col="#d55e00",lty=2)
lines(tvec,ivec,col="#009e73",lty=2)
lines(tvec,rvec,col="#56b4e9",lty=2)
#lines(tvec,svec+ivec+rvec,col="black") #sanity test
legend("right",legend=c("Susceptible","Infected","Removed"),col=c("#d55e00","#009e73","#56b4e9"),lty=1)

#Remark: numerical errors introduced breaks conservation of constant popsize. 

### Runge-Kutta method



