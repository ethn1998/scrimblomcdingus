
#SIR epidemic model
### Constants


dt <- 0.02 #Step size for determininstic numerical methods
popsize <- 1000 #Population size
t0 <- 0.0
tmax <- 5.0

beta <- 6.0/popsize #Infection rate,
gamma <- 2.0 #Recovery rate
R_0 <- beta/gamma

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
  # Calculate reaction rates
  R_beta <- beta*s*i #Infection rate
  R_gamma <- gamma*i #Removal rate
  RTOT <- R_beta + R_gamma #Total rate
  if(RTOT>0){ #Only update while there's still something left to update
    p <- runif(1,0,1)
    # Determine Reaction
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
  } else {
    break
  }
}

png("gillespieSIRtraj.png",height=2250 ,width=2250 ,units="px",res=300)
plot(tvec,svec,xlim=c(0.0,tmax),ylim=c(0,popsize),type="l",xlab="Time",ylab="Number of individuals",
     main="SIR epidemic model",col="#d55e00")
lines(tvec,ivec,col="#009e73")
lines(tvec,rvec,col="#56b4e9")
legend("right",legend=c("Susceptible","Infected","Removed"),col=c("#d55e00","#009e73","#56b4e9"),lty=c(1,1,1))
dev.off()
### Tau leaping?


### Euler method for deterministic trajectory

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

png("eulerSIRtraj.png",height=2250 ,width=2250 ,units="px",res=300)
plot(tvec,svec,xlim=c(0.0,tmax),ylim=c(0,popsize),type="l",xlab="Time",ylab="Number of individuals",
     main="SIR Epidemic model", sub=sprintf("R0 = %.2e",R_0),
     col="#d55e00",lty=1,cex.main=2.0,cex.lab=1.5)
lines(tvec,ivec,col="#009e73",lty=1)
lines(tvec,rvec,col="#56b4e9",lty=1)
#lines(tvec,svec+ivec+rvec,col="black") #sanity check
legend("right",legend=c("Susceptible","Infected","Recovered"),col=c("#d55e00","#009e73","#56b4e9"),lty=c(1,1,1))
dev.off()

#Remark: numerical errors introduced breaks conservation of constant popsize. 
