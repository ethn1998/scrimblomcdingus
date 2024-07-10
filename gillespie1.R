
### Gillespie Algorithm in R

set.seed(20101019)

### Easy example: A + A <-> AA reversible 
# Forward rate: kb, Backward rate kd

n0 <- 100

kf <- 0.2
kb <- 0.1

tmax <- 5.0 #end trajectory when t > 2.0

dt <- 0.001 #Trajectory for deterministic algorithm


#Initial conditions

tvec <- c(0.0)
navec <- c(n0)
nabvec <- c(0)

t <- 0.0
na <- n0
nab <- 0

#Gillespie Algorithm (Wikipedia example)
while(t<tmax){
  r <- runif(1,0,1)
  Rf <- kf*na*na #Binding rate #use na=nb here
  Rb <- kb*nab
  RTOT <- Rf + Rb  
  
  #Reaction
  if(r<Rf/RTOT){
    na <- na - 1
    nab <- nab + 1
  } else {
    na <- na + 1
    nab <- nab - 1
  }
  #Update
  t <- t + rexp(1,rate=RTOT)
  tvec <- append(tvec,t)
  navec <- append(navec,na)
  nabvec <- append(nabvec,nab)
}

plot(tvec,navec,col="black",type="l",lty=1,xlim = c(0,tmax),ylim=c(0,n0),
     xlab="Time",ylab="Number of molecules",main="A + B <-> AB")
lines(tvec,nabvec,col="red",lty=1)
#legend("right",legend=c("A","AB"),col=c("black","red"),lty=c(1,1))

#Euler method (Deterministic tracing)

tvec <- c(0.0)
navec <- c(n0)
nabvec <- c(0)

t <- 0.0
na <- n0
nab <- 0

while(t<tmax){
  #Update
  
  na <- na + dt*(-kf*na*na + kb*nab) #Binding rate
  nab <- nab + dt*(kf*na*na -kb*nab)
  t <- t + dt
  
  #Record
  tvec <- append(tvec,t)
  navec <- append(navec,na)
  nabvec <- append(nabvec,nab)
}
lines(tvec,navec,col="black",type="l",lty=3,xlim = c(0,tmax),ylim=c(0,n0),
     xlab="Time",ylab="Number of molecules",main="A + B <-> AB")
lines(tvec,nabvec,col="red",lty=3)

# Analytical trajectory
# Compare multiple dts
