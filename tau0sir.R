###Tau leaping
#This is like the Euler method

###SIR epidemic model
### Constants

tau <- 0.5 #fixed tau
maxguesses <- 1000 #Max guesses per jump


popsize <- 1000 #Population size
t0 <- 0.0
tmax <- 60.0

beta <- 1.0/popsize #Probability of infection
gamma <- 1.0/3.0 #Recovery rate

i0 <- 1.0*popsize/100 #Starting percentage of infected
s0 <- popsize - i0
r0 <- 0

###### Tau leaping
### Sanity checks:
# 1: s,i,r >0
# 2: s+i+r is constant

set.seed(20101019)

t <- t0
s <- s0
i <- i0
r <- r0

#tvec <- seq(t0,tmax,tau) #Try Deterministic fixed tau
leaps <- (tmax-t0)/tau #(tmax-t0)/tau

tvec <- c(t0)
svec <- c(s0)
ivec <- c(i0)
rvec <- c(r0)

while(t<tmax){
  trajfailed <- FALSE
  for(g in 1:maxguesses){
    pass <- TRUE #Boolean to determine if passed
    
    #Sample a candidate jump 
    n_beta <- rpois(n=1,lambda=tau*beta*s*i) #Number of infections
    n_gamma <- rpois(n=1,lambda=tau*gamma*i) #Number of removals
    #if(g > 0.5*maxguesses){ #
    #  print(sprintf("t:%.1e, Guess:%d",t,g))
    #  print(sprintf("s:%d i:%d r:%d",s,i,r))
    #  print(sprintf("Infections: %d Removals: %d",n_beta,n_gamma))
    #}
    if(s<n_beta || i<n_gamma){ #Criterion 1 sanity check
      pass <- FALSE
    }
    if(pass){ #Pass if sane
      t <- t+tau
      s <- s-n_beta
      i <- i+n_beta-n_gamma
      r <- r+n_gamma
      
      #Update
      tvec <- append(tvec,t)
      svec <- append(svec,s)
      ivec <- append(ivec,i)
      rvec <- append(rvec,r)
      break
    }
    if(g==maxguesses && !pass){ #If no good guesses at all
      print("Warning: Exceeded maximum candidate moves") #TERMINATE
      trajfailed <- TRUE
      break
    }
  }
  if(trajfailed){
    break #TERMINATE ALGORITHM
  }
}

#minl <- min(length(tvec),length(svec),length(ivec),length(rvec))

plot(tvec,svec,xlim=c(0.0,tmax),ylim=c(0,popsize),type="l",xlab="Time",ylab="Number of individuals",
     main="SIR epidemic model",sub=sprintf("(Fixed tau = %.2e)",tau),col="#d55e00") #Plot until end of epidemic
lines(tvec,ivec,col="#009e73")
lines(tvec,rvec,col="#56b4e9")

