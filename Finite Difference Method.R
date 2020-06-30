### Code for implicit finite difference method
implicitSolution <- function(Smax,dS,T,dT,K,R,sigma,gamma,q) {
  #Smax: max stock price
  #dS: steps of stock price
  # T: time to maturity
  #dT: time step
  #K: Strike price
  # R: risk free rate
  # q: dividend
  # sigma: volatility
  #gamma: tunning parameter
  M <- ceil(Smax/dS)
  ds <- Smax/M
  N <- ceil(T/dT)
  dt <- T/N
  J <-  seq(1,M -1,by = 1)
put <- matrix(0:0, nrow = N+1, ncol = M+1)
put[N+1,] <- max(K - (seq(0,Smax,by=ds)*(1 -q))^gamma, 0)
put[,1] <- K*exp(-R*dt*(N - seq(0,N,by=1)))
put[,M+1] <- 0
for(i in N:1){
  if(i*dt<=T/2){
    a <-0.5*R*dt*J - 0.5.sigma^2*dt*J^2
    b <- 1+ sigma^2*dt*J^2 +R*dt
    c <- -0.5*R*dt*J - 0.5*sigma^2*J^2 
  }
  else
    {a <-0.5*(R -q)*dt*J - 0.5.sigma^2*dt*J^2
  b <- 1+ sigma^2*dt*J^2 +R*dt
  c <- -0.5*(R-q)*dt*J - 0.5*sigma^2*J^2  }
  
  A <- diag(b) + diag(a)
  
}

### Code for explicit finite difference method
explicitSolutions <- function(Smax,dS,T,dT,K,R,sigma,gamma,q) {
  #Smax: max stock price
  #dS: steps of stock price
  # T: time to maturity
  #dT: time step
  #K: Strike price
  # R: risk free rate
  # q: dividend
  # sigma: volatility
  #gamma: tunning parameter
  M <- ceil(Smax/dS)
  ds <- Smax/M
  N <- ceil(T/dT)
  dt <- T/N
  J <-  seq(1,M -1,by = 1)
  #Coefficents
  put <- matrix(0:0, nrow = N+1, ncol = M+1) #initial matrix
  put[N+1,] <- max(K - (seq(0,Smax,by=ds)*(1 -q))^gamma, 0)
  put[,1] <- K*exp(-R*dt*(N - seq(0,N,by=1)))
  put[,M+1] <- 0
  q1 <- 0
  for(i in N:1){
    if (i*dt >T/2){q1 <-q}
    else {q1 =0}
 a <- ( - 0.5*(R -q1)*dt*J + 0.5*sigma^2*dt*J^2)/(1 +R*dt)  
 b <- (1 - sigma^2*dt*J^2)/(1 + R*dt)
 c <- (0.5*(R-q1)*dt*J + 0.5*sigma^2*dt*J^2)/(1 +R*dt)  
 A <- diag(b) + diag(a[2:(M-1)], - 1) +  diag(c[1:(M-2)], 1)
   y <-  matrix(0:0, nrow = 1, ncol = M -1)
   
 
  }
}