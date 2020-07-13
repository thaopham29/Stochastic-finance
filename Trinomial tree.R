#Code of trinomial tree for knock-out put option
Down_and_Out <- function(K,T,S,sigma,r,H,Nt,div,tdiv,isUS) {
# H: the barrier (S >=H)
#K: strike price
# T: maturity time
# S: stock price
# sigma: volatility
# r: risk-free rate
# Nt: number of time steps (Nt*dt = T)
#div: vector of dividends (eg: at time 6-month and 12-month)
#tdiv: vector of ex-dividend time
#isUS: flag to indicate if it's an American option (true) or European option (FALSE)
  
  dt <- T/Nt
  q <- sum(div) #total dividend
 #finding (u,d) and (pd,pm,pu) 
Nu <- round(-log(H) +log(S))/(sigma*sqrt(3*dt)+0.5)
u <- exp((log(S) -log(H))/Nu
d <- 1/u
#For the last "branch of the tree
St <- rep(0,Nt+1)
St[1] <- S*d^Nt*(1-q)
for(i in 2:2*Nt+1){
  St[j] <-St[j-1]*u
}
for (i in 1:2*Nt+1) #C has the same dimension as S
{
  if (St[j]>H){C[j]<- max(K - St[j], 0)}
  else {C[j]<- 0}
}
#Going backward to the "root" of the trinomial tree
for (i in Nt-1:1) 
{if(i*dt<=tdiv[1]) {q <- 0} #Before month 6
else if(i*dt<=tdiv[2])  {q <- div[1]} #Before month 12

#probability of down/middle/up (adding total dividend)
pd <- -(r -q-sigma^2/2)*dt/(2*log(u)) +sigma^2*dt/2/log(u)^2
pm <- 1 -simga^2*dt/(log(u)^2)
pu <- (r - q -sigma^2/2)*dt/2/log(u) + sigma^2*dt/2/log(u)^2
St <- rep(0,2*i+1)
St[1] <- S*d^i*(1-q)
#Values of put on the right-side of the current level
Cr <- C
C <- rep(1,2*i+1) #initialize new vector for put
for (j in 2:2*i+1){
  St[j] <- St[j -1]*u
  if(St[j] > H){
    C[j] <- exp(-r*st)*(pd*Cr[j] +pm*Cr[j+1]+pu*Cr[j+2]) 
    if(isUS){C[j]<- max(C[j], K - St[j])}}
  else {C[j]<- 0}
}
}
#At the root (of the tree)
Cr <- C
St <- S*(1 -q) #now q should be 0
C <- exp(-r*dt)*(pd*Cr[1]) + pm*Cr[2] +pu*Cr[3]
return (C)
}