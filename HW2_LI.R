library("prob")
#part 1
# a)
  #i)randomly selected non-smoker has lung disease 
0.07*0.1/(0.07*0.1+0.93*0.75)
  #ii)randomly selected non-smoker does not have lung disease 
0.93*0.75/(0.07*0.1+0.93*0.75)
  #verification with Bayes Rule
bayes <- function (prior, likelihood) {
  numerators <- prior * likelihood
  return (numerators / sum(numerators)) 
}

prior <- c(0.07, 0.93)
likelihood <- c(0.1, 0.75)
bayes(prior, likelihood)
# b)
  #i) favor & democrat
0.45*0.7/(0.45*0.7+0.5*0.4+0.05*0.2)
  #ii) favor & republican
0.5*0.4/(0.45*0.7+0.5*0.4+0.05*0.2)
  #iii) favor & independent
0.05*0.2/(0.45*0.7+0.5*0.4+0.05*0.2)
  #verification with Bayes Rule
bayes <- function (prior, likelihood) {
  numerators <- prior * likelihood
  return (numerators / sum(numerators)) 
}

prior <- c(0.45,0.5, 0.05)
likelihood <- c(0.7,0.4, 0.2)
bayes(prior, likelihood)

#part 2
  # a)
S <- rolldie(2,makespace= T)
S <- addrv(S, A=abs(X1-X2))
S
# b)
P1 <- subset(S, A == 3)
P1
Prob(P1)
  # c)
marginal(S, vars="A")
  # d)
S <- addrv(S, B=X1+X2)
S
logtmp <- function(B){
  ifelse(B>6, B <- "TRUE", B <- "FALSE")
}
S <- addrv(S, FUN=logtmp, invars="B", name="C")
S

#alternative
S <- addrv(S, H=(X1+X2>6))
S

# All the ways dont work out to calculate the probability of X1+X2>6:
#P2 <- subset(S, B>"6") Prob(P2)
#Prob(S,X1+X2>6)
#Prob(S, C=="FALSE")
marginal(S, vars="C")

#part 3
  # 1.
all.primes = function(n) {
  source = 2:n
  result=NULL
  while (length(source)>0) {
    result=c(result,source[1])
    i=which(source%%source[1]==0)
    source=source[-i]
  }
  return(result)
}

all.primes(10)

