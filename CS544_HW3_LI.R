library(UsingR)

#Part 1)
head(primes) #show head of the dataset
diff(primes)
table(diff(primes))
barplot(table(diff(primes)), main="Frequency of Prime Differences",xlab = "Prime differences", ylab = "Frequency",ylim=c(0,80),col="green")

#Part 2)
head(coins)
  #a)
x2 <- table(coins$value)
x2
  #b)
as.numeric(rownames(x2))*x2
  #c)
sum(as.numeric(rownames(x2))*x2)
#d)
barplot(table(coins$year), main="Number of coins by year", xlab="Year", ylab="Numbers of coins", col="pink")

#Part 3)
south
  #a)
stem(south)
#interpretation: the data has a concentration on the section ranging from 10 to 14, 
#and spreads from 6, the lowest value, to 33, the highest value.
  #b)
quantile(south)
lower <- quantile(south)[2]-1.5*IQR(south)
lower
upper <- quantile(south)[4]+1.5*IQR(south)
upper
south[south<=lower]
south[south>=upper]
  #c)
boxplot(south,horizontal=T,xaxt="n", xlab="south")
axis(side = 1, at = quantile(south), labels = TRUE, las=2)

#part 4)
head(pi2000)
  #a)
table(pi2000)
  #b)
table(pi2000)/sum(table(pi2000))
  #c)
hist(pi2000,main="Histogram of pi2000", ylab="Frequncy",xlab="Digits of pi2000",breaks=4)

#Part 5)
  #a)
mat5 <- matrix(c(25,10,15,20,40,30),nr=2,byrow=T)
mat5
  #b)
rownames(mat5) <- c("Men","Women")
mat5
  #c)
colnames(mat5) <- c("NFL","NBA","NHL")
mat5
  #d)
dimnames(mat5) <- list(Gender=rownames(mat5),Sport=colnames(mat5))
mat5
  #e)
margin.table(mat5,1)
margin.table(mat5,2)
  #f)
addmargins(mat5)
  #g)
prop.table(mat5,1)
#Among all the 50 male audiences, half of them prefer watching NFL, 20% of them like NBA and 30% of them prefer NHL.
#Among all the 90 female audiences, around 22% of them prefer NFL, 44% of them like NBA more and 33% prefer watching NHL.
prop.table(mat5,2)
#Among all the 45 audiences who like NFL, around 55.6% of which are men while 44.4% are women.
#Among all the 50 audiences who like NBA, around 20% of which are men while 80% are women.
#Among all the 45 audiences who like NHL, around 33.3% of thich are men and 66.7% are women.
  #h)
mosaicplot(mat5,color=rainbow(3))
par(mfrow=c(1,2))
barplot(margin.table(mat5,1),col=c("blue","red"),ylim=c(0,100),xlab="Gender",ylab="People",main="By Gender",legend.text = T)
barplot(margin.table(mat5,2),col=rainbow(3),ylim=c(0,50),xlab="Sport",ylab="People",main="By Sport",legend.text=T)

#part 6)
par(mfrow=c(1,1))
head(midsize)
pairs(midsize,pch=19)
  #interpretation:
  #1) The price of Honda Accord cars, in general, increased as they were sold more recently;
  #2) The price of Toyota Camry cars, in general, was higher as they were sold more recently;
  #3) The price of Ford Taurus cars, in general, increasd as they were sold more recently;
  #4) The prices of three mid-sized cars were in positive correlation and all peaked in 2004. 
#part 7)
head(MLBattend)
class(MLBattend)
# a)
BAL_wins <- MLBattend$wins[MLBattend$franchise=="BAL"]
BOS_wins <- MLBattend$wins[MLBattend$franchise=="BOS"]
DET_wins <- MLBattend$wins[MLBattend$franchise=="DET"]
LA_wins <- MLBattend$wins[MLBattend$franchise=="LA"]
PHI_wins <- MLBattend$wins[MLBattend$franchise=="PHI"]
#b)
dat5 <- data.frame("BAL"=BAL_wins,"BOS"=BOS_wins,"DET"=DET_wins,"LA"=LA_wins,"PHI"=PHI_wins)
dat5
#c)
boxplot(dat5,horizontal=T)
#d) interpretation
  #1) BAL has the widest range in terms of the number of wins between 1969 and 2000, which means that the number varied a lot;
  #2) BOS has the narrowest range in terms of the number of wins between 1969 and 2000, except the two outliers, which means that the number had little variation;
  #3) Generally, BAL has the largest number of wins than the other four teams between 1969 and 2000.
  #4) Generally, PHI is more symmetric than the other four teams between 1969 and 2000.
  #5) DET has the smallest number of wins than the other four teams between 1969 and 2000.
#part 8)
senate <- read.csv("http://kalathur.com/cs544/data/senate.csv")
house <- read.csv("http://kalathur.com/cs544/data/house.csv")
#a)
table(senate$Party)
table(house$Party)
#b)
barplot(table(senate$Party),col=c("blue","purple","red"),main="Numbers of Senate Members by Party Affiliation",xlab="Party Affiliation",ylab="Numbers")
barplot(table(house$Party),col=c("blue","purple","red"),main="Numbers of Senate Members by Party Affiliation",xlab="Party Affiliation",ylab="Numbers")
#c)
senate$Name[senate$Years_in_office==max(senate$Years_in_office)]
house$Name[house$Years_in_office==max(house$Years_in_office)]
#d)
sum(senate$Term_ends=="January 3, 2019")
#f)
table(house$State)
#g)
table(house$State)[table(house$State)==(max(sort(table(house$State))))]
#h)
table(house$State)[table(house$State)==(min(sort(table(house$State))))]
