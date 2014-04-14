##0 Getting Data set up
homes <- read.csv("homes2004.csv")

# create a var for downpayment being greater than 20%
homes$gt20dwn <- 
	factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)

##Question 1

## went through each of four characteristics to see effects on price/value

#NEIGHBORHOOD
#SMELLS - disappointingly, there wasn't much of a difference with odors
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ODORA, data=homes, xlab="Neighborhood has bad smells", ylab = "Current value of home")
plot(LPRICE ~ ODORA, data=homes,xlab="Neighborhood has bad smells", ylab = "Purchase price")
par(old.par)

#STREET NOISE - not that much difference
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ STRNA, data=homes, xlab="Neighborhood has street noise", ylab = "Current value of home")
plot(LPRICE ~ STRNA, data=homes,xlab="Neighborhood has street noise", ylab = "Puchase price")
par(old.par)

#ABANDONED BUILDINGS - see difference in values
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ EABAN, data=homes,xlab="Abandoned bldgs withn 1/2 blk", ylab = "Current value of home")
plot(LPRICE ~ EABAN, data=homes,xlab="Abandoned bldgs withn 1/2 blk",ylab="Purchase price")
par(old.par)

#HIGHWAY - value decreases with close highway
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ETRANS, data=homes,xlab="Rr/airport/hwy within 1/2 block", ylab="Current value of home")
plot(LPRICE ~ ETRANS, data=homes,xlab="Rr/airport/hwy within 1/2 block", ylab="Purchase price")
par(old.par)

#RATING OF NEIGHBORHOOD - positive effect
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ HOWH, data=homes)
plot(LPRICE ~ HOWH, data=homes)
par(old.par)

## DEMOGRAPHICS

#Household income - can't see patterns; data not evenly distributed
old.par <- par(mfrow=c(1, 2))
plot(log(VALUE) ~ log(ZINC2), data=homes)
plot(log(LPRICE) ~ log(ZINC2), data=homes)
par(old.par)

#educational level - positively correlated with educational level
plot(VALUE ~ HHGRAD, data=homes)

## GEOGRAPHY

# state - vary by state
plot(VALUE ~ STATE, data=homes)

#suburban / urban - rural more expensive than urban
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ METRO, data=homes)
plot(LPRICE ~ METRO, data=homes)
par(old.par)

## FINANCE

#INT
## The first thing that I did was plot price/value against the interest rate.  I think that this is interesting because of the bell shaped curve that you get when you plot in
old.par <- par(mfrow=c(1, 2))
boxplot(VALUE ~ INTW, data=homes, xlab="Int of first mortgage",ylab="Current value of home")
plot(LPRICE ~ INTW, data=homes,xlab="Int of first mortgage",ylab="Purchase price")
par(old.par)

#Plotting interest rate against state I thought was interesting since states either had a lot of variation (like Texas and Indiana) or barely any
plot(INTW ~ STATE, data=homes)

#The spread on interest rate and whether the downpayment came from the previous home was really interesting too. If it came from another source, the median wasn't that different, but the spread was much greater.
boxplot(INTW ~ DWNPAY, data=homes)

### NOT USED IN QUESTION 1

#again, this same strange bell curve appears when you plot the first mortgage amount against the interest rate
boxplot(AMMORT ~ INTW, data=homes)

#again, the spread was really big in the areas where you couldn't put the downpayment in the same year that you got the mortgage
plot(INTW ~ MATBUY, data=homes)

#again, there is a huge disparity fo interest rate when plotted against if you had abandoned apartments buildings next to you
plot(INTW ~ EABAN, data=homes)

##not that much differeince
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ EAPTBL, data=homes)
plot(LPRICE ~ EAPTBL, data=homes)
par(old.par)

##not that much differeince
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ECOM1, data=homes)
plot(LPRICE ~ ECOM1, data=homes)
par(old.par)

##not that much differeince
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ECOM2, data=homes)
plot(LPRICE ~ ECOM2, data=homes)
par(old.par)

##not that much difference
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ EGREEN, data=homes)
plot(LPRICE ~ EGREEN, data=homes)
par(old.par)

##not that much difference
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ EJUNK, data=homes)
plot(LPRICE ~ EJUNK, data=homes)
par(old.par)

##not that much difference
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ECOM2, data=homes)
plot(LPRICE ~ ECOM2, data=homes)
par(old.par)

##not that much difference
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ESFD, data=homes)
plot(LPRICE ~ ESFD, data=homes)
par(old.par)

##basically exactly the same
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ELOW1, data=homes)
plot(LPRICE ~ ELOW1, data=homes)
par(old.par)


##Question 2
classreg <- glm(log(VALUE) ~ .-AMMORT -LPRICE, data=homes)
pvals <- summary(classreg)$coef[-1,4]
source("fdr.R")
cutoff <- fdr_cut(pvals, 0.1)
names(pvals)[pvals<cutoff]
summary(classreg)
#think that there are 36 covariates that are significant
1 - 10359/14920 
# Rsquared for the first model is 0.3056971

names(pvals)[pvals>cutoff]
#dropped all of the names that came up
classreg2b <- glm(log(VALUE) ~ .-AMMORT -LPRICE -ECOM1 -EGREEN -ELOW1 -ETRANS -ODORA -PER -ZADULT, data=homes)
#r squared comparisions
summary(classreg2b)
1 - 10364/14920
# Rsquared for the second model is 0.3053619. Lower because though a better model, doesn't have a lot of unnecessary things that are creating an overfitting of the data.

##Question 3
summary(homes$gt20dwn)
classreg3a <- glm(gt20dwn ~ .-LPRICE - AMMORT, data=homes, family="binomial")
summary(classreg3a)
## looks like if this was someone's first home, they were less likely to have to put greater than 20% down by ~40%.  For every extra bathroom, 24% more likely to have to put down greater than 20%
classreg3b = glm(gt20dwn ~ . +FRSTHO*BATHS -AMMORT -LPRICE, data=homes, family="binomial")
summary(classreg3b)
## first home buyer, for every new bathroom only 10% more likely to have to pay mortgage greater than 20%. for return buyers, 30% more likely.

##Question 4

homes100 <- homes[homes$VALUE>100000,]
homes99 <- homes[homes$VALUE<100000,]
classreg4a = glm(gt20dwn ~ . +BATHS*FRSTHO -AMMORT -LPRICE, data=homes100, family="binomial")
summary(classreg4a)
#Rsquared for this is (1 - 13617/15210) = 0.1047337

classreg4b = glm(gt20dwn ~ . +BATHS*FRSTHO -AMMORT -LPRICE, data=homes99, family="binomial")
summary(classreg4b)
#Rsquared for this is (1- 2820.9/3086.0) = 0.08590408