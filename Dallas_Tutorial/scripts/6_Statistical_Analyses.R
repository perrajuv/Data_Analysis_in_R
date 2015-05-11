#---------------------------------#
#- Data Analysis & Graphics in R -#
#-------Statistical Analyses------#
#---------------------------------#

#Linear Correlation#
# Correlation is used to test for a relationship 
# between two numerical variables or two ranked (ordinal) variables.
# We will begin with a data set called "cats" from the "MASS" library, 
# which contains information on various features of house cats.

# Please install package if not already installed
# install.packages('MASS')

library("MASS")
data(cats)
str(cats)
summary(cats)
with(cats, plot(Bwt, Hwt))
title(main="Heart Weight (g) vs. Body Weight (kg)\nof Domestic Cats")
# Pearson's Correlation:
with(cats, cor(Bwt, Hwt))
#coefficient of determination:
with(cats, cor(Bwt, Hwt))^2
#test of significance:
with(cats, cor.test(Bwt, Hwt))
#For positive correlation:
with(cats, cor.test(Bwt, Hwt, alternative="greater", conf.level=.8))
#For a more revealing scatterplot:
with(cats, plot(Bwt, Hwt, 
                xlab="Body Weight in kg", 
                ylab="Heart Weight in g", 
                main="Heart Weight vs. Body Weight of Cats"))
with(cats,points(Bwt[Sex=="F"],Hwt[Sex=="F"],pch=16,col="red"))
with(cats,points(Bwt[Sex=="M"],Hwt[Sex=="M"],pch=17,col="blue"))
legend("topleft", c('Females','Males'), cex=0.60, horiz = FALSE, pch=c(16,17), col=c('red','blue'))



#Correlation and Covariance Matrices#
#If a data frame (or other table-like object) contains more than two numerical variables,
#then the cor( ) function will result in a correlation matrix.

rm(cats)                             # if you haven't already
data(cement)                         # also in the MASS library
str(cement)
cor(cement)
#If you prefer a covariance matrix, use cov( )... 
cov(cement)
#If you have a covariance matrix and want a correlation matrix... 
cov.matr = cov(cement)
cov2cor(cov.matr)
#If you want a visual representation of the correlation matrix (i.e., a scatterplot matrix)... 
pairs(cement)
#The command plot(cement) would also have done the same thing.
plot(cement)


#Correlations for Ranked Data#
ls()
rm(cement, cov.matr)                 # clean up first
coach1 = c(1,2,3,4,5,6,7,8,9,10)
coach2 = c(4,8,1,5,9,2,10,7,3,6)
ls()
cor(coach1, coach2, method="spearman")
cor.test(coach1, coach2, method="spearman")
cor(coach1, coach2, method="kendall")
cor.test(coach1, coach2, method="kendall")
ls()
rm(coach1,coach2)                    # clean up again

#Linear Regression#
data(cats) 
#attach(cats)                         
lm(Hwt ~ Bwt)
lm.out = lm(Hwt ~ Bwt)               # name the output anything you like
lm.out               
summary(lm.out)
#turn off the significance stars:
options(show.signif.stars=F)
#plotting the regression line on a scatterplot:
plot(Hwt ~ Bwt, main="Cat Hwt-Bwt Regression Plot")
abline(lm.out, col="red")
mypar <- par()              # make a copy of current settings
par(mfrow=c(2,2))
plot(lm.out)
cats[144,]
lm.out$fitted[144]
lm.out$residuals[144]
par(mfrow=c(1,1))                    # if you haven't already done this
plot(cooks.distance(lm.out))
lm.without144 = lm(Hwt ~ Bwt, subset=(Hwt<20.5))
lm.without144
rlm(Hwt ~ Bwt)
par(mypar)                  # restore original settings


#ANOVA#
#quick, easy way to rule out un-needed variables
#that contribute little to the explanation of a dependent variable
#Import cars dataset:
cars <- read.csv('/Users/atrivedi/R/Code/Dallas_Tutorial/data/Cars.csv')
head(cars)
a1 <- aov(HighwayMPG~Origin,data=cars)
summary(a1)
a2 <- aov(HighwayMPG~Origin+Type,data=cars)
summary(a2)
drop1(a2,~.,test='F')
a3 <- aov(HighwayMPG~Origin*Type,data=cars)
drop1(a3,~.,test='F')
#Plotting categorical-by-categorical interactions:
interaction.plot(cars$Origin,cars$Type,cars$HighwayMPG)
interaction.plot(cars$Origin,cars$Type,cars$HighwayMPG,pch=c(1,15,4,16,0,18),type='b')

#ANOVA to compare nested models
mymodel1 <- lm(HighwayMPG~Origin+Weight,data=cars)
mymodel2 <- lm(HighwayMPG~Origin+Weight+EngineSize+Price,data=cars)
anova(mymodel1,mymodel2)


#Logistic regression
adm <- read.csv('/Users/atrivedi/R/Code/Dallas_Tutorial/data/binary.csv')
head(adm)
adm$rank <- factor(adm$rank)
logit1 <- glm(admit~rank,data=adm,family='binomial')
summary(logit1)
logit2 <- glm(admit~gpa,data=adm,family='binomial')
summary(logit2)
logit3 <- glm(admit~rank+gre+gpa,data=adm,family='binomial')
summary(logit3)


#Plotting predicted probabilities for continuous predictor
mypp <- data.frame(gpa=sort(adm$gpa))
plot(adm$gpa,adm$admit,pch=16,cex=.5)
lines(mypp$gpa,predict(logit2,newdata=mypp,type='response'))



#Mixed models

popular <- read.csv('/Users/atrivedi/R/Code/Dallas_Tutorial/data/Popular.csv')

install.packages('lme4')
library(lme4)


#Random intercept:
mixed1 <- lmer(popular~(1|class),data=popular)
summary(mixed1)


#Adding fixed effects:
mixed2 <- lmer(popular~texp+sex+extrav+(1|class),data=popular)
summary(mixed2)


#Adding random slopes:
mixed3 <- lmer(popular~texp+sex+extrav+(1+sex|class),data=popular)
summary(mixed3)


#Plotting random slopes:
rnd_slopes <- by(popular,popular$class,function(popular) fitted(lm(popular~sex,data=popular)))
rnd_slopes <- unlist(rnd_slopes)
names(rnd_slopes) <- NULL
interaction.plot(popular$sex,popular$class,rnd_slopes)
