# Knowledge Mining: Linear Discriminant Analysis
# File: Lab_LDA01.R
# Theme: Linear Discriminant Analysis
# Adapted from ISLR Chapter 4 Lab

# Setup
require(ISLR)
require(MASS)
require(descr)
attach(Smarket)

## Linear Discriminant Analysis
freq(Direction)
train = Year<2005
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit, col="dodgerblue")
Smarket.2005=subset(Smarket,Year==2005) # Creating subset with 2005 data for prediction
lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
Direction.2005=Smarket$Direction[!train] 
table(lda.class,Direction.2005) 
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)


From the three methods (best subset, forward stepwise, and backward stepwise):
a. Which of the three models with k predictors has the smallest training RSS?
From these methods, best subset selects the model with the smallest training RSS. 
This method searches through all the subsets of predictors and picks the one 
with the lowest RSS on the training data. 

b. Which of the three models with k predictors has the smallest test RSS?
In order to pick the model with the smallest test RSS, we need to estimate the error. 
We can do this using validation or cross-validation, or indirectly estimate the
error by making an adjustment to the training error to account for the bias that 
comes from overfitting
It also depends on other factors such as number of variables and curve of the data. 
Cp, AIC, BIC, and Adjusted R2 are all techniques for adjusting the training error
and allows us to select amongst models with different numbers of variables. 
Cp: This statistic takes on a small value when the test error is also small. 
AIC: This statistic is best for a large class of models fit by maximum likelihood
BIC: Dervied from a bayesian perspective. This also takes on a smaller value 
as the test error decreases. This method adds a heavier penalty on models with 
many variables which results in smaller model selection compared to Cp. 
R^2: this statistic increases as test error decreases. 


  

