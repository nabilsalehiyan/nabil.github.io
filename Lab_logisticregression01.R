# Knowledge Mining: Logistic Regression
# File: Lab_logisticregression01.R
# Theme: Logistic regression
# Adapted from ISLR Chapter 4 Lab

# Load ISLR library

require(ISLR)

# Check dataset Smarket
?Smarket
names(Smarket)
summary(Smarket)

# Create a dataframe for data browsing
sm=Smarket

# Bivariate Plot of inter-lag correlations
pairs(Smarket,col=Smarket$Direction,cex=.5, pch=20)

# Logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response") 
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)

# Make training and test set for prediction
train = Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial, subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
#Fit smaller model
glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket,family=binomial, subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

# Check accuracy rate
106/(76+106)

# Can you interpret the results? (ask about this)
## The model correctly predicted the direction of the stock market 48% of the time 
## when fitted to a set with all the variables
## When fit to a smaller data set (some variables removed) the model predicts 
## direction with 56% accuracy. This higher accuracy score suggests that lag1 and 
## lag2 have a stronger relationship to the direction of the stock market
## The model is has an accuracy score of 58% when predicting "up", which means
## for some reason the model is better at predicting just "up" compared to both
## up and down


