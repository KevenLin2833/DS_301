library(ISLR2)
head(Default)
dim(Default)

set.seed(7)
train = sample(1:nrow(Default),nrow(Default)/2, replace=FALSE)
test = (-train)

glm.fit = glm(default~student+balance, data=Default, subset=train, family='binomial')

# use summary argument
summary(glm.fit)
head(glm.fit$fitted.values) #P(Y=1|X) 
head(Default[train,])


glm.prob = predict(glm.fit,Default[test,],type='response') 
head(glm.prob) #P(Y=1|X)
head(Default[test,])
## These just output your predicted probability. 

# What does R classify (internally) as 'Yes' or 'No'? 
contrasts(Default$default)

# How do we actually want to classify our test set? 
glm.pred = rep('No',length(test))
glm.pred[glm.prob >0.2] ='Yes'
table(glm.pred,Default[test,]$default) #rows are predicted, # columns are true 
# This matrix is called our confusion matrix

# what is our misclassification rate? 
1-mean(glm.pred == Default[test,]$default)

# Is this misclassification any good? What if I used a simple (but useless classifier) 
# that always classifies people as 'No' for Default?
# What would my misclassification rate be? 

# Looking at misclassifcation rate is not always the most meaningful thing, 
# especially if your categories are highly unbalanced. 
# In this context, what kind of error is more expensive for the bank? 

# Among individuals that actually defaulted, what is my misclassification rate? 
# (102)/(102+53) = 0.65 

## How can I try to bring down this specific misclassification rate (false negative rate)? 
## using 0.5 as your threshold, will give you the lowest overall misclassification rate. 

## you may care about specific misclassification rates more, and then you should adjust thresholding accordingly. 


### Another example of logistic regression: ####

library(ISLR2)
head(Smarket) # consists of percentage returns for the S&P 500 stock index over 1250 days from 2001 - 2005

# For each date, recorded the percentage returns for each of the previous trading days (Lag1 - Lag5)

# Volume (number of shares traded)
# Today (percentage return on the date in question)
# Direction (whether the market was up or down on this date)

train = (Smarket$Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)

glm.fit = glm(Direction~Lag1+Lag2, data=Smarket, subset=train, family='binomial')

summary(glm.fit)

## practice writing your own code to obtain the confusion matrix/misclassification rate for the test set (Year = 2005). 