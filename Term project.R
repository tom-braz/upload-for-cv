#****setting things up****
rm(list=ls()) 
expectancy <- read.csv("life.csv") 
#Clearing data then loading in life expectancy data
library(MASS)#general stats 
library(ISLR)#general stats
library(VIM) #pretty graphs
library(class)#classification things (not sure I use this)
library(boot)# for bootstrapping
library(corrplot) #for correlationplot
library(car) # for outliers test might not be needed, maybe delete
library(glmnet) # For Lasso/Ridge glmnet function
library(pls)  # For PCR and PLSR functions
library(leaps)#regression subset studd
library(splines) # for splines
library(gam) #for gam
library(mgcv) #for gam R^2
library(tree) # Regression Trees
library(randomForest) # Bagging and Random Forest
library(gbm) # Boosting
library(rpart) # regression trees things
library(dplyr)#for tables
library(tidyr)#for tables
library(broom)#for tables
#libraries
set.seed(1)
#setting seed

#****preliminary data analysis****
summary(expectancy)
#preliminary data investigation
attach(expectancy)
#To make it a bit easier to access data
dim(expectancy)
#2922 variables with 22 categories
is.na(expectancy)
#looking at which data points have N/A values
apply(is.na(expectancy), 2, which)
#where and which data points have N/A values
expectancy <- na.omit(expectancy)
#removing N/A values
dim(expectancy)
#returns 1638 22, thus removing 1284 due to N/A removal.Need to justify
expectancy <- subset (expectancy, select = -Country)
expectancy <- subset (expectancy, select = -Year)
#removing country and year as it has no explanatory power
expectancy <- subset (expectancy, select = -Status)
#removing Status as it has no explanatory power
round(cor(expectancy), 2)
#Getting correlation values

#                                   Life.expectancy 
#Year                                         0.05           
#Life.expectancy                              1.00         
#Adult.Mortality                             -0.70           
#infant.deaths                               -0.17         
#Alcohol                                      0.40           
#percentage.expenditure                       0.41         
#Hepatitis.B                                  0.20          
#Measles                                     -0.07           
#BMI                                          0.54           
#under.five.deaths                           -0.19           
#Polio                                        0.33           
#Total.expenditure                            0.17          
#Diphtheria                                   0.34           
#HIV.AIDS                                    -0.59            
#GDP                                          0.44           
#Population                                  -0.02           
#thinness..1.19.years                        -0.46            
#thinness.5.9.years                          -0.46            
#Income.composition.of.resources              0.72           
#Schooling                                    0.73 
#thinness 1.19 and thinness 5.9 0.93 corr therefore will only pick one
#Excluding life expecancy (perf cor) and adult mortality (very high corr as very similar to life expecency)
# highest are schooling (0.73), Income.composition.of.resource(0.72), Adult Mortality(-0.7),
# HIV(0.59) BMI (0.54), thiness..1.19 (-0.46), GDP (0.44),percentage.expenditure (0.41), Alcohol (0.40)

#****removing outliers****
lm.full <- lm(Life.expectancy~., data=expectancy)
summary(lm.full)
#Just checking the model above is useful and comparing with full model
lm.fit <- lm(Life.expectancy ~ Schooling+Income.composition.of.resources+Adult.Mortality+HIV.AIDS+BMI+thinness..1.19.years+GDP+percentage.expenditure +Alcohol, data=expectancy)
#fitting the model based on significance from full model, all variables have low p values
par(mfrow=c(1,2))
#splitting graph space
plot(abs(rstudent(lm.fit))>3, pch="*", cex=2, main="Influential Obs by r student")
#plotting outliers
abline(h = abs(rstudent(lm.fit))>3, col="red")
#adding cut off red line
plot(abs(hatvalues(lm.fit)) > 0.06, pch="*", cex=2, main="Influential Obs by hat values")
#plotting leverage values
abline(h = abs(hatvalues(lm.fit)) > 0.06, col="red")
# add cutoff line
index_outlier <- which(abs(rstudent(lm.fit))>3)
#putting outliers into a vector
expectancy <- expectancy[-index_outlier, ]
#removing those outliers
index_highlev <- which(hatvalues(lm.fit) > 0.06)
#putting high leverage points into a vector
expectancy <- expectancy[-index_highlev, ]
#removing those high leverage points

#****Data analysis****
if(!is.null(dev.list())) dev.off()
#clearing graph space
par(mfrow=c(7,9))
#segmenting graph space
plot(expectancy$Alcohol, expectancy$Life.expectancy, main ="Life expectancy against Alcohol consumption per capita (15+) (in litres of pure alcohol)")
plot(expectancy$percentage.expenditure, expectancy$Life.expectancy, main ="Life expectancy against percentage of gdp spent on health expenditure")
plot(expectancy$GDP, expectancy$Life.expectancy, main ="Life expectancy against GDP per capita in dollars")
plot(expectancy$thinness..1.19.years, expectancy$Life.expectancy, main ="Life expectancy against Prevalence of thinness among children and adolescents for Age 10 to 19 (%)")
plot(expectancy$BMI, expectancy$Life.expectancy, main ="Life expectancy against average BMI")
plot(expectancy$HIV.AIDS, expectancy$Life.expectancy, main ="Life expectancy against HIV/AIDs preverlance per 1000 live births")
plot(expectancy$Adult.Mortality, expectancy$Life.expectancy, main ="Life expectancy against probability of dying between 15 and 60 years per 1000 population")
plot(expectancy$Income.composition.of.resources, expectancy$Life.expectancy, main ="Life expectancy against income composition of resources")
plot(expectancy$Schooling, expectancy$Life.expectancy, main ="Life expectancy against average years of schooling")
plot(expectancy$infant.deaths, expectancy$Life.expectancy, main ="Life expectancy against infant deaths per 1000 population")
plot(expectancy$under.five.deaths, expectancy$Life.expectancy, main ="Life expectancy against number of under 5 deaths per 1000 births")
plot(expectancy$Diphtheria, expectancy$Life.expectancy, main ="Life expectancy against income composition of resources")
plot(expectancy$Total.expenditure, expectancy$Life.expectancy, main ="Life expectancy against percentage of government spending on health")
#plotting graphs of significant varibales to have a look
if(!is.null(dev.list())) dev.off()
#clearing graph space
par(mfrow=c(2,7))
#segmenting graph space
hist (expectancy$Life.expectancy,freq = FALSE, breaks=40, col="violet", xlab="Life expectancy",las =1, main=" Life expectancy histogram")
lines(density(expectancy$Life.expectancy, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$Alcohol,freq = FALSE, breaks=40, col="violet", xlab="Alcohol consumption",las =1, main=" Alcohol consumption histogram")
lines(density(expectancy$Alcohol, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$percentage.expenditure,freq = FALSE, breaks=40, col="violet", xlab="percentage expenditure",las =1, main=" percentage expenditure histogram")
lines(density(expectancy$percentage.expenditure, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$GDP,freq = FALSE, breaks=40, col="violet", xlab="GDP",las =1, main="GDP histogram")
lines(density(expectancy$GDP, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$thinness..1.19.years,freq = FALSE, breaks=40, col="violet", xlab="Thiness 10-19",las =1, main=" Thiness 10-19 histogram")
lines(density(expectancy$thinness..1.19.years, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$BMI,freq = FALSE, breaks=40, col="violet", xlab="BMI",las =1, main="BMI histogram")
lines(density(expectancy$BMI, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$HIV.AIDSl,freq = FALSE, breaks=40, col="violet", xlab="HIV/AIDS",las =1, main=" HIV/AIDS histogram")
lines(density(expectancy$HIV.AIDS, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$Adult.Mortality,freq = FALSE, breaks=40, col="violet", xlab="Adult mortality",las =1, main=" Adult mortality histogram")
lines(density(expectancy$Adult.Mortality, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$Income.composition.of.resources,freq = FALSE, breaks=40, col="violet", xlab="income composition of resources",las =1, main=" income composition of resource histogram")
lines(density(expectancy$Income.composition.of.resources, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$Schooling,freq = FALSE, breaks=40, col="violet", xlab="Schooling",las =1, main="Schooling histogram")
lines(density(expectancy$Schooling, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$infant.deaths,freq = FALSE, breaks=40, col="violet", xlab="Infancy deaths",las =1, main="Infancy deaths histogram")
lines(density(expectancy$infant.deaths, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$under.five.deaths,freq = FALSE, breaks=40, col="violet", xlab="Under five deaths",las =1, main="Under five deaths histogram")
lines(density(expectancy$under.five.deaths, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$Diphtheria,freq = FALSE, breaks=40, col="violet", xlab="Diptheria",las =1, main="Diptheria histogram")
lines(density(expectancy$Diphtheria, na.rm=TRUE), lwd=4, col="red")
hist (expectancy$Total.expenditure,freq = FALSE, breaks=40, col="violet", xlab="Total expenditure",las =1, main="Total expenditure histogram")
lines(density(expectancy$Total.expenditure, na.rm=TRUE), lwd=4, col="red")
#creating histograms for all the variables 
mydata.cor=cor(expectancy)
#creating correlation thing
if(!is.null(dev.list())) dev.off()
#clearing graph space
corrplot(mydata.cor)
#creating a correlation graph

#****multiple linear regression****#
train <- sample(1:length(expectancy$Life.expectancy), 1200)
expectancy.train <- expectancy[train,]
expectancy.test <- expectancy[-train,]
#test and train data sets
lm.full <- lm(Life.expectancy~., data=expectancy.train)
#running the full linear regression model
summary(lm.full)
#getting the statistics
RSS <- c(crossprod(lm.full$residuals))
#getting residual sum of squares
MSE <- RSS / length(lm.full$residuals)
#mean sum 
RMSE <- sqrt(MSE)
#square root of this
RMSE
#reporting it
lm.fit <- lm(Life.expectancy ~ Schooling+Income.composition.of.resources+Adult.Mortality+HIV.AIDS+BMI+thinness..1.19.years+GDP+percentage.expenditure +Alcohol+under.five.deaths, data=expectancy.train)
#fitting my selected modal
summary(lm.fit)
#getting statistics of it
RSS <- c(crossprod(lm.fit$residuals))
#getting residual squares
MSE <- RSS / length(lm.fit$residuals)
#mean sum
RMSE <- sqrt(MSE)
#square root of this
RMSE
#reporting this



#****forwardwise selection****
reg.fwd <- regsubsets(Life.expectancy ~ ., data = expectancy, nvmax = 19, method = "forward")
#running regular subsets
reg.summary.fwd <- summary(reg.fwd)
#putting the summary into a variable
reg.summary.fwd
#reporting this
dev.off()
#clearing previous parameters
par(mfrow=c(2,2))
#putting in 4 graph spots
plot(reg.summary.fwd$cp, xlab = "No. of variables", ylab = "Cp", type = "l")
points(which.min(reg.summary.fwd$cp), reg.summary.fwd$cp[which.min(reg.summary.fwd$cp)], col = "red", cex = 2, pch = 20)
which.min(reg.summary.fwd$cp)
#plotting graph of AIC
#cp 12 but pretty flat from 9
plot(reg.summary.fwd$bic, xlab = "No. of variables", ylab = "BIC", type = "l")
points(which.min(reg.summary.fwd$bic), reg.summary.fwd$bic[which.min(reg.summary.fwd$bic)], col = "red", cex = 2, pch = 20)
which.min(reg.summary.fwd$bic)
#plotting graph of BIC
#BIC 9
plot(reg.summary.fwd$adjr2, xlab = "No. of variables", ylab = "Adj. R2", type = "l")
points(which.max(reg.summary.fwd$adjr2), reg.summary.fwd$adjr2[which.max(reg.summary.fwd$adjr2)], col = "red", cex = 2, pch = 20)
which.max(reg.summary.fwd$adjr2)
#PLotting adj R^2
#Adj R^2 14 but pretty flat from 9 onwards
#constantly falling (or increasing in r2) but not much benefit after 6/7
coef(reg.fwd,9)
#reporting the coefficents

#****doing backwardswise selection****

reg.bwd <- regsubsets(Life.expectancy ~ ., data = expectancy, nvmax = 19, method = "backward")
#regsubsets to create the backwards subsets
reg.summary.bwd <- summary(reg.bwd)
#putting the summary statistics in here
reg.summary.bwd
#reporting this
par(mfrow=c(2,2))
#creating 4 graph spots
plot(reg.summary.bwd$cp, xlab = "No. of variables", ylab = "Cp", type = "l")
points(which.min(reg.summary.bwd$cp), reg.summary.bwd$cp[which.min(reg.summary.bwd$cp)], col = "red", cex = 2, pch = 20)
which.min(reg.summary.bwd$cp)
#plotting AIC
# Cp = 12 variables bu flat after 9
plot(reg.summary.bwd$bic, xlab = "No. of variables", ylab = "BIC", type = "l")
points(which.min(reg.summary.bwd$bic), reg.summary.bwd$bic[which.min(reg.summary.bwd$bic)], col = "red", cex = 2, pch = 20)
which.min(reg.summary.bwd$bic)
#Plotting BIC
# BIC = 9 vars
plot(reg.summary.bwd$adjr2, xlab = "No. of variables", ylab = "Adj. R2", type = "l")
points(which.max(reg.summary.bwd$adjr2), reg.summary.bwd$adjr2[which.max(reg.summary.bwd$adjr2)], col = "red", cex = 2, pch = 20)
which.max(reg.summary.bwd$adjr2)
#plotting adj R^2
#adj R^2 is 14 but relatively flat around 9
coef(reg.bwd,9)
#getting the coefficents
lm.fit <- lm(Life.expectancy ~ Schooling+Income.composition.of.resources+Adult.Mortality+HIV.AIDS+BMI+percentage.expenditure +infant.deaths+under.five.deaths+Diphtheria, data=expectancy.train)
#fitting a linear model of this 
summary(lm.fit)
#summary of lm
RSS <- c(crossprod(lm.fit$residuals))
#getting residual sum of squares
MSE <- RSS / length(lm.fit$residuals)
#making these mean squared error
RMSE <- sqrt(MSE)
#taking the square root so its RMSE
RMSE
#reporting this

#****doing ridge****
train.X <- model.matrix(Life.expectancy ~ ., data = expectancy.train)[,-1]
test.X <- model.matrix(Life.expectancy ~ ., data = expectancy.test)[,-1]
#creating an input matrix, of dimension nobs x nvars; each row is an observation vector for both test and train
grid = 10^seq(10, -2, length = 100)
#specifying a grid of lambdas ranging from lambda=10^10 to lambda=10^-2 esentially covering from least squares to null with just intercept
ridge.mod <- glmnet(train.X, expectancy.train$Life.expectancy, alpha = 0, lambda = grid)
#running a ridge regression without training
summary(ridge.mod)
#summary of this
ridge.cv <- cv.glmnet(train.X, expectancy.train$Life.expectancy, alpha = 0, lambda = grid)
#getting a k-fold (10 folds) error 
dev.off()
#clearing previous parameters
plot(ridge.cv)
#just to have a look at whats going on
ridge.bestlam <- ridge.cv$lambda.min
#getting the best lamda
ridge.bestlam
#finding the best lambda value (0.01)
ridge.pred <- predict(ridge.mod, s = ridge.bestlam, newx = test.X)
#predicting the ridge model with the best lambda
sqrt(mean((ridge.pred - expectancy.test$Life.expectancy)^2))
#mean squared error (comes out as 12.18047)
predict(ridge.mod,type="coefficients",s=ridge.bestlam)
#predicting the ridge
eval_results <- function(true, predicted, df) {
SSE <- sum((predicted - true)^2)
SST <- sum((true - mean(true))^2)
Rsquarex <- 1 - SSE / SST
Rsquare <- 1-((1-Rsquarex)*(df-1))
RMSE = sqrt(SSE/nrow(df))
# Model performance metrics
data.frame(
RMSE = RMSE,
Rsquare = R_square)}
#function to work out R^2 and stuff
eval_results(expectancy.test$Life.expectancy, ridge.pred, expectancy.test)
#reporting this
ridge.1se <- ridge.cv$lambda.1se
#running a ridge with a lamda and 1 standard deviation
ridge.1se
#returns 0.1629751
ridge.pred <- predict(ridge.mod, s = ridge.1se, newx = test.X)
#predicting this
eval_results(expectancy.test$Life.expectancy, ridge.pred, expectancy.test) 
#returns root mean squared error ( comes out as 12.44297)
mod = lm.ridge(Life.expectancy ~ ., expectancy,     lambda = seq(0,0.1,0.001))
#running a ridge regression of this
plot(mod)
#plotting this

#****doing lasso****
train.X <- model.matrix(Life.expectancy ~ ., data = expectancy.train)[,-1]
test.X <- model.matrix(Life.expectancy ~ ., data = expectancy.test)[,-1]
#setting up X as above 
grid = 10^seq(10, -2, length = 100)
#lambda grid
lasso.mod <- glmnet(train.X, expectancy.train$Life.expectancy, alpha = 1, lambda = grid)
#running a lasso regression using grid from ridge
lasso.cv <- cv.glmnet(train.X, expectancy.train$Life.expectancy, alpha = 1, lambda = grid)
#getting cv of lasso
plot(lasso.cv)
#plotting to have a look
lasso.bestlam <- lasso.cv$lambda.min
#getting the best lamda
lasso.bestlam
# reporting the best lambda (0.01)
lasso.pred <- predict(lasso.mod, s = lasso.bestlam, newx = test.X)
#fitting best lambda to training data
eval_results(expectancy.test$Life.expectancy, lasso.pred, expectancy.test) 
#results
sqrt(mean((lasso.pred - expectancy.test$Life.expectancy)^2))  
#getting mean squared error =12.12476
predict(lasso.mod,type="coefficients",s=lasso.bestlam)
#getting coefficents
lasso.bestlam2 <- lasso.cv$lambda.1se
#getting the best lamda
lasso.bestlam2
#reporting second lambda based of 1se =0.03184747
lasso.pred2 <- predict(lasso.mod, s = lasso.bestlam2, newx = test.X)
#applying this to test
eval_results(expectancy.test$Life.expectancy, lasso.pred2, expectancy.test)
#getting evaluating things
sqrt(mean((lasso.pred2 - expectancy.test$Life.expectancy)^2))
# getting the mean squared error =12.28949
predict(lasso.mod,type="coefficients",s=lasso.bestlam2)
#getting coeffients for model with second lambda

#****Doing PCR****
pcr.mod <- pcr(Life.expectancy ~ ., data = expectancy.train, scale = TRUE, validation = "CV")
#setting up pcr 
summary(pcr.mod)
#Having a look
par(mfrow=c(1,1))
#partitioning the graph space into just one
validationplot(pcr.mod, val.type = "MSEP")
#looking at how many components we need
pcr.pred = predict(pcr.mod, expectancy.test, ncomp=9)
#for now just picked 9, might need to check if its okay
Rmse.pcr <- sqrt(mean((expectancy.test$Life.expectancy - pcr.pred)^2))
#getting rmse
Rmse.pcr
#reporting rmse
validationplot(pcr.mod, val.type="R2")
#gives us the mse error = 12.92383

#****doing patrial least squares****
pls.mod <- plsr(Life.expectancy ~ ., data = expectancy.train, scale = TRUE, validation = "CV")
#running partial least squares
summary(pls.mod)
#computing a partial least squares
validationplot(pls.mod, val.type = "MSEP")
#checking how many components is optimal
pls.pred <- predict(pls.mod, expectancy.test, ncomp = 3)
#computing the test data pls but with 4, again eyeballed
rmse.pls <- sqrt(mean(( expectancy.test$Life.expectancy - pls.pred)^2))
#getting rmse
rmse.pls
#reporting rmse


#****doing General Addative Model****
gam.fit <- gam(Life.expectancy ~ Schooling + HIV.AIDS + Adult.Mortality, data = expectancy.train)
#running the model
gam.fit <- gam(Life.expectancy ~ s(Schooling, df=4)  + s(HIV.AIDS, df=4) + s(Adult.Mortality, df=4), data = expectancy.train)
#model fit without tuning (just assuming df=4) using forwards selection top 3
plot(gam.fit, se = T, col = "green")
#model as above, just having a look
cv.err <- rep(0, 17) 
cv.err1 <- rep(0, 17)
cv.err2 <- rep(0, 17)
#vectors to put in cv error
for (i in 3:20) {
  cv.fit <- glm(Life.expectancy ~ bs(Schooling, df = i), data = expectancy.train)
  cv.err[i-2] <- cv.glm(expectancy.train, cv.fit, K = 10)$delta[1]
  for (i in 3:20) {
    cv.fit1 <- glm(Life.expectancy ~ bs(HIV.AIDS, df = i), data = expectancy.train)
    cv.err1[i-2] <- cv.glm(expectancy.train, cv.fit1, K = 10)$delta[1]
  
  for (i in 3:20) {
    cv.fit2 <- glm(Life.expectancy ~ bs(Adult.Mortality, df = i), data = expectancy.train)
    cv.err2[i-2] <- cv.glm(expectancy.train, cv.fit2, K = 10)$delta[1]
  }
    }
} 
#running a for loop to find best degrees of freedom
par(mfrow=c(3,1))
#partitioning the graph space into 3
plot(1:18, cv.err, ylab = "Test MSE", xlab = "Degrees of Freedom", main="schooling", type = "l") 
DOFS <- which.min(cv.err)
#plotting cv against schooling
DOFS
#reporting df
plot(1:18, cv.err1, ylab = "Test MSE", xlab = "Degrees of Freedom",main="HIV", type = "l") 
DOFH <- which.min(cv.err1)
#plotting HIV against cv error
DOFH
#reporting df
plot(1:18, cv.err2, ylab = "Test MSE", xlab = "Degrees of Freedom", main="adult mortality", type = "l") 
DOFA <- which.min(cv.err2)
# plotting adult mortality against cv error
DOFA
#reporting df
gam.fit <- gam(Life.expectancy ~ s(Schooling, df=13)  + s(HIV.AIDS, df=8) + s(Adult.Mortality, df=14), data = expectancy.test)
#running the gam
par(mfrow=c(3,1))
#splitting the graphing space
plot(gam.fit)
#plotting gam
rmse.pls <- sqrt(mean(( expectancy.test$Life.expectancy - gam.fit)^2))
#computing rmse
rmse.pls
#reporting rmse
 
  
#****doing regression trees****
regtree.expectancy  <- tree(Life.expectancy ~ ., data = expectancy.train)
#running a regression tree on the full model
summary(regtree.expectancy)
#will give how many variables, residual mean deviance and distribution of variables
par(mfrow=c(1,1))
#splitting the graphing space into 1
plot(regtree.expectancy)
# to plot tree structure
text(regtree.expectancy, pretty = 0)
#putting the test on the tree
pred.expectancy <- predict(regtree.expectancy, newdata = expectancy.test) #predict function is overloaded 
#and will look at regression object and call the right function
rmse= sqrt(mean((pred.expectancy - expectancy.test$Life.expectancy)^2))
#computing rmse
rmse
#reporting rmse
cv.expectancy <- cv.tree(regtree.expectancy)
#getting the cv error
cv.expectancy
#reporting the cv error
which.min(cv.expectancy$dev)
#which is the minimum
par(mfrow=c(1,2))
#splitting the graphing space into two
plot(cv.expectancy$size, cv.expectancy$dev, type = "b")
#plotting the min cv error
plot(cv.expectancy$k, cv.expectancy$dev, type = "b")
#plotting the min cv error
meanerror <- rep(0, 9)
#creating a vector of zeros
for(i in 2:10){
prune.expectancy<- prune.tree(regtree.expectancy,best = i)
pred.sales2 <- predict(prune.expectancy, newdata = expectancy.test)
meanerror[i] <- mean((pred.sales2 - expectancy.test$Life.expectancy)^2)
}
for(i in 1:9){
prune.expectancy<- prune.tree(regtree.expectancy,best = i)
pred.sales2 <- predict(prune.expectancy, newdata = expectancy.test)
meanerror[i] <- mean((pred.sales2 - expectancy.test$Life.expectancy)^2)
}
meanerror
for(i in 2:9){
prune.expectancy<- prune.tree(regtree.expectancy,best = i)
pred.sales2 <- predict(prune.expectancy, newdata = expectancy.test)
meanerror[i] <- mean((pred.sales2 - expectancy.test$Life.expectancy)^2)
}
#all these for loops are just for me to check the errors and make sure that the best tree is full...
meanerror
#reporting the mean error
par(mfrow=c(1,1))
#partitioning the graph space
prune.expectancy<- prune.tree(regtree.expectancy,best = 8)
#using weakest link pruning, using 7 from above
plot(prune.expectancy)
#plotting the pruning expectancy
text(prune.expectancy,pretty=0)
#putting the text on
pred.sales2 <- predict(prune.expectancy, newdata = expectancy.test)
#predicting the prune expectancy
sqrt(mean((pred.sales2 - expectancy.test$Life.expectancy)^2)) 
# running root mean square error


#****doing bagging****
bagging.expectancy <- randomForest(Life.expectancy ~ ., data = expectancy.train, mtry = 18, importance = TRUE, ntree=1000)
#running a random forest with 1000 trees, mtry is 18 as 19 variables in expectancy but 1 is life.expectancy
bagging.expectancy 
#looking at these 
pred.bagging <- predict(bagging.expectancy, newdata = expectancy.test)
#predicting bagging
rmse <- sqrt(mean((pred.bagging  - expectancy.test$Life.expectancy)^2)) 
#root mean squared error
importance(bagging.expectancy)
#looking at the importance of variables  
varImpPlot(bagging.expectancy)
#as above
  
#****doing random forests****
test.error <- rep(0, 18)
#creating a vector of 0s
for(i in 1:18){
rf0.expectancy <- randomForest(Life.expectancy ~ ., data = expectancy.train, mtry = i, importance = TRUE, ntree=1000)
pred.rf0 <- predict(rf0.expectancy, newdata = expectancy.test)
test.error[i] <- mean((pred.rf0  - expectancy.test$Life.expectancy)^2) # test MSE  
}
#for loop to try out different values of mtry then keeping the MSE in the vector
test.error
#looking at test error vecor
min(test.error)
#min value
rf9.expectancy <- randomForest(Life.expectancy ~ ., data = expectancy.train, mtry = 9, importance = TRUE, ntree=1000)
#running a random forest with 9
pred.rf9 <- predict(rf9.expectancy, newdata = expectancy.test)
#predicting this
test.error9 <- mean((pred.rf9  - expectancy.test$Life.expectancy)^2)
#getting the test error
RMSE <- sqrt(test.error9)
#taking the square root
RMSE
#producing lowest test.error
importance(rf9.expectancy) 
#looking at the important variables
varImpPlot(rf9.expectancy)
#as above

#****doing boosting****
lambda <- 10^seq(from=-5,to=-0.5,by=0.05) 
#creating a vector of lambda values
train.error <- rep(-1, length(lambda))
#creating a training error vector
for (i in 1:length(lambda)) {
boosting.expectancy <- gbm(Life.expectancy ~ ., data = expectancy.train, distribution = "gaussian", n.trees = 1000, shrinkage = lambda[i])
pred.expectancy.test <- predict(boosting.expectancy, expectancy.test, n.trees = 1000)
train.error[i] <- mean((pred.expectancy.test - expectancy.test$Life.expectancy)^2)
}
#for loop to vary lambda shrinkage value to find mean error
bestlam.boosting <- lambda[which.min(train.error)] 
#finding the best lambda  
min(train.error)
#reporting the min error
boosting.expectancy <- gbm(Life.expectancy ~ ., data = expectancy.train, distribution = "gaussian", n.trees = 1000, shrinkage = bestlam.boosting)
#running the optimal model on test set
summary(boosting.expectancy)
#summary of the boosting
pred.expectancy.test <- predict(boosting.expectancy, expectancy.test, n.trees = 1000)
#predicting the model
test.error <- sqrt(mean((pred.expectancy.test - expectancy.test$Life.expectancy)^2))
#gettig the rmse 
test.error
#reporting this
