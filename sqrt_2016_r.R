library(glmnet)
library(faraway)
library(leaps)
library(tidyverse)

hc_df<-read.csv('hate_crimes_full_v2.csv', row.names = 'state_abbrev')

#hc_df<-hc_df[!apply(hc_df['state_full'] == "", 1, all),]
#colnames(hc_df)

hc_df$confederate<-factor(hc_df$confederate)
hc_df$permit<-factor(hc_df$permit)
hc_df$universl<-factor(hc_df$universl)

hc_df$pk_per100k <- hc_df$pk_percap*100000

#hc_df$univerl['DC'] <- 'Yes'
#hc_df$permit['DC'] <- 'Yes'

fbi_df_2016<-hc_df[ , !(names(hc_df) %in% c('fbi_2019_per100k', 'hate_crimes_per_100k_splc', 'state_full', 'FIP', 'Year', 
                                      'hate_group_count_2019', 'FIP	Year',	'HFR_se',	'BRFSS',	'GALLUP',	'GSS',	'PEW',	'HuntLic',	'GunsAmmo',	
                                      'BackChk',	'PewQChng',	'BS1',	'BS2',	'BS3', 'pk_count', 'Number.of.participating.agencies',
                                      'Agencies.submitting.incident.reports', 'pop_covered', 'population', 'incidents', 'pk_percap'))]


fbi_df_2019<-hc_df[ , !(names(hc_df) %in% c('avg_hatecrimes_per_100k_fbi', 'hate_crimes_per_100k_splc', 'state_full', 'FIP', 'Year', 
                                      'hate_group_count_2019', 'FIP	Year',	'HFR_se',	'BRFSS',	'GALLUP',	'GSS',	'PEW',	'HuntLic',	'GunsAmmo',	
                                      'BackChk',	'PewQChng',	'BS1',	'BS2',	'BS3', 'pk_count', 'Number.of.participating.agencies',
                                      'Agencies.submitting.incident.reports', 'pop_covered', 'population', 'incidents', 'pk_percap'))]


splc_df_2016<-hc_df[ , !(names(hc_df) %in% c('avg_hatecrimes_per_100k_fbi', 'fbi_2019_per100k', 'state_full', 'FIP', 'Year', 
                                           'FIP	Year',	'HFR_se',	'BRFSS',	'GALLUP',	'GSS',	'PEW',	'HuntLic',	'GunsAmmo',	
                                           'BackChk',	'PewQChng',	'BS1',	'BS2',	'BS3', 'pk_count', 'Number.of.participating.agencies',
                                           'Agencies.submitting.incident.reports', 'pop_covered', 'population', 'incidents', 'pk_percap'))]


fbi_df_2016<-rename(fbi_df_2016, hc_per100k=avg_hatecrimes_per_100k_fbi)
fbi_df_2019<-rename(fbi_df_2019, hc_per100k=fbi_2019_per100k)
splc_df_2016<-rename(splc_df_2016, hc_per100k=hate_crimes_per_100k_splc)

fbi_df_2016<-fbi_df_2016 %>% drop_na(hc_per100k)
fbi_df_2019<-fbi_df_2019 %>% drop_na(hc_per100k)
splc_df_2016<-splc_df_2016 %>% drop_na(hc_per100k)


fbi_df_2016$hc_per100k<-sqrt(fbi_df_2016$hc_per100k)
fbi_df_2019$hc_per100k<-sqrt(fbi_df_2019$hc_per100k)
splc_df_2016$hc_per100k<-sqrt(splc_df_2016$hc_per100k)


par(mfrow=c(3,3))
boxplot(fbi_df_2016$hc_per100k~fbi_df_2016$confederate, main = '2016 FBI')
boxplot(fbi_df_2019$hc_per100k~fbi_df_2019$confederate, main = '2019 FBI')
boxplot(splc_df_2016$hc_per100k~splc_df_2016$confederate, main = '2016 SPLC')

boxplot(fbi_df_2016$hc_per100k~fbi_df_2016$permit, main = '2016 FBI')
boxplot(fbi_df_2019$hc_per100k~fbi_df_2019$permit, main = '2019 FBI')
boxplot(splc_df_2016$hc_per100k~splc_df_2016$permit, main = '2016 SPLC')

boxplot(fbi_df_2016$hc_per100k~fbi_df_2016$universl, main = '2016 FBI')
boxplot(fbi_df_2019$hc_per100k~fbi_df_2019$universl, main = '2019 FBI')
boxplot(splc_df_2016$hc_per100k~splc_df_2016$universl, main = '2016 SPLC')


pairs(fbi_df_2016, lower.panel=NULL)
pairs(fbi_df_2019, lower.panel=NULL)
pairs(splc_df_2016, lower.panel=NULL)



#cor(x)


model_2016<-lm(hc_per100k~., data = fbi_df_2016)
model_2019<-lm(hc_per100k~., data = fbi_df_2019)
model_splc<-lm(hc_per100k~., data = splc_df_2016)


vif(model_2016) # HFR is highly correlated (18+ VIF)
# Gun ownership is so highly correlated with suicide rates. We shouldn't use suicide rates


vif(model_2019) # HFR is highly correlated (18+ VIF)
# Gun ownership is so highly correlated with suicide rates. We shouldn't use suicide rates


vif(model_splc) # HFR is highly correlated (18+ VIF)
# Gun ownership is so highly correlated with suicide rates. We shouldn't use suicide rates


# remove correlated variables


splc_df_2016<-splc_df_2016[ , !(names(splc_df_2016) %in% c('Fem_FS_S', 'Male_FS_S', 'share_population_in_metro_areas', 'HFR', 'share_population_with_high_school_degree', 'hate_group_count_2019'))] 
fbi_df_2016<-fbi_df_2016[ , !(names(fbi_df_2016) %in% c('Fem_FS_S', 'Male_FS_S', 'share_population_in_metro_areas', 'HFR', 'share_population_with_high_school_degree'))] 
fbi_df_2019<-fbi_df_2019[ , !(names(fbi_df_2019) %in% c('Fem_FS_S', 'Male_FS_S', 'share_population_in_metro_areas', 'HFR', 'share_population_with_high_school_degree'))] 



model_2016<-lm(hc_per100k~., data = fbi_df_2016)
model_2019<-lm(hc_per100k~., data = fbi_df_2019)
model_splc<-lm(hc_per100k~., data = splc_df_2016)



vif(model_2016) # HFR is highly correlated (18+ VIF)
# Gun ownership is so highly correlated with suicide rates. We shouldn't use suicide rates


vif(model_2019) # HFR is highly correlated (18+ VIF)
# Gun ownership is so highly correlated with suicide rates. We shouldn't use suicide rates


vif(model_splc) # HFR is highly correlated (18+ VIF)
# Gun ownership is so highly correlated with suicide rates. We shouldn't use suicide rates


# remove HFR and police killings
#splc_df_2016<-splc_df_2016[ , !(names(splc_df_2016) %in% c('HFR'))] 
#fbi_df_2016<-fbi_df_2016[ , !(names(fbi_df_2016) %in% c('HFR'))] 
#fbi_df_2019<-fbi_df_2019[ , !(names(fbi_df_2019) %in% c('HFR'))] 


#model_2016<-lm(hc_per100k~., data = fbi_df_2016)
#model_2019<-lm(hc_per100k~., data = fbi_df_2019)
#model_splc<-lm(hc_per100k~., data = splc_df_2016)


#vif(model_2016) # HFR is highly correlated (18+ VIF)
# Gun ownership is so highly correlated with suicide rates. We shouldn't use suicide rates


#vif(model_2019) # HFR is highly correlated (18+ VIF)
# Gun ownership is so highly correlated with suicide rates. We shouldn't use suicide rates


#vif(model_splc) # HFR is highly correlated (18+ VIF)
# Gun ownership is so highly correlated with suicide rates. We shouldn't use suicide rates


summary(model_2016)
summary(model_2019)
summary(model_splc)

pairs(fbi_df_2016, lower.panel=NULL, main = 'FBI 2016')
pairs(fbi_df_2019, lower.panel=NULL, main = 'FBI 2019')
pairs(splc_df_2016, lower.panel=NULL, main = 'SPLC 2016')

# clear outlier in hate crimes for DC


##residuals
#res_2016<-model_2016$residuals

res_2016 <- unname(model_2016[['residuals']])
res_2016

##studentized residuals
student.res<-rstandard(model_2016) 

##externally studentized residuals
ext.student.res<-rstudent(model_2016) 

par(mfrow=c(1,3))
plot(model_2016$fitted.values,res_2016,main="Residuals")
plot(model_2016$fitted.values,student.res,main="Studentized Residuals")
plot(model_2016$fitted.values,ext.student.res,main="Externally  Studentized Residuals")

n<-length(fbi_df_2016$hc_per100k)
p<-length(model_2016$coefficients)

##critical value using Bonferroni procedure
qt(1-0.05/(2*n), n-p-1)

sort(ext.student.res)

plot(ext.student.res,main="Externally Studentized Residuals", ylim=c(-4,4))
abline(h=qt(1-0.05/(2*n), n-p-1), col="red")
abline(h=-qt(1-0.05/(2*n), n-p-1), col="red")

ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)]

##leverages
lev<-lm.influence(model_2016)$hat 

sort(lev)
2*p/n

plot(lev, main="Leverages", ylim=c(0,0.4))
abline(h=2*p/n, col="red")

##identify data points on plot
#identify(lev)

lev[lev>2*p/n]

##influential observations
DFFITS<-dffits(model_2016)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]

DFBETAS<-dfbetas(model_2016)
DFBETAS[abs(DFBETAS)>2/sqrt(n)]

COOKS<-cooks.distance(model_2016)
COOKS[COOKS>qf(0.5,p,n-p)]


# DC is a leverage point


x_2016<-model.matrix(hc_per100k~., fbi_df_2016)[,-1] # remove the first column of 1s representing the intercept
y_2016<-fbi_df_2016$hc_per100k
x_2016


x_2019<-model.matrix(hc_per100k~., fbi_df_2019)[,-1] # remove the first column of 1s representing the intercept
y_2019<-fbi_df_2019$hc_per100k


x_splc<-model.matrix(hc_per100k~., splc_df_2016)[,-1] # remove the first column of 1s representing the intercept
y_splc<-splc_df_2016$hc_per100k



###### shrinkage

##alpha=0 for ridge, alpha=1 for LASSO
##threshold value should be very small if multicollinearity is present. see what happens if thresh was set to a larger value
##we know theoretically the coeffs should be the same as lm when lambda is 0
lasso.r<-glmnet(x_2016,y_2016,alpha=1, lambda=0, thresh = 1e-14)
coefficients(lasso.r)

##MLR - produce the same thing as above **as long as thresh is small enough
result_2016<-lm(hc_per100k~.,fbi_df_2016)
summary(result_2016)

##split data
set.seed(12)
train<-sample(1:nrow(x_2016), nrow(x_2016)/2)
test<-(-train)
y.test<-y_2016[test]

##use CV to find optimal lambda based on training set
set.seed(12)
cv.out<-cv.glmnet(x_2016[train,],y_2016[train],alpha=1) # lasso regression
bestlam<-cv.out$lambda.min # value of lambda that minimizes MSE (the optimal value)
bestlam
plot(cv.out)

##fit lasso regression using training data
lasso.mod<-glmnet(x_2016[train,],y_2016[train],alpha=1,lambda=bestlam, thresh = 1e-14)

##test MSE with lambda=1
lasso.pred.0<-predict(lasso.mod,newx=x_2016[test,])
mean((lasso.pred.0-y.test)^2)


set.seed(12)
cv.out<-cv.glmnet(x_2016[train,],y_2016[train],alpha=1) # lasso regression
bestlam2<-cv.out$lambda.min # value of lambda that minimizes MSE (the optimal value)
bestlam2
plot(cv.out)

##fit ridge regression using training data
ridge.mod<-glmnet(x_2016[train,],y_2016[train],alpha=0,lambda=bestlam2, thresh = 1e-14)

coefficients(ridge.mod)

##test MSE with lambda=1
lasso.pred.0<-predict(lasso.mod,newx=x_2016[test,])
mean((lasso.pred.0-y.test)^2)



##Compare ridge with OLS using best lambda and all observations
out.lasso<-glmnet(x_2016,y_2016,alpha=1,lambda=bestlam,thresh = 1e-14)
out.ridge<-glmnet(x_2016,y_2016,alpha=0,lambda=bestlam2,thresh = 1e-14)
out.ols<-glmnet(x_2016,y_2016,alpha=0, lambda=0, thresh = 1e-14)
cbind(coefficients(out.lasso), coefficients(out.ridge), coefficients(out.ols))

lasso_mod<-lm(hc_per100k~median_household_income + gini_index + 
                share_voters_voted_trump + confederate
              , fbi_df_2016)

summary(lasso_mod)

#########################################

##perform all possible regressions (1st order)
allreg <- regsubsets(hc_per100k ~., data=fbi_df_2016, nbest=9) # sqrt

##create a "data frame" that stores the predictors in the various models considered as well as their various criteria
best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(hc_df)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic
best

##sort by various criteria
best[order(best$r2),]
best[order(best$adjr2, decreasing = TRUE),]

best[order(best$mse),]
best[order(best$cp),]
best[order(best$bic),]

best_r2 <- lm(hc_per100k~share_non_citizen + gini_index + share_non_white + share_voters_voted_trump + confederate + elasticity + universl
              + hate_group_count_2016, data = fbi_df_2016)

summary(best_r2)

##intercept only model
regnull <- lm(hc_per100k~1, data=fbi_df_2016)
##model with all predictors
regfull <- lm(hc_per100k~., data=fbi_df_2016)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")


for_model <- lm(formula = hc_per100k ~ share_voters_voted_trump + share_non_white + 
                  gini_index + confederate + universl + elasticity + share_non_citizen + 
                  hate_group_count_2016, data = fbi_df_2016)

back_model <- lm(formula = hc_per100k ~ share_non_citizen + gini_index + share_non_white + 
                   share_voters_voted_trump + confederate + elasticity + universl + 
                   hate_group_count_2016, data = fbi_df_2016)

both_model <- lm(formula = hc_per100k ~ share_voters_voted_trump + share_non_white + 
                   gini_index + confederate + universl + elasticity + share_non_citizen + 
                   hate_group_count_2016, data = fbi_df_2016)

summary(for_model)
summary(back_model)
summary(both_model)




plot(back_model$fitted.values,back_model$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

library(MASS)
boxcox(back_model, lambda = seq(-1, 3, 1/10))

##acf plot of residuals
acf(back_model$residuals)



anova(back_model)


n <- nrow(fbi_df_2016) # number of rows
sum_ressqr <- 0 # start with MSE = 0

for (i in 1:n) # loop through i = 1 to 28
{

  testrow <- fbi_df_2016[c(i), ] # test DF is just the ith row
  train_data <- fbi_df_2016[-c(i),] # training data is a DF with every row except the ith
  
  #back_model # fit a model to the training data
  preds<-predict(back_model,testrow, type="response") # predict the response for the ith row
  
  sum_ressqr <- sum_ressqr + (preds - testrow$hc_per100k)^2 # add the res^2 to the cumulative sum 
  print(preds - sqrt(testrow$hc_per100k))
  
}

print(sum_ressqr/n) # avg MSE for the LOOCV


# test the above solution

library(boot)

glm.fit<-glm(hc_per100k ~ share_non_citizen + gini_index + share_non_white + 
               share_voters_voted_trump + confederate + elasticity + universl + 
               hate_group_count_2016, data=fbi_df_2016)
cv.err<-cv.glm(data, glm.fit)
cv.err$delta[1] ##the output for the LOOCV should match your own
cv.glm(data,glm.fit, K=10)$delta[1] ##k fold CV with k=10


##perform levene's test. Null states the variances are equal for all classes. 
library(lawstat)
levene.test(fbi_df_2016$sqrt_per100k,fbi_df_2016$confederate) # variances aren't equal
#levene.test(hc_df$fbi_2019_per100k,hc_df$permit) # variances are equal
#levene.test(hc_df$fbi_2019_per100k,hc_df$universl) # variances are equal


##perform Tukey's multiple comparisons
library(multcomp)
pairwise<-glht(for_model, linfct = mcp(confederate= "Tukey"))
summary(pairwise)

##perform Tukey's multiple comparisons
library(multcomp)
pairwise<-glht(for_model, linfct = mcp(universl= "Tukey"))
summary(pairwise)

model_2<-lm(hc_per100k~share_non_white + gini_index + elasticity + share_non_citizen
            + share_voters_voted_trump + confederate + universl + hate_group_count_2016, data = fbi_df_2016)

model_red<-lm(hc_per100k~share_non_white + gini_index + elasticity + share_non_citizen
              , data = fbi_df_2016)

anova(model_2, model_red)

# should keep some variables in. Keep checking

model_red3<-lm(hc_per100k~share_non_white + gini_index + elasticity + share_non_citizen
               + share_voters_voted_trump, data = fbi_df_2016)

anova(model_2, model_red2)

model_red3<-lm(hc_per100k~share_non_white + gini_index + elasticity + share_non_citizen
               + share_voters_voted_trump + confederate, data = fbi_df_2016)

anova(model_2, model_red3)

model_red4<-lm(hc_per100k~share_non_white + gini_index + elasticity + share_non_citizen
               + share_voters_voted_trump + confederate + universl, data = fbi_df_2016)

anova(model_2, model_red4)

model_red5<-lm(hc_per100k~share_non_white + gini_index + elasticity + share_non_citizen
               + share_voters_voted_trump + confederate + hate_group_count_2016, data = fbi_df_2016)

anova(model_2, model_red5)


# red 3 is the best one...


summary(model_red3)

# but the model isn't great.


summary(model_2)
boxcox(back_model)


#LOOCV with 2016 data

n <- nrow(fbi_df_2016) # number of rows
sum_ressqr <- 0 # start with MSE = 0

for (i in 1:n) # loop through i = 1 to 28
{
  
  testrow <- fbi_df_2016[c(i), ] # test DF is just the ith row
  train_data <- fbi_df_2016[-c(i),] # training data is a DF with every row except the ith
  
  #back_model # fit a model to the training data
  preds<-predict(back_model,testrow, type="response") # predict the response for the ith row
  
  sum_ressqr <- sum_ressqr + (preds - testrow$hc_per100k)^2 # add the res^2 to the cumulative sum 
  print(preds - testrow$hc_per100k)
  
}

print(sum_ressqr/n) # avg MSE for the LOOCV




#LOOCV with 2019 data

n <- nrow(fbi_df_2019) # number of rows
sum_ressqr <- 0 # start with MSE = 0

for (i in 1:n) # loop through i = 1 to 28
{
  
  testrow <- fbi_df_2019[c(i), ] # test DF is just the ith row
  train_data <- fbi_df_2019[-c(i),] # training data is a DF with every row except the ith
  
  #back_model # fit a model to the training data
  preds<-predict(back_model,testrow, type="response") # predict the response for the ith row
  
  sum_ressqr <- sum_ressqr + (preds - testrow$hc_per100k)^2 # add the res^2 to the cumulative sum 
  print(preds - testrow$hc_per100k)
  
}

print(sum_ressqr/n) # avg MSE for the LOOCV

#LOOCV with SPLC data (doesn't really fit since these data are too different)

n <- nrow(splc_df_2016) # number of rows
sum_ressqr <- 0 # start with MSE = 0

for (i in 1:n) # loop through i = 1 to 28
{
  
  testrow <- splc_df_2016[c(i), ] # test DF is just the ith row
  train_data <- splc_df_2016[-c(i),] # training data is a DF with every row except the ith
  
  #back_model # fit a model to the training data
  preds<-predict(back_model,testrow, type="response") # predict the response for the ith row
  
  sum_ressqr <- sum_ressqr + (preds - testrow$hc_per100k)^2 # add the res^2 to the cumulative sum 
  print(preds - (testrow$hc_per100k))
  
}

print(sum_ressqr/n) # avg MSE for the LOOCV


model_2019 <- lm(formula = hc_per100k ~ share_voters_voted_trump + share_non_white + 
                   gini_index + confederate + universl + elasticity + share_non_citizen + 
                   hate_group_count_2016, data = fbi_df_2019)
summary(model_2019)



regnull <- lm(hc_per100k~1, data=fbi_df_2019)
##model with all predictors
regfull <- lm(hc_per100k~., data=fbi_df_2019)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

back_2019<-lm(formula = hc_per100k ~ gini_index + share_voters_voted_trump + 
             confederate + permit + hate_group_count_2016 + pk_per100k, 
           data = fbi_df_2019)




n <- nrow(fbi_df_2019) # number of rows
sum_ressqr <- 0 # start with MSE = 0

for (i in 1:n) # loop through i = 1 to 28
{
  
  testrow <- fbi_df_2019[c(i), ] # test DF is just the ith row
  train_data <- fbi_df_2019[-c(i),] # training data is a DF with every row except the ith
  
  #back_model # fit a model to the training data
  preds<-predict(back_2019,testrow, type="response") # predict the response for the ith row
  
  sum_ressqr <- sum_ressqr + (preds - testrow$hc_per100k)^2 # add the res^2 to the cumulative sum 
  print(preds - testrow$hc_per100k)
  
}

print(sum_ressqr/n) # avg MSE for the LOOCV




##residual plot of model with no interaction
plot(back_model$fitted.values,back_model$residuals,main="Residual plot")
abline(h=0,col="red")

##ACF plot of residuals
acf(back_model$residuals)

##QQ plot of residuals
qqnorm(back_model$residuals)
qqline(back_model$residuals, col="red")


##perform levene's test. Null states the variances are equal for all classes. 
library(lawstat)
levene.test(fbi_df_2016$hc_per100k,fbi_df_2016$confederate)

summary(reduced)

##perform Tukey's multiple comparisons
library(multcomp)
pairwise<-glht(back_model, linfct = mcp(confederate= "Tukey"))
summary(pairwise)

reduced$coef

##obtain the variance-covariance matrix of the coefficients
vcov(reduced)





###### Interaction analysis

# consider each cut as a subset
uni_y<-subset(hc_df,universl=="Yes") 
uni_n<-subset(hc_df,universl=="No")



# fit separate regressions
gini_yes <- lm(fbi_2019_per100k~gini_index,data=uni_y)
gini_no <- lm(fbi_2019_per100k~gini_index,data=uni_n)

# Create scatters:

plot(hc_df$gini_index, hc_df$fbi_2019_per100k, main="log Price by log Carat and Cut")
points(uni_y$gini_index, uni_y$fbi_2019_per100k, pch=2, col="blue")
points(uni_n$gini_index, uni_n$fbi_2019_per100k, pch=3, col="red")


abline(gini_yes,lty=1, col="blue")
abline(gini_no,lty=2, col="red") 
#abline(price_VG,lty=3, col="red")

