library(glmnet)
library(faraway)
library(leaps)
library(tidyverse)

hc_df<-read.csv('hate_crimes_full_v2.csv', row.names = 'state_abbrev') # read in data from the cleansing file


# We don't really want to use the SPLC data since those data were collected for some period directly after the 2016 election. SPLC also collects HC data in a very specific
# manner vastly different than how the FBI reports


# ensure cat. variables are factors
hc_df$confederate<-factor(hc_df$confederate) 
hc_df$permit<-factor(hc_df$permit)
hc_df$universl<-factor(hc_df$universl)


# make police killings per 100k pop
hc_df$pk_per100k <- hc_df$pk_percap*100000 


# remove some variables from the hc_df dataframe
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

colnames(fbi_df_2016)

# rename hate crime columns to standardize
fbi_df_2016<-rename(fbi_df_2016, hc_per100k=avg_hatecrimes_per_100k_fbi)
fbi_df_2019<-rename(fbi_df_2019, hc_per100k=fbi_2019_per100k)
splc_df_2016<-rename(splc_df_2016, hc_per100k=hate_crimes_per_100k_splc)


# drop NAs from HC values
fbi_df_2016<-fbi_df_2016 %>% drop_na(hc_per100k)
fbi_df_2019<-fbi_df_2019 %>% drop_na(hc_per100k)
splc_df_2016<-splc_df_2016 %>% drop_na(hc_per100k)


# create box plots of categorical data. Notice difference in shape of confederate data -- confederate states had fewer HCs 
# remaining two variables do not show the same difference

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


# not much to see in the scatter matrix. DC sticks out a lot as far as hate crimes, and we notice some heavy correlation between variables. Will want to 
# explore DC during outlier analysis


pairs(fbi_df_2016, lower.panel=NULL)
pairs(fbi_df_2019, lower.panel=NULL)
pairs(splc_df_2016, lower.panel=NULL)


# fit the full data -- not helpful except to look at correlation/multicollinearity

model_2016<-lm(hc_per100k~., data = fbi_df_2016)
model_2019<-lm(hc_per100k~., data = fbi_df_2019)
model_splc<-lm(hc_per100k~., data = splc_df_2016)


# run VIF. notice high values (like HFR, suicide rates, share_population_in_metro_areas). We will remove these

vif(model_2016) # HFR is highly correlated (18+ VIF)

vif(model_2019) # HFR is highly correlated (18+ VIF)

vif(model_splc) # HFR is highly correlated (18+ VIF)

# remove correlated variables

splc_df_2016<-splc_df_2016[ , !(names(splc_df_2016) %in% c('median_household_income', 'Fem_FS_S', 'share_population_in_metro_areas', 'HFR', 'share_population_with_high_school_degree', 'hate_group_count_2019'))] 
fbi_df_2016<-fbi_df_2016[ , !(names(fbi_df_2016) %in% c('share_non_citizen', 'median_household_income', 'Fem_FS_S', 'Male_FS_S', 'share_population_in_metro_areas', 'HFR', 'share_population_with_high_school_degree'))] 
fbi_df_2019<-fbi_df_2019[ , !(names(fbi_df_2019) %in% c('Fem_FS_S', 'Male_FS_S', 'share_population_in_metro_areas', 'HFR', 'share_population_with_high_school_degree'))] 

# fit models again and run VIF

model_2016<-lm(hc_per100k~., data = fbi_df_2016)
model_2019<-lm(hc_per100k~., data = fbi_df_2019)
model_splc<-lm(hc_per100k~., data = splc_df_2016)

vif(model_2016) # HFR is highly correlated (18+ VIF)

vif(model_2019) # HFR is highly correlated (18+ VIF)

vif(model_splc) # HFR is highly correlated (18+ VIF)

# check model summaries

summary(model_2016)
summary(model_2019)
summary(model_splc)


# produce scatter matrices again

pairs(fbi_df_2016, lower.panel=NULL, main = 'FBI 2016')
pairs(fbi_df_2019, lower.panel=NULL, main = 'FBI 2019')
pairs(splc_df_2016, lower.panel=NULL, main = 'SPLC 2016')


# model selection #


##intercept only model
regnull <- lm(hc_per100k~1, data=fbi_df_2016)
##model with all predictors
regfull <- lm(hc_per100k~., data=fbi_df_2016)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")


for_model <- lm(formula = hc_per100k ~ share_voters_voted_trump + gini_index + 
                  share_non_white + confederate + elasticity + universl, data = fbi_df_2016)

back_model <- lm(formula = hc_per100k ~ gini_index + share_non_white + share_voters_voted_trump + 
                   confederate + elasticity + universl, data = fbi_df_2016)

both_model <- lm(formula = hc_per100k ~ share_voters_voted_trump + gini_index + 
                   share_non_white + confederate + elasticity + universl, data = fbi_df_2016)

summary(for_model)
summary(back_model)
summary(both_model)

# all produce the same models Relatively high adj R^2 and significant t-vals

plot(both_model$fitted.values,both_model$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

library(MASS)
boxcox(both_model, lambda = seq(-1, 3, 1/10))

##acf plot of residuals
acf(both_model$residuals)


# too good to be true -- need to transform the data. Try a sqrt transform?

sqrt_model <- lm(formula = sqrt(hc_per100k) ~ share_voters_voted_trump + gini_index + 
                   share_non_white + confederate + elasticity + universl, data = fbi_df_2016)


summary(sqrt_model)

library(MASS)
boxcox(sqrt_model, lambda = seq(-1, 3, 1/10))

# not too bad, but we might want to rerun the model selection with the newly-transformed data


# rerun model selection #


# transform variable

fbi_df_2016$hc_per100k<-sqrt(fbi_df_2016$hc_per100k)

# rerun code from above

# no need to rerun correlation analysis since the transformation doesn't change that analysis

pairs(fbi_df_2016, lower.panel=NULL, main = 'FBI 2016')


##intercept only model
regnull <- lm(hc_per100k~1, data=fbi_df_2016)
##model with all predictors
regfull <- lm(hc_per100k~., data=fbi_df_2016)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")


for_model <- lm(formula = hc_per100k ~ share_voters_voted_trump + share_non_white + 
                  gini_index + confederate + universl + elasticity, data = fbi_df_2016)

back_model <- lm(formula = hc_per100k ~ gini_index + share_non_white + share_voters_voted_trump + 
                   confederate + elasticity + universl, data = fbi_df_2016)

both_model <- lm(formula = hc_per100k ~ share_voters_voted_trump + share_non_white + 
                   gini_index + confederate + universl + elasticity, data = fbi_df_2016)

summary(for_model)
summary(back_model)
summary(both_model)

# they're all the same! decent adj R^2

full_sqrt<-lm(hc_per100k~., data = fbi_df_2016)

vif(full_sqrt)


plot(back_model$fitted.values,back_model$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

library(MASS)
boxcox(back_model, lambda = seq(-1, 3, 1/10))

##acf plot of residuals
acf(back_model$residuals)


fin_vars<-c('hc_per100k', 'gini_index', 'gini_index', 
            'share_voters_voted_trump', 'confederate', 'universl', 'elasticity')


# correlation matrix with the subset of variables

pairs(fbi_df_2016[fin_vars], lower.panel=NULL, main = 'Final Variables')

fin_data<-fbi_df_2016[fin_vars]

model_full<-lm(hc_per100k~., data = fin_data)

summary(model_full)

red<-lm(hc_per100k~gini_index+gini_index+share_voters_voted_trump+confederate+universl, fin_data)

anova(red, model_full)

# Check to see if step processes produce similar models

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
#best

##sort by various criteria
best[order(best$r2,decreasing = TRUE),] # our full_model
best[order(best$adjr2, decreasing = TRUE),] # our full_model
best[order(best$mse),] # our full_model
best[order(best$cp),] # our full_model
best[order(best$bic),] # not the full model. confederate, and Gini index

bic_mod<-lm(formula = hc_per100k ~ gini_index + confederate, data = fbi_df_2016)

summary(bic_mod)


# outlier analysis #

# residuals

res_full <- unname(model_full[['residuals']]) # need to remove the field headers
#res_2016 # uncomment to see residuals
res_bic <- unname(bic_mod[['residuals']]) # need to remove the field headers
#res_2016 # uncomment to see residuals

# outlier, influential & leverage points analysis

# studentized residuals

student.res<-rstandard(model_full) 

# externally studentized residuals

ext.student.res<-rstudent(model_full) 

# plot residuals vs standardized residuals found above

par(mfrow=c(1,3))
plot(model_full$fitted.values,res_full,main="Residuals")
plot(model_full$fitted.values,student.res,main="Studentized Residuals")
plot(model_full$fitted.values,ext.student.res,main="Externally  Studentized Residuals")

# calc values

n<-length(fin_data$hc_per100k)
p<-length(model_full$coefficients)

##critical value using Bonferroni procedure
qt(1-0.05/(2*n), n-p-1)

sort(ext.student.res)

plot(ext.student.res,main="Externally Studentized Residuals", ylim=c(-4,4))
abline(h=qt(1-0.05/(2*n), n-p-1), col="red")
abline(h=-qt(1-0.05/(2*n), n-p-1), col="red")

ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)]


student.res<-rstandard(bic_mod) 

# externally studentized residuals

ext.student.res<-rstudent(bic_mod) 

# plot residuals vs standardized residuals found above

par(mfrow=c(1,3))
plot(bic_mod$fitted.values,res_bic,main="Residuals")
plot(bic_mod$fitted.values,student.res,main="Studentized Residuals")
plot(bic_mod$fitted.values,ext.student.res,main="Externally  Studentized Residuals")


# calc values

n<-length(fbi_df_2016$hc_per100k)
p<-length(model_full$coefficients)

##critical value using Bonferroni procedure
qt(1-0.05/(2*n), n-p-1)

sort(ext.student.res)

plot(ext.student.res,main="Externally Studentized Residuals", ylim=c(-4,4))
abline(h=qt(1-0.05/(2*n), n-p-1), col="red")
abline(h=-qt(1-0.05/(2*n), n-p-1), col="red")

ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)]

##leverages
lev_full<-lm.influence(model_full)$hat 
lev_bic<-lm.influence(bic_mod)$hat 

sort(lev_full)
sort(lev_bic)
2*p/n

plot(lev_full, main="Leverages", ylim=c(0,0.4))
abline(h=2*p/n, col="red")

plot(lev_bic, main="Leverages", ylim=c(0,0.4))
abline(h=2*p/n, col="red")

# get leverage points

lev_full[lev_full>2*p/n]
lev_bic[lev_bic>2*p/n]

# DC very leveraged in BIC
# DC and Cali are leverage points in full

# influential observations
DFFITS<-dffits(model_full)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]

DFBETAS<-dfbetas(model_full)
DFBETAS[abs(DFBETAS)>2/sqrt(n)]

COOKS<-cooks.distance(model_full)
COOKS[COOKS>qf(0.5,p,n-p)]



# influential observations for BIC
DFFITS<-dffits(bic_mod)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]

DFBETAS<-dfbetas(bic_mod)
DFBETAS[abs(DFBETAS)>2/sqrt(n)]

COOKS<-cooks.distance(bic_mod)
COOKS[COOKS>qf(0.5,p,n-p)]


# check shrinkage models to see if they align with model_full and have decent predictive power

# begin shrinkage analysis

x_2016<-model.matrix(hc_per100k~., fbi_df_2016)[,-1] # remove the first column of 1s representing the intercept
y_2016<-fbi_df_2016$hc_per100k



###### Lasso

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

##fit lasso regression using training data
lasso.mod.1<-glmnet(x_2016[train,],y_2016[train],alpha=1,lambda=1, thresh = 1e-14)

##test MSE with lambda=1
lasso.pred.1<-predict(lasso.mod.1,newx=x_2016[test,])
mean((lasso.pred.1-y.test)^2)

# perform ridge

##alpha=0 for ridge, alpha=1 for LASSO
##threshold value should be very small if multicollinearity is present. see what happens if thresh was set to a larger value
##we know theoretically the coeffs should be the same as lm when lambda is 0
ridge.r<-glmnet(x_2016,y_2016,alpha=0, lambda=0, thresh = 1e-14)
coefficients(ridge.r)

##MLR - produce the same thing as above **as long as thresh is small enough
summary(result_2016)

##split data
set.seed(12)
train<-sample(1:nrow(x_2016), nrow(x_2016)/2)
test<-(-train)
y.test<-y_2016[test]

##use CV to find optimal lambda based on training set
set.seed(12)
cv.out.ridge<-cv.glmnet(x_2016[train,],y_2016[train],alpha=0) # ridge regression
bestlam.r<-cv.out.ridge$lambda.min # value of lambda that minimizes MSE (the optimal value)
bestlam.r
plot(cv.out.ridge)

##fit ridge regression using training data
ridge.mod<-glmnet(x_2016[train,],y_2016[train],alpha=0,lambda=bestlam.r, thresh = 1e-14)

##test MSE with lambda=1
ridge.pred.0<-predict(ridge.mod,newx=x_2016[test,])
mean((ridge.pred.0-y.test)^2)

##fit ridge regression using training data
ridge.mod.1<-glmnet(x_2016[train,],y_2016[train],alpha=0,lambda=1, thresh = 1e-14)

##test MSE with lambda=1
ridge.pred.1<-predict(ridge.mod.1,newx=x_2016[test,])
mean((ridge.pred.1-y.test)^2)



# lasso produced a slightly lower MSE


##Compare ridge with OLS using best lambda and all observations
out.lasso<-glmnet(x_2016,y_2016,alpha=1,lambda=bestlam,thresh = 1e-14)
out.ridge<-glmnet(x_2016,y_2016,alpha=0,lambda=bestlam.r,thresh = 1e-14)
out.ols<-glmnet(x_2016,y_2016,alpha=0, lambda=0, thresh = 1e-14)
cbind(coefficients(out.lasso), coefficients(out.ridge), coefficients(out.ols))




# check with smaller dataset #


x_2016<-model.matrix(hc_per100k~., fin_data)[,-1] # remove the first column of 1s representing the intercept
y_2016<-fin_data$hc_per100k



###### Lasso

##alpha=0 for ridge, alpha=1 for LASSO
##threshold value should be very small if multicollinearity is present. see what happens if thresh was set to a larger value
##we know theoretically the coeffs should be the same as lm when lambda is 0
lasso.r<-glmnet(x_2016,y_2016,alpha=1, lambda=0, thresh = 1e-14)
coefficients(lasso.r)

##MLR - produce the same thing as above **as long as thresh is small enough
result_2016<-lm(hc_per100k~.,fin_data)
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

##fit lasso regression using training data
lasso.mod.1<-glmnet(x_2016[train,],y_2016[train],alpha=1,lambda=1, thresh = 1e-14)

##test MSE with lambda=1
lasso.pred.1<-predict(lasso.mod.1,newx=x_2016[test,])
mean((lasso.pred.1-y.test)^2)

# perform ridge

##alpha=0 for ridge, alpha=1 for LASSO
##threshold value should be very small if multicollinearity is present. see what happens if thresh was set to a larger value
##we know theoretically the coeffs should be the same as lm when lambda is 0
ridge.r<-glmnet(x_2016,y_2016,alpha=0, lambda=0, thresh = 1e-14)
coefficients(ridge.r)

##MLR - produce the same thing as above **as long as thresh is small enough
summary(result_2016)

##split data
set.seed(12)
train<-sample(1:nrow(x_2016), nrow(x_2016)/2)
test<-(-train)
y.test<-y_2016[test]

##use CV to find optimal lambda based on training set
set.seed(12)
cv.out.ridge<-cv.glmnet(x_2016[train,],y_2016[train],alpha=0) # ridge regression
bestlam.r<-cv.out.ridge$lambda.min # value of lambda that minimizes MSE (the optimal value)
bestlam.r
plot(cv.out.ridge)

##fit ridge regression using training data
ridge.mod<-glmnet(x_2016[train,],y_2016[train],alpha=0,lambda=bestlam.r, thresh = 1e-14)

##test MSE with lambda=1
ridge.pred.0<-predict(ridge.mod,newx=x_2016[test,])
mean((ridge.pred.0-y.test)^2)

##fit ridge regression using training data
ridge.mod.1<-glmnet(x_2016[train,],y_2016[train],alpha=0,lambda=1, thresh = 1e-14)

##test MSE with lambda=1
ridge.pred.1<-predict(ridge.mod.1,newx=x_2016[test,])
mean((ridge.pred.1-y.test)^2)



# lasso produced a slightly lower MSE


##Compare ridge with OLS using best lambda and all observations
out.lasso<-glmnet(x_2016,y_2016,alpha=1,lambda=bestlam,thresh = 1e-14)
out.ridge<-glmnet(x_2016,y_2016,alpha=0,lambda=bestlam.r,thresh = 1e-14)
out.ols<-glmnet(x_2016,y_2016,alpha=0, lambda=0, thresh = 1e-14)
cbind(coefficients(out.lasso), coefficients(out.ridge), coefficients(out.ols))












summary(out.lasso)

# LOOCV

n <- nrow(fbi_df_2016) # number of rows (28)
sum_ressqr <- 0 # start with MSE = 0

for (i in 1:n) # loop through i = 1 to 28
{

  testrow <- fbi_df_2016[c(i), ] # test DF is just the ith row
  train_data <- fbi_df_2016[-c(i),] # training data is a DF with every row except the ith
  
  preds<-predict(model_full,testrow, type="response") # predict the response for the ith row
  
  sum_ressqr <- sum_ressqr + (preds - testrow$hc_per100k)^2 # add the res^2 to the cumulative sum 
  print(preds - testrow$hc_per100k)
  
}

print(sum_ressqr/n) # avg MSE for the LOOCV

sum_ressqr <- 0 # start with MSE = 0

for (i in 1:n) # loop through i = 1 to 28
{
  
  testrow <- fbi_df_2016[c(i), ] # test DF is just the ith row
  train_data <- fbi_df_2016[-c(i),] # training data is a DF with every row except the ith
  
  preds<-predict(bic_mod,testrow, type="response") # predict the response for the ith row
  
  sum_ressqr <- sum_ressqr + (preds - testrow$hc_per100k)^2 # add the res^2 to the cumulative sum 
  print(preds - testrow$hc_per100k)
  
}

print(sum_ressqr/n) # avg MSE for the LOOCV


# full_model does better by MSE


# test the above solution

library(boot)

glm.fit<-glm(hc_per100k ~ share_voters_voted_trump + share_non_white + 
               gini_index + confederate + universl + elasticity + 
               hate_group_count_2016, data = fbi_df_2016)
cv.err<-cv.glm(fbi_df_2016, glm.fit)
cv.err$delta[1] ##the output for the LOOCV should match your own
cv.glm(fbi_df_2016,glm.fit, K=10)$delta[1] ##k fold CV with k=10


##perform levene's test. Null states the variances are equal for all classes. 
library(lawstat)
levene.test(fbi_df_2016$hc_per100k,fbi_df_2016$confederate) # variances aren't equal
#levene.test(hc_df$fbi_2019_per100k,hc_df$permit) # variances are equal
#levene.test(hc_df$fbi_2019_per100k,hc_df$universl) # variances are equal


summary(model_full)

##perform Tukey's multiple comparisons
library(multcomp)
pairwise<-glht(model_full, linfct = mcp(confederate= "Tukey"))
summary(pairwise)


##perform levene's test. Null states the variances are equal for all classes. 
library(lawstat)
levene.test(fbi_df_2016$hc_per100k,fbi_df_2016$universl) # variances aren't equal
#levene.test(hc_df$fbi_2019_per100k,hc_df$permit) # variances are equal
#levene.test(hc_df$fbi_2019_per100k,hc_df$universl) # variances are equal


summary(model_full)

##perform Tukey's multiple comparisons
library(multcomp)
pairwise<-glht(model_full, linfct = mcp(universl= "Tukey"))
summary(pairwise)






###### Interaction analysis

# consider each cut as a subset
con_y<-subset(fbi_df_2016,confederate=="Yes") 
con_n<-subset(fbi_df_2016,confederate=="No")



# fit separate regressions
gini_yes <- lm(hc_per100k~gini_index,data=con_y)
gini_no <- lm(hc_per100k~gini_index,data=con_n)

# Create scatters:

plot(fbi_df_2016$gini_index, fbi_df_2016$hc_per100k, main="confederate")
points(con_y$gini_index, con_y$hc_per100k, pch=2, col="blue")
points(con_n$gini_index, con_n$hc_per100k, pch=3, col="red")


abline(gini_yes,lty=1, col="blue")
abline(gini_no,lty=2, col="red") 
#abline(price_VG,lty=3, col="red")

summary(model_full)

###############

# consider each cut as a subset
uni_y<-subset(fbi_df_2016,universl=="Yes") 
uni_n<-subset(fbi_df_2016,universl=="No")


# fit separate regressions
gini_yes <- lm(hc_per100k~share_non_white,data=uni_y)
gini_no <- lm(hc_per100k~share_non_white,data=uni_n)

# Create scatters:

plot(fbi_df_2016$share_non_white, fbi_df_2016$hc_per100k, main="universl")
points(uni_y$share_non_white, uni_y$hc_per100k, pch=2, col="blue")
points(uni_n$share_non_white, uni_n$hc_per100k, pch=3, col="red")


abline(gini_yes,lty=1, col="blue")
abline(gini_no,lty=2, col="red") 


inter_model<-lm(formula = hc_per100k ~ gini_index + share_voters_voted_trump + share_voters_voted_trump*universl
                                 +  share_non_white*confederate, data = fbi_df_2016)


all_inter<-lm(formula = hc_per100k ~ .^2, data = fin_data)

summary(all_inter)
summary(inter_model) # no instances of Yes and Yes for confederate and universl
summary(model_full)

mod_full_ex<-lm(hc_per100k~gini_index+share_non_white+share_voters_voted_trump+confederate+universl, data = fbi_df_2016)

summary(model_full_ex)

inter_red<-lm(formula = hc_per100k ~ gini_index + share_voters_voted_trump +
                 +  share_non_white*confederate, data = fbi_df_2016)


anova(inter_red, inter_model) # cannot remove some of the terms and use the inter_red model

summary(inter_model)


source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(fbi_df_2016, row.vars = "confederate", col.vars = "universl", type = "f")
##obtain the variance-covariance matrix of the coefficients
vcov(model_full)



n <- nrow(fbi_df_2019) # number of rows (28)
sum_ressqr <- 0 # start with MSE = 0

for (i in 1:n) # loop through i = 1 to 28
{
  
  testrow <- fbi_df_2016[c(i), ] # test DF is just the ith row
  train_data <- fbi_df_2016[-c(i),] # training data is a DF with every row except the ith
  
  preds<-predict(inter_model,testrow, type="response") # predict the response for the ith row
  
  sum_ressqr <- sum_ressqr + (preds - testrow$hc_per100k)^2 # add the res^2 to the cumulative sum 
  #print((preds - testrow$hc_per100k)/testrow$hc_per100k)
  print(preds - testrow$hc_per100k)
  print(preds)
  
}

print(sum_ressqr/n) # avg MSE for the LOOCV



plot(inter_model$fitted.values,inter_model$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

library(MASS)
boxcox(inter_model, lambda = seq(-1, 3, 1/10))

##acf plot of residuals
acf(inter_model$residuals)

summary(model_full)


