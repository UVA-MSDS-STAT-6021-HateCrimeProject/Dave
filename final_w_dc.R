library(glmnet)
library(faraway)
library(leaps)
library(tidyverse)

hc_df<-read.csv('hate_crimes_full_v2.csv', row.names = 'state_abbrev') # read in data from the cleansing file


# We don't really want to use the SPLC data since those data were collected for some period directly after the 2016 election. SPLC also collects HC data in a very specific
# manner vastly different than how the FBI reports

# ensure cat. variables are factors
hc_df$con_uni_combo<-factor(hc_df$con_uni_combo)

# make police killings per 100k pop
hc_df$pk_per100k <- hc_df$pk_percap*100000 


# remove some variables from the hc_df dataframe
fbi_df_2016<-hc_df[ , !(names(hc_df) %in% c('fbi_2019_per100k', 'hate_crimes_per_100k_splc', 'state_full', 'FIP', 'Year', 'confederate', 'universl',
                                            'hate_group_count_2019', 'FIP	Year',	'HFR_se',	'BRFSS',	'GALLUP',	'GSS',	'PEW',	'HuntLic',	'GunsAmmo',	
                                            'BackChk',	'PewQChng',	'BS1',	'BS2',	'BS3', 'pk_count', 'Number.of.participating.agencies',
                                            'Agencies.submitting.incident.reports', 'pop_covered', 'population', 'incidents'))]

fbi_df_2019<-hc_df[ , !(names(hc_df) %in% c('average_hatecrimes_per_100k_fbi', 'hate_crimes_per_100k_splc', 'state_full', 'FIP', 'Year', 'confederate', 'universl',
                                            'hate_group_count_2019', 'FIP	Year',	'HFR_se',	'BRFSS',	'GALLUP',	'GSS',	'PEW',	'HuntLic',	'GunsAmmo',	
                                            'BackChk',	'PewQChng',	'BS1',	'BS2',	'BS3', 'pk_count', 'Number.of.participating.agencies',
                                            'Agencies.submitting.incident.reports', 'pop_covered', 'population', 'incidents'))]

colnames(fbi_df_2016)

levels(fbi_df_2016$con_uni_combo)
fbi_df_2016$con_uni_combo<-relevel(fbi_df_2016$con_uni_combo, ref = "Neither")
levels(fbi_df_2016$con_uni_combo)

fbi_df_2019$con_uni_combo<-relevel(fbi_df_2019$con_uni_combo, ref = "Neither")
levels(fbi_df_2019$con_uni_combo)


# rename hate crime columns to standardize
fbi_df_2016<-rename(fbi_df_2016, hc_per100k=avg_hatecrimes_per_100k_fbi)

fbi_df_2019<-rename(fbi_df_2019, hc_per100k=fbi_2019_per100k)

# drop NAs from HC values
fbi_df_2016<-fbi_df_2016 %>% drop_na(hc_per100k)

# create box plots of categorical data. Notice difference in shape of confederate data -- confederate states had fewer HCs 
# remaining two variables do not show the same difference

par(mfrow=c(1,2))
boxplot(fbi_df_2016$hc_per100k~fbi_df_2016$con_uni_combo, main = '2016 FBI')

boxplot(fbi_df_2019$hc_per100k~fbi_df_2019$con_uni_combo, main = '2019 FBI')


# not much to see in the scatter matrix. DC sticks out a lot as far as hate crimes, and we notice some heavy correlation between variables. Will want to 
# explore DC during outlier analysis


pairs(fbi_df_2016, lower.panel=NULL)


# fit the full data -- not helpful except to look at correlation/multicollinearity

model_2016<-lm(hc_per100k~., data = fbi_df_2016)

# run VIF. notice high values (like HFR, suicide rates, share_population_in_metro_areas). We will remove these

vif(model_2016) # HFR is highly correlated (18+ VIF)

# remove correlated variables

fbi_df_2016<-fbi_df_2016[ , !(names(fbi_df_2016) %in% c('pk_per100k', 'share_non_citizen', 'median_household_income', 'Fem_FS_S', 'Male_FS_S', 'share_population_in_metro_areas', 'HFR', 'share_population_with_high_school_degree'))] 

# fit models again and run VIF

model_2016<-lm(hc_per100k~., data = fbi_df_2016)

vif(model_2016) # HFR is highly correlated (18+ VIF)

# check model summaries

summary(model_2016)

# produce scatter matrices again

pairs(fbi_df_2016, lower.panel=NULL, main = 'FBI 2016, post Multicollinearity Analysis')

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
                  con_uni_combo + share_non_white + elasticity, data = fbi_df_2016)

back_model <- lm(formula = hc_per100k ~ gini_index + share_non_white + share_voters_voted_trump + 
                   elasticity + con_uni_combo, data = fbi_df_2016)

both_model <- lm(formula = hc_per100k ~ share_voters_voted_trump + gini_index + 
                   con_uni_combo + share_non_white + elasticity, data = fbi_df_2016)

summary(for_model)
summary(back_model)
summary(both_model)

# all produce the same models Relatively high adj R^2 and significant t-vals

par(mfrow=c(1,3))

plot(both_model$fitted.values,both_model$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

library(MASS)
boxcox(both_model, lambda = seq(-2, 3, 1/10), main="Box Cox, Model 1")

##acf plot of residuals
acf(both_model$residuals)


# too good to be true -- need to transform the data. Try a sqrt transform?

sqrt_model <- lm(formula = sqrt(hc_per100k) ~ share_voters_voted_trump + gini_index + 
                   con_uni_combo + share_non_white + elasticity, data = fbi_df_2016)


summary(sqrt_model)

plot(sqrt_model$fitted.values,sqrt_model$residuals, main="Plot of Residuals against Fitted Values, post Transform")
abline(h=0,col="red")

library(MASS)
boxcox(sqrt_model, lambda = seq(-2, 4, 1/10), main="Box Cox, sqrt Y")

##acf plot of residuals
acf(sqrt_model$residuals)

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
                  gini_index + con_uni_combo + elasticity, data = fbi_df_2016)

back_model <- lm(formula = hc_per100k ~ gini_index + share_non_white + share_voters_voted_trump + 
                   elasticity + con_uni_combo, data = fbi_df_2016)

both_model <- lm(formula = hc_per100k ~ share_voters_voted_trump + share_non_white + 
                   gini_index + con_uni_combo + elasticity, data = fbi_df_2016)

summary(for_model)
summary(back_model)
summary(both_model)

# they're all the same! decent adj R^2. Also the same as the models found in part 1,
# just with transformed response

full_sqrt<-lm(hc_per100k~., data = fbi_df_2016)

vif(full_sqrt)

# no multicollinearity, which makes sense since the transformation shouldn't have changed that

par(mfrow=c(1,3))

plot(back_model$fitted.values,back_model$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

library(MASS)
boxcox(back_model, lambda = seq(-1, 4, 1/10))

##acf plot of residuals
acf(back_model$residuals)


fin_vars<-c('hc_per100k', 'gini_index', 'share_non_white',
            'share_voters_voted_trump', 'con_uni_combo', 'elasticity')


# correlation matrix with the subset of variables

pairs(fbi_df_2016[fin_vars], lower.panel=NULL, main = 'Final Variables')

fin_data<-fbi_df_2016[fin_vars]

model_full<-lm(hc_per100k~., data = fin_data)

summary(model_full)

red<-lm(hc_per100k~gini_index+share_voters_voted_trump+share_non_white+con_uni_combo, fin_data) # tested without elasticity, but they're significant; remove elasticity

anova(red, model_full) # elasticity is not significant with alpha = 0.05

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

# mostly produce the same model as above (with elasticity, which we've determined isn't significant)
# same model with hate group might be decent. will check 

hategroup_mod<-lm(formula = hc_per100k ~ share_voters_voted_trump + share_non_white + hate_group_count_2016 +
                    gini_index + con_uni_combo + elasticity, data = fbi_df_2016)

summary(hategroup_mod)

hategroup_mod_red<-lm(formula = hc_per100k ~ share_voters_voted_trump + share_non_white + hate_group_count_2016 +
                        gini_index + con_uni_combo, data = fbi_df_2016)
# removed elasticity

summary(hategroup_mod_red)

anova(hategroup_mod, hategroup_mod_red)

# can keep elasticity out. check about hate group

hategroup_mod_red<-lm(formula = hc_per100k ~ share_voters_voted_trump + share_non_white +
                        gini_index + con_uni_combo, data = fbi_df_2016)



anova(hategroup_mod, hategroup_mod_red)

# partial F says it's not significant (cannot reject H_0, that its slope's 0)

# keep prior model

best_mod <- red


# outlier analysis #

# residuals

res_full <- unname(best_mod[['residuals']]) # need to remove the field headers
#res_2016 # uncomment to see residuals

# outlier, influential & leverage points analysis

# studentized residuals

student.res<-rstandard(best_mod) 

# externally studentized residuals

ext.student.res<-rstudent(best_mod) 

sort(ext.student.res)
par(mfrow=c(1,3))
plot(best_mod$fitted.values,res_full,main="Residuals")
plot(best_mod$fitted.values,student.res,main="Studentized Residuals")
plot(best_mod$fitted.values,ext.student.res,main="Externally  Studentized Residuals")

# calc values

# plot residuals vs standardized residuals found above
n<-length(fin_data$hc_per100k)
p<-length(best_mod$coefficients)

##critical value using Bonferroni procedure
qt(1-0.05/(2*n), n-p-1)

par(mfrow=c(1,1))

plot(ext.student.res,main="Externally Studentized Residuals", ylim=c(-4,4))
abline(h=qt(1-0.05/(2*n), n-p-1), col="red")
abline(h=-qt(1-0.05/(2*n), n-p-1), col="red")

# no outliers in the response...

ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)]

##leverages
lev_full<-lm.influence(best_mod)$hat 

sort(lev_full)

plot(lev_full, main="Leverages", ylim=c(0,0.6))
abline(h=2*p/n, col="red")

# get leverage points

lev_full[lev_full>2*p/n]

# DC and VT leveraged in full

# influential observations
DFFITS<-dffits(best_mod)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]

DFBETAS<-dfbetas(best_mod)
DFBETAS[abs(DFBETAS)>2/sqrt(n)]

COOKS<-cooks.distance(best_mod)
COOKS[COOKS>qf(0.5,p,n-p)]

# influential points by DFFITS, but not Cooks


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

n <- nrow(fbi_df_2016)
sum_ressqr <- 0 # start with MSE = 0

for (i in 1:n) # loop through i = 1 to 28
{

  testrow <- fbi_df_2016[c(i), ] # test DF is just the ith row
  train_data <- fbi_df_2016[-c(i),] # training data is a DF with every row except the ith
  
  preds<-predict(best_mod,testrow, type="response") # predict the response for the ith row
  
  sum_ressqr <- sum_ressqr + (preds - testrow$hc_per100k)^2 # add the res^2 to the cumulative sum 
  print(preds - testrow$hc_per100k)
  
}

print(sum_ressqr/n) # avg MSE for the LOOCV

# check with hate group, found from MSE, adjR^2, etc. analysis

sum_ressqr <- 0 # start with MSE = 0

for (i in 1:n) # loop through i = 1 to 28
{
  
  testrow <- fbi_df_2016[c(i), ] # test DF is just the ith row
  train_data <- fbi_df_2016[-c(i),] # training data is a DF with every row except the ith
  
  preds<-predict(hategroup_mod,testrow, type="response") # predict the response for the ith row
  
  sum_ressqr <- sum_ressqr + (preds - testrow$hc_per100k)^2 # add the res^2 to the cumulative sum 
  print(preds - testrow$hc_per100k)
  
}

print(sum_ressqr/n) # avg MSE for the LOOCV


# w elasticity and hate group does slightly better by MSE
# both produce pretty good MSEs, so both predict pretty well

summary(best_mod)
summary(hategroup_mod)



##perform levene's test. Null states the variances are equal for all classes. 
library(lawstat)
levene.test(fbi_df_2016$hc_per100k,fbi_df_2016$con_uni_combo) # variances are equal
#levene.test(hc_df$fbi_2019_per100k,hc_df$permit) # variances are equal
#levene.test(hc_df$fbi_2019_per100k,hc_df$universl) # variances are equal


summary(best_mod)


##perform Tukey's multiple comparisons
library(multcomp)
pairwise<-glht(model_full, linfct = mcp(con_uni_combo= "Tukey"))
summary(pairwise)


levels(fbi_df_2016$con_uni_combo)


###### Interaction analysis

# consider each cut as a subset
neither<-subset(fbi_df_2016,con_uni_combo=="Neither") 
con<-subset(fbi_df_2016,con_uni_combo=="Confederate Only")
law<-subset(fbi_df_2016,con_uni_combo=="Gun Law Only")


# fit separate regressions
gini_neither <- lm(hc_per100k~gini_index,data=neither)
gini_con <- lm(hc_per100k~gini_index,data=con)
gini_law <- lm(hc_per100k~gini_index,data=law)


# Create scatters:

plot(fbi_df_2016$gini_index, fbi_df_2016$hc_per100k, main="Con and Gun Law")
points(neither$gini_index, neither$hc_per100k, pch=2, col="blue")
points(con$gini_index, con$hc_per100k, pch=3, col="red")
points(law$gini_index, law$hc_per100k, pch=4, col="orange")


abline(gini_neither,lty=1, col="blue")
abline(gini_con,lty=2, col="red") 
abline(gini_law,lty=3, col="orange")


summary(red)



###############

# fit separate regressions
trump_neither <- lm(hc_per100k~share_voters_voted_trump,data=neither)
trump_con <- lm(hc_per100k~share_voters_voted_trump,data=con)
trump_law <- lm(hc_per100k~share_voters_voted_trump,data=law)


# Create scatters:

plot(fbi_df_2016$share_voters_voted_trump, fbi_df_2016$hc_per100k, main="Con and Gun Law")
points(neither$share_voters_voted_trump, neither$hc_per100k, pch=2, col="blue")
points(con$share_voters_voted_trump, con$hc_per100k, pch=3, col="red")
points(law$share_voters_voted_trump, law$hc_per100k, pch=4, col="orange")


abline(trump_neither,lty=1, col="blue")
abline(trump_con,lty=2, col="red") 
abline(trump_law,lty=3, col="orange")




###############

# fit separate regressions
nw_neither <- lm(hc_per100k~share_non_white,data=neither)
nw_con <- lm(hc_per100k~share_non_white,data=con)
nw_law <- lm(hc_per100k~share_non_white,data=law)


# Create scatters:

plot(fbi_df_2016$share_non_white, fbi_df_2016$hc_per100k, main="Con and Gun Law")
points(neither$share_non_white, neither$hc_per100k, pch=2, col="blue")
points(con$share_non_white, con$hc_per100k, pch=3, col="red")
points(law$share_non_white, law$hc_per100k, pch=4, col="orange")


abline(nw_neither,lty=1, col="blue")
abline(nw_con,lty=2, col="red") 
abline(nw_law,lty=3, col="orange")



###############

# fit separate regressions
el_neither <- lm(hc_per100k~elasticity,data=neither)
el_con <- lm(hc_per100k~elasticity,data=con)
el_law <- lm(hc_per100k~elasticity,data=law)


# Create scatters:

plot(fbi_df_2016$elasticity, fbi_df_2016$hc_per100k, main="Con and Gun Law")
points(neither$elasticity, neither$hc_per100k, pch=2, col="blue")
points(con$elasticity, con$hc_per100k, pch=3, col="red")
points(law$elasticity, law$hc_per100k, pch=4, col="orange")


abline(el_neither,lty=1, col="blue")
abline(el_con,lty=2, col="red") 
abline(el_law,lty=3, col="orange")


######################



summary(best_mod)


inter_full<-lm(formula = hc_per100k ~ .^2, data = fin_data)

summary(inter_full)

inter_1<-lm(formula = hc_per100k ~ gini_index*con_uni_combo +
            + share_non_white*con_uni_combo
            + share_voters_voted_trump*con_uni_combo +
            + elasticity*con_uni_combo + share_voters_voted_trump*elasticity 
            + share_non_white*elasticity, data = fin_data)

summary(inter_1)

# start removing preds and check partial F

red_inter_1 <-lm(formula = hc_per100k ~ gini_index*con_uni_combo +
                   + share_non_white*con_uni_combo
                 + share_voters_voted_trump*con_uni_combo +
                 + share_non_white*elasticity, data = fin_data)

summary(red_inter_1)
anova(inter_1, red_inter_1)

# can support red_inter_1 from partial F. widdle down some more:

red_inter_1 <-lm(formula = hc_per100k ~ share_non_white*con_uni_combo
                 + share_voters_voted_trump*con_uni_combo +
                   + share_non_white*elasticity, data = fin_data)

summary(red_inter_1)
anova(inter_1, red_inter_1)

# keep reducing


red_inter_1 <-lm(formula = hc_per100k ~ share_non_white
                 + share_voters_voted_trump*con_uni_combo +
                   + share_non_white*elasticity, data = fin_data)

summary(red_inter_1)
anova(inter_1, red_inter_1)



red_inter_1 <-lm(formula = hc_per100k ~ share_non_white
                 + share_voters_voted_trump*con_uni_combo +
                   + share_non_white*elasticity, data = fin_data)

no_inter<-lm(formula = hc_per100k ~ share_non_white
                 + share_voters_voted_trump + con_uni_combo +
                   + elasticity, data = fin_data)

summary(no_inter)
summary(red_inter_1)
anova(inter_1, red_inter_1)

best_mod<-red_inter_1

summary(red_inter_1) # hierarchical principle: higher order (interaction) terms are significant, so must leave lower-order terms in


#vcov(red_inter_1)


vif(no_inter)

# no multicolinearity

# check if it predicts 2019 HC data well

fbi_df_2019$hc_per100k<-sqrt(fbi_df_2019$hc_per100k)

n <- nrow(fbi_df_2016) # number of rows (28)
sum_ressqr <- 0 # start with MSE = 0

for (i in 1:n) # loop through i = 1 to 28
{
  
  testrow <- fbi_df_2016[c(i), ] # test DF is just the ith row
  train_data <- fbi_df_2016[-c(i),] # training data is a DF with every row except the ith
  
  preds<-predict(best_mod,testrow, type="response") # predict the response for the ith row
  
  sum_ressqr <- sum_ressqr + (preds - testrow$hc_per100k)^2 # add the res^2 to the cumulative sum 
  #print((preds - testrow$hc_per100k)/testrow$hc_per100k)
  print(preds - testrow$hc_per100k)
  print(preds)
  
}

print(sum_ressqr/n) # avg MSE for the LOOCV

# does pretty well


# 2019

n <- nrow(fbi_df_2019) # number of rows (28)
sum_ressqr <- 0 # start with MSE = 0

for (i in 1:n) # loop through i = 1 to 28
{
  
  testrow <- fbi_df_2019[c(i), ] # test DF is just the ith row
  train_data <- fbi_df_2019[-c(i),] # training data is a DF with every row except the ith
  
  preds<-predict(best_mod,testrow, type="response") # predict the response for the ith row
  
  sum_ressqr <- sum_ressqr + (preds - testrow$hc_per100k)^2 # add the res^2 to the cumulative sum 
  #print((preds - testrow$hc_per100k)/testrow$hc_per100k)
  print(preds - testrow$hc_per100k)
  print(preds)
  
}

print(sum_ressqr/n) # avg MSE for the LOOCV

# not the best, but not terrible



plot(best_mod$fitted.values,best_mod$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

library(MASS)
boxcox(best_mod, lambda = seq(-1, 4, 1/10))

##acf plot of residuals
acf(best_mod$residuals)

summary(best_mod)


