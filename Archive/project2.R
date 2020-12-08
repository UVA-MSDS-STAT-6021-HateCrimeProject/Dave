library(glmnet)
library(faraway)
library(leaps)

hc_df<-read.csv('hate_crimes_full.csv', row.names = 'state_abbrev')

#hc_df<-hc_df[!apply(hc_df['state_full'] == "", 1, all),]
colnames(hc_df)

hc_df$confederate<-factor(hc_df$confederate)
hc_df$permit<-factor(hc_df$permit)
hc_df$universl<-factor(hc_df$universl)

hc_df<-hc_df[ , !(names(hc_df) %in% c('hate_crimes_per_100k_splc', 'state_full', 'FIP', 'Year', 
                                      'FIP	Year',	'HFR_se',	'Fem_FS_S',	'Male_FS_S',	'BRFSS',	'GALLUP',	'GSS',	'PEW',	'HuntLic',	'GunsAmmo',	
                                      'BackChk',	'PewQChng',	'BS1',	'BS2',	'BS3'))]

hc_df<-hc_df[!is.na(c(hc_df$avg_hatecrimes_per_100k_fbi, 
                      hc_df$share_non_citizen)),]

hc_df<-hc_df[complete.cases(hc_df), ] # removes rows with NAs

nrow(hc_df)

x<-model.matrix(avg_hatecrimes_per_100k_fbi~., hc_df)[,-1] # remove the first column of 1s representing the intercept
y<-hc_df$avg_hatecrimes_per_100k_fbi

#x
nrow(x)

pairs(x, lower.panel=NULL, main="Scatterplots of Predictors")

cor(x)

boxplot(hc_df$avg_hatecrimes_per_100k_fbi~hc_df$confederate)
boxplot(hc_df$avg_hatecrimes_per_100k_fbi~hc_df$universl)
boxplot(hc_df$avg_hatecrimes_per_100k_fbi~hc_df$permit)

model<-lm(avg_hatecrimes_per_100k_fbi~., data = hc_df)


vif(model) # HFR is highly correlated (18+ VIF)

summary(model)


###### Ridge
##Note some predictors are highly correlated with each other. 
pairs(x, lower.panel=NULL, main="Scatterplots of Predictors")

##alpha=0 for ridge, alpha=1 for LASSO
##threshold value should be very small if multicollinearity is present. see what happens if thresh was set to a larger value
##we know theoretically the coeffs should be the same as lm when lambda is 0
lasso.r<-glmnet(x,y,alpha=1, lambda=0, thresh = 1e-14)
coefficients(lasso.r)

##MLR - produce the same thing as above **as long as thresh is small enough
result<-lm(avg_hatecrimes_per_100k_fbi~.,hc_df)
summary(result)

##split data
set.seed(12)
train<-sample(1:nrow(x), nrow(x)/2)
test<-(-train)
y.test<-y[test]

##use CV to find optimal lambda based on training set
set.seed(12)
cv.out<-cv.glmnet(x[train,],y[train],alpha=1) # lasso regression
bestlam<-cv.out$lambda.min # value of lambda that minimizes MSE (the optimal value)
bestlam
plot(cv.out)

##fit lasso regression using training data
lasso.mod<-glmnet(x[train,],y[train],alpha=1,lambda=bestlam, thresh = 1e-14)

##test MSE with lambda=1
lasso.pred.0<-predict(lasso.mod,newx=x[test,])
mean((lasso.pred.0-y.test)^2)

##fit lasso regression using training data
lasso.mod<-glmnet(x[train,],y[train],alpha=0,lambda=bestlam, thresh = 1e-14)

##test MSE with lambda=1
lasso.pred.0<-predict(lasso.mod,newx=x[test,])
mean((lasso.pred.0-y.test)^2)



##Compare ridge with OLS using best lambda and all observations
out.lasso<-glmnet(x,y,alpha=1,lambda=bestlam,thresh = 1e-14)
out.ridge<-glmnet(x,y,alpha=0,lambda=bestlam,thresh = 1e-14)
out.ols<-glmnet(x,y,alpha=0, lambda=0, thresh = 1e-14)
cbind(coefficients(out.lasso), coefficients(out.ridge), coefficients(out.ols))




#########################################

##perform all possible regressions (1st order)
allreg <- regsubsets(avg_hatecrimes_per_100k_fbi ~., data=hc_df, nbest=9)

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

##intercept only model
regnull <- lm(avg_hatecrimes_per_100k_fbi~1, data=hc_df)
##model with all predictors
regfull <- lm(avg_hatecrimes_per_100k_fbi~., data=hc_df)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")


for_model <- lm(formula = avg_hatecrimes_per_100k_fbi ~ confederate + gini_index + 
                  share_non_white + share_non_citizen, data = hc_df)

back_model <- lm(formula = avg_hatecrimes_per_100k_fbi ~ share_non_citizen + 
                   gini_index + share_non_white + confederate + elasticity + 
                   universl, data = hc_df)

both_model <- lm(formula = avg_hatecrimes_per_100k_fbi ~ share_non_white + 
                   share_non_citizen, data = hc_df)

summary(for_model)
summary(back_model)
summary(both_model)

pairs(hc_df, lower.panel = NULL)


plot(for_model$fitted.values,for_model$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

library(MASS)
boxcox(for_model, lambda = seq(-1, 2, 1/10))

##acf plot of residuals
acf(for_model$residuals)


# adj r2

adj <- lm(formula = avg_hatecrimes_per_100k_fbi ~ median_household_income + share_non_citizen + share_white_poverty + gini_index + share_non_white
+ confederate + elasticity + universl, hc_df)

summary(adj)

plot(adj$fitted.values,adj$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")


