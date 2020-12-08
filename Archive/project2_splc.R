library(glmnet)
library(faraway)
library(leaps)

hc_df<-read.csv('hate_crimes_full.csv', row.names = 'state_abbrev')

#hc_df<-hc_df[!apply(hc_df['state_full'] == "", 1, all),]
colnames(hc_df)

hc_df$confederate<-factor(hc_df$confederate)
hc_df$permit<-factor(hc_df$permit)
hc_df$universl<-factor(hc_df$universl)

hc_df<-hc_df[ , !(names(hc_df) %in% c('avg_hatecrimes_per_100k_fbi', 'state_full', 'FIP', 'Year', 
                                      'FIP	Year',	'HFR_se',	'Fem_FS_S',	'Male_FS_S',	'BRFSS',	'GALLUP',	'GSS',	'PEW',	'HuntLic',	'GunsAmmo',	
                                      'BackChk',	'PewQChng',	'BS1',	'BS2',	'BS3'))]

hc_df<-hc_df[!is.na(c(hc_df$hate_crimes_per_100k_splc, 
                      hc_df$share_non_citizen)),]

hc_df<-hc_df[complete.cases(hc_df), ] # removes rows with NAs

nrow(hc_df)

x<-model.matrix(hate_crimes_per_100k_splc~share_unemployed_seasonal+
                  share_population_with_high_school_degree+
                  share_white_poverty+
                  share_non_white+
                  median_household_income+
                  share_population_in_metro_areas+
                  share_non_citizen+
                  gini_index+
                  gini_index+
                  elasticity+
                  confederate+
                  HFR+
                  universl+
                  permit, hc_df)[,-1] # remove the first column of 1s representing the intercept
y<-hc_df$hate_crimes_per_100k_splc

#x
nrow(x)

pairs(x, lower.panel=NULL, main="Scatterplots of Predictors")

cor(x)

boxplot(hc_df$hate_crimes_per_100k_splc~hc_df$confederate)
boxplot(hc_df$hate_crimes_per_100k_splc~hc_df$universl)
boxplot(hc_df$hate_crimes_per_100k_splc~hc_df$permit)

model<-lm(hate_crimes_per_100k_splc~., data = hc_df)


vif(model) # HFR is highly correlated (18+ VIF)

summary(model)


###### Ridge

ridge.r<-glmnet(x,y,alpha=0, lambda=0, thresh = 1e-14)
coefficients(ridge.r)

##split data
set.seed(12)
train<-sample(1:nrow(x), nrow(x)/2)
test<-(-train)
y.test<-y[test]


###### Lasso

lasso.r<-glmnet(x,y,alpha=0, lambda=1, thresh = 1e-14)
coefficients(lasso.r)

##split data
set.seed(12)
train<-sample(1:nrow(x), nrow(x)/2)
test<-(-train)
y.test<-y[test]

#########################################

##perform all possible regressions (1st order)
allreg <- regsubsets(hate_crimes_per_100k_splc ~., data=hc_df, nbest=9)

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
regnull <- lm(hate_crimes_per_100k_splc~1, data=hc_df)
##model with all predictors
regfull <- lm(hate_crimes_per_100k_splc~., data=hc_df)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")


for_model <- lm(formula = hate_crimes_per_100k_splc ~ universl + share_population_with_high_school_degree, 
                data = hc_df)

back_model <- lm(formula = hate_crimes_per_100k_splc ~ median_household_income + 
                   share_population_with_high_school_degree + share_non_citizen + 
                   share_non_white + share_voters_voted_trump + elasticity + 
                   HFR + universl, data = hc_df)

both_model <- lm(formula = hate_crimes_per_100k_splc ~ universl + share_population_with_high_school_degree, 
                 data = hc_df)

summary(for_model)
summary(back_model)
summary(both_model)

reduce<-lm(formula = hate_crimes_per_100k_splc ~ share_non_citizen + 
             share_non_white + share_voters_voted_trump + elasticity + 
             HFR + universl, data = hc_df)


anova(reduce,back_model)

summary(reduce)

pairs(hc_df, lower.panel = NULL)
