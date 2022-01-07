#Ordered Discrete Choice models
#================

#This script was developed by Carlos Roque (LNEC) with some "polishing" by Filipe Moura.
#(v.2022)
  
  #### Example exercise: Run-Off-Road crashes occured in Portuguese freeways 
  ##### during 2009 and 2010, provided by the National Road Safety Authority (ANSR).
  
#  **Your task**: Estimate a Ordered Discrete Choice model that detects_
#the unforgiving roadside contributors to different severity levels of crashes.

#### Variables:

#-   `CHOICE_ORD`: severity levels of accidentes (PDO = 1; Light injury = 2; severe injury and death = 3);
#-   `Winter`: crash occured during the winter;
#-   `peak1820`: crash occured during the peak hour;
#-   `valeta`: collision with a ditch as first harmful event;
#-   `Curva`: crash occured in a right curve (= 1);
#-   `ACDIR`: leaving the road to the right side of the carriageway;
#-   `capota`: crash resulted in a rollover;
#-   `veic1`: crash involved another passenger car;
#-   `age`: driver's age;
#-   `age32`: driver under 32 years;
#-   `age26`: driver under 26 years;
#-   `age23`: driver under 23 years;
#-   `age21`: driver under 21 years;
#-   `gender`: driver's gender (female =1);
#-   `NOBST`: # of obstacles hit in a ROR crash;
#-   `LVEL`: segment speed limit;
#-   `NOCUP`: # of persons involved in the accident.
#-   `TXALC`: the driver's blood alcohol level.

##### Import Libraries
library(readr)
library(ordinal)
library(VGAM)


##### Import dataset
data_3lev <- read.delim("/Volumes/HD2/Documents_20200412/Aulas/TDM/Examples/ODCM_example/example_croque/Data_freeways_2009_2010_3levels_v1.txt")
head(data_3lev)
View(data_3lev)

attach(data_3lev)

####Reclassify variables 
#####lhs variable
CHOICE_ORD<- as.factor(CHOICE_ORD)
CHOICE_ORD <- as.ordered(CHOICE_ORD)

#####rhs variables
CAPOTA <- as.factor(CAPOTA)
WINTER <- as.factor(WINTER)
GENDER <- as.factor(GENDER)
ACDIR <- as.factor(ACDIR)
VEIC1 <- as.factor(VEIC1)
PEAK1820 <- as.factor(PEAK1820)
AGE32 <- as.factor(AGE32)
AGE21 <- as.factor(AGE21)
AGE23 <- as.factor(AGE23)
AGE26 <- as.factor(AGE26)
VALETA <- as.factor(VALETA)
AGE <- as.integer(AGE)
NOCUP <- as.integer(NOCUP)
LVEL <- as.numeric(LVEL)

#models

##Determining the null model, assuming that a parallel regression is possible
null <- vglm(CHOICE_ORD ~ 1, family=cumulative(parallel=TRUE, reverse = TRUE), data = data_3lev)
summary(null)

##1st model (logit)
fit1 <- vglm(CHOICE_ORD ~ NOCUP + GENDER + WINTER + ACDIR+ VEIC1 + NOBST + CAPOTA + VALETA + AGE32 + PEAK1820, family=cumulative(parallel=TRUE, reverse = TRUE), data = data_3lev)
summary(fit1)
coef(fit1) #returns the intercepts (cut-off) and variables parameters only
coef(fit1, matrix= TRUE) #returns the intercepts (cut-off) and variables parameters only in a mtrix for each category
deviance(fit1)

##2nd model (probit)
fit2 <- vglm(CHOICE_ORD ~ WINTER + PEAK1820  + NOBST + VALETA + NOCUP + ACDIR+ CAPOTA + VEIC1 + AGE32  + GENDER, family=cumulative(parallel = FALSE ~GENDER, reverse = TRUE), data = data_3lev)
summary(fit2)
coef(fit2)#returns the variables parameters only
coef(fit2, matrix= TRUE) #returns the intercepts (cut-off) and variables parameters only in a mtrix for each category
deviance(fit2)

#Testing the parallelism assumption
##Formally, there are two common ways to test the parallelism assumption. 

###The first is by a likelihood ratio test
lrtest(fit1, fit2)  

#When compared by the likelihood ratio test, it appears that the PPO model
#with single coefficient for the GENDER variable is adequate 
# (the fit is significantly different).The small p-value here indicates that 
#a parallelism assumption is not reasonable.

###The second method is using the Wald test
(cfit <- coef(fit2))
index <- 4:5 # These coefficients need testing for equality (2 values in this case)
L.mat <- cbind(diag(npred(fit2) - 1), -1) # Matrix of contrasts
T.mat <- solve(L.mat %*% vcov(fit1)[index, index] %*% t(L.mat))
W.stat <- t(L.mat %*% cfit[index]) %*% T.mat %*% (L.mat %*% cfit[index])
W.stat
round(digits = 5, pchisq(W.stat, df = nrow(L.mat), lower.tail = FALSE))  # p-value

#If for the coefficients tested for equality, the Wald test is significant 
#(p-value less than 0.05), we are able to reject the null hypothesis.
#Then we would conclude that the parameters associated with these variables are 
#not equal, so that a parallelism assumption is not reasonable.


#Marginal Effects
nrow= nparam(fit2) - nparam(null) 
ncol=3
k <- nobs(fit2)
marginal<-round(digits = 4, margeff(fit2))
mean = matrix(1:(nrow*ncol),nrow,ncol)
sd = matrix(1:(nrow*ncol),nrow,ncol)
for (j in 1:ncol) {
  for (i in 1:nrow) {
    mean [i,j]<- round(digits = 4,mean(marginal[i,j,1:k]))
    sd [i,j]<- round(digits = 4,sd(marginal[i,j,1:k]))
  }
}

mean
sd


#Model fitting statistics
statistics <- function (model, model_null)
{
  adjusted_rho2 <- round(digits = 4, 1-((logLik(model)-nparam(model))/logLik(model_null)))
  pseudoR2<- round(digits = 4,1-(logLik(model)/logLik(model_null)))
  cat("adjusted_rho2: ", adjusted_rho2," pseudoR2: ", pseudoR2, "AIC: ", AIC(model), "BIC: ", BIC(model)) 
}

statistics(fit2,null)

#Predict the probabilities of each outcome category
fitted(fit2)
depvar(fit2)
predict(fit2)
     