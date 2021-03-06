#' 
#' #### Example: Work-to-home departure delay
#' 
#' A survey of 204 Seattle-area commuters was conducted to examine the duration of time that commuters delay their work-to-home trips in an effort to avoid peak period traffic congestion. Of the 204 commuters surveyed, 96 indicated that they sometimes delayed their work-to-home trip to avoid traffic congestion. These commuters provided their average time delay. Thus, each commuter has a completed delay duration so that neither left nor right censoring is present in the data.
#' 
#' **Your tasks:**
#' 
#' 1. Plot the Kaplan-Meier estimate of the duration of time that commuters delay
#' their work-to-home trips;
#' 2. Determine the significant factors that affect the duration of commuters’ delay
#' using a Cox model;
#' 3. Examine the work-to-home departure delay using exponential, Weibull, and
#' log-logistic proportional-hazards models.
#' 
#' ## Get to know your data
#' ##### Import Libraries
#' 
library(readxl)
library(skimr)
library(survival)
library(coxme)
library(survminer)
library(ggplot2)

#' 
#' ##### Import dataset, transform in dataframe, and take a first look.

data.delay <- read_excel("Data/ExerciseHBDM.xlsx")
data.delay <- data.frame(data.delay)
head(data.delay) #variable names are missing

#' 
#' ##### Assign variable lables to each one respectively

names(data.delay) <- c("id","minutes","activity","number_of_times","mode","route","congested","age",
                        "gender","number_cars","number_children","income","flexible","distance",
                        "LOSD","rate_of_travel","population","retail","service","size") 
df <- data.frame(data.delay, row.names = 1) #make id (1st variable) as row name

#' 
#' ##### Take a look at the structure

str(df)
skim(df)

#' 
#' ##### Plot yout data

df <- df[order(df$minutes),] #sort by time
plot(df$minutes, type="h") #high-density vertical lines

#' 
#' ## Survival function
#' ### 1.Plot the Kaplan-Meier estimate curve
#' ##### Create the life table survival object for df

data.delay2 <-subset(df, minutes>0)
df.survfit = survfit(Surv(minutes) ~ 1, data= data.delay2)
summary(df.survfit)

#' 
#' >**Note:** The functions `survfit()` and `Surv()` create a life table survival object.
#' 
#' ##### Plot the Kaplan-Meier curve

# option 1
plot(
  df.survfit,
  xlab = "Time (minutes)",
  ylab = "Survival
probability",
conf.int = TRUE
)


#option 2
ggsurvplot(
  df.survfit,
  xlab = "Time (minutes)",
  xlim =
    range(0:250) ,
  conf.int = TRUE,
  pallete = "red",
  ggtheme =
    theme_minimal()
)


#' > **Note:** It is the most widely applied nonparametric method in survival analysis.The Kaplan–Meier method provides useful estimates of survival probabilities and a graphical presentation of the survival distribution.  
#' The Kaplan–Meier method assumes that censoring is independent of survival times. If this is false, the Kaplan–Meier method is inappropriate.
#' 
#' ### 2. Cox proportional-hazards Model  
#' The Cox proportional-hazards model is semiparametric method that produces estimated hazard ratios (sometimes called rate ratios or risk ratios).  
#' Let's perform a Cox Proportional Hazard Model Estimate estimate the model for Duration of Commuter Work-To-Home Delay to Avoid Congestion

result.cox <- coxph(Surv(minutes) ~ gender + rate_of_travel + distance + population, data= data.delay2)
summary(result.cox)

#' 
#' > **Note:** Regression coefficients are on a log scale.
#' 
#' ##### Testing proportional Hazards assumption  
#' Test the proportional hazards assumption on the basis of partial residuals. Type of residual known as Schoenfeld residuals.  
#' It includes an interaction between the covariate and a function of time (or distance). Log time is often used but it could be any function.
#' For each covariate, the function `cox.zph()` correlates the corresponding set of scaled Schoenfeld residuals with time, to test for independence between residuals and time. Additionally, it performs a global test for the model as a whole.  
#' A plot that shows a non-random pattern against time is evidence of violation of the PH assumption.  

test.ph <- cox.zph(result.cox)
par(mfrow=c(2,2))
plot(test.ph)
ggcoxzph(test.ph)

#' 
#' > **Note:**  If significant then the assumption is violated.  
#' In principle, the Schoenfeld residuals are independent of time.
#' 
#' 
#' ##### Plot the baseline survival function

ggsurvplot(
  survfit(result.cox),
  data = data.delay2,
  palette = "#2E9FDF",
  ggtheme = theme_minimal()
)

#' 
#' ##### Plot the cummulative hazard function

ggsurvplot(
  survfit(result.cox),
  data = data.delay2,
  conf.int = TRUE,
  palette = c("#FF9E29", "#86AA00"),
  risk.table = TRUE,
  risk.table.col = "strata",
  fun = "event"
)

#' 
#' ##### Calculate the McFadden Pseudo R-square

result.cox$loglik
LLi = result.cox$loglik[1] # Initial log-likelihood
LLf = result.cox$loglik[2] # Final log-likelihood
1- (LLf/LLi) # pseudo R-suare

#' 
#' 
#' ### 3. Exponential, Weibull, and log-logistic proportional-hazards models
#' Parametric Model Estimates of the Duration of Commuter Work-ToHome Delay to Avoid Congestion
#' 
#' * **Exponential**

survreg(
  Surv(minutes) ~ gender + rate_of_travel + distance + population,
  data = data.delay2,
  dist = "exponential" )

#' 
#' * **Weibull**

survreg(
  Surv(minutes) ~ gender + rate_of_travel + distance + population,
  data = data.delay2,
  dist = "weibull" )

#' 
#' * **Log-logistic**

survreg(
  Surv(minutes) ~ gender + rate_of_travel + distance + population,
  data = data.delay2,
  dist = "loglogistic" ) 


#' >**Note:** The argument _dist_ has several options to describe the parametric model used ("weibull", "exponential", "gaussian", "logistic", "lognormal", or "loglogistic". See more with `?survreg`
