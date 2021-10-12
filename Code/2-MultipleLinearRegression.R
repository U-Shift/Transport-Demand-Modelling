
#' 
#' #### Example exercise: Trip production of 57 Traffic Assignment Zones of Chicago in 1960's.
#' 
#' **Your task**: Estimate a linear regression model that predicts trips per occupied dwelling unit.  
#' 
#' #### Variables:
#' 
#' * `TODU`: Motorized Trips (private car or Public Transportation) per occupied dwelling unit;
#' * `ACO`: Average car ownership (cars per dwelling);
#' * `AHS`: Average household size;
#' * `SRI`: Social Rank Index:  
#'       1. proportion of blue-collar workers (e.g., construction, mining);  
#'       2. proportion of people with age higher than 25 years that have completed at least 8 year of education;
#'         (_**Note:** The SRI has its maximum value when there are no blue-collar workers and all adults have education            of at least 8 years_) 
#' * `UI`: Urbanization Index:  
#'       1. fertility rate, defined as the ratio of children under 5 years of age to  the female population of childbearing age;  
#'       2. female labor force participation rate, meaning the % of women who are in the labor force;  
#'       3. % of single family units to total dwelling units.
#' 
#'     The degree of urbanization index would be increased by
#'         a) lower fertility rate,
#'         b) higher female labor force participation rate, and
#'        c) higher proportion of single dwelling units.
#'       (_**Note:** High values for this index imply less attachment to the home_)
#' 
#' * `SI`:Segregation Index
#'    It measures the proportion of an area to which minority groups (e.g: non-whites, foreign-born, Eastern Europeans) live in isolation.
#'      (_**Note:** High values for this index imply that those communities are less prone to leaving their living areas and as such to having lower levels of mobility_)
#' 
#' ## Let's begin!
#' 
#' ##### Import Libraries
library(readxl) #Library used to import excel files
library(tidyverse) # Pack of most used libraries
library(skimr) # Library used for providing a summary of the data
library(DataExplorer) # Library used in data science to perform exploratory data analysis
library(corrplot) # Library used for correlation plots
library(car) # Library used for testing autocorrelation (Durbin Watson)
library(olsrr) # Library used for testing multicollinearity (VIF, TOL, etc.)

#' 
#' ##### Import dataset
dataset <- read_excel("Data/TDM_Class3_MLR_Chicago_Example.xls")
class(dataset)

#' 
#' ##### Transform the dataset into a dataframe
df <- data.frame(dataset)

#' 
#' ##### Show summary statistics
skim(df)
summary(df)

#' 
#' ## Multiple Linear Regression
#' Equation with `TODU` as the dependent variable:  
#' 
#' #### Checking assumptions
#' Before running the model, you need to check if the assumptions are met.
#' 
#' ##### Linear relation
#' For instance, let's take a look if the independent variables have linear relation with the dependent variable.
#' 
par(mfrow=c(2,3)) #set plot area as 2 rows and 3 columns
plot(x = df$TODU, y = df$ACO, xlab = "TODU", ylab = "ACO")  
plot(x = df$TODU, y = df$AHS, xlab = "TODU", ylab = "AHS")  
plot(x = df$TODU, y = df$SI, xlab = "TODU", ylab = "SI")  
plot(x = df$TODU, y = df$SRI, xlab = "TODU", ylab = "SRI")  
plot(x = df$TODU, y = df$UI, xlab = "TODU", ylab = "UI")

#' 
#' Or you could execute a pairwise scatterplot matrix, that compares every variable with each other: 
#' 
pairs(df[,1:6], pch = 19, lower.panel = NULL)

#' 
#' > **Note:** SRI and TODU do not have a linear relationship. This should interfere on the model. 
#' 
#' ##### Normal distribution of the dependent variable
#' Check if the dependent variable is normally distributed. If the sample is smaller than 2000 observations, use Shapiro-Wilk test: 
#' 
shapiro.test(df$TODU)

#' 
#' If not, use the Kolmogorov-Smirnov test
#' 
ks.test(df$TODU, "pnorm", mean=mean(df$TODU), sd = sd(df$TODU))

#' 
#' > **Note:** Regarding the warning that appears in the Kolmogorov-Smirnov test "ties should not be present for the Kolmogorov-Smirnov test", what most likely happened is that this test is only reliable with continuous variables.   
#' 
#' Although `TODU` is a continuous variable, the small sample size (n=57) makes it likely to have repeated values. Consequently, the test considers `TODU` as a categorical variable. Therefore, this is another evidence, that for small samples it is more appropriate to use the Shapiro-Wilk Test.  
#' The null hypothesis of both tests is that the distribution is normal. Therefore, for the distribution to be normal, the pvalue > 0.05 and you should not reject the null hypothesis.
#' 
#' ### Multiple linear regression model
#' 
model <- lm(TODU ~ ACO + AHS + SI + SRI + UI, data = df)
summary(model)

#' 
#' **Assessing the model**:
#' 
#' 1. First check the **pvalue** and the **F statistics** of the model to see if there is any statistical relation between the dependent variable and the independent variables. If pvalue < 0.05 and the F statistics > Fcritical = 2,39, then the model is statistically acceptable.  
#' 2. The **R-square** and **Adjusted R-square** evaluate the amount of variance that is explained by the model. The difference between one and another is that the R-square does not consider the number of variables. If you increase the number of variables in the model, the R-square will tend to increase which can lead to overfitting. On the other hand, the Adjusted R-square adjust to the number of independent variables.  
#' 3. Take a look a the **t-value** and the Pr(>|t|). If the t-value > 1,96 or Pr(>|t|) < 0,05, then the IV is statistically significant to the model.   
#' 4. To analyze the **estimates** of the variables, you should first check the **signal** and evaluate if the independent variable has a direct or inverse relationship with the dependent variable. It is only possible to evaluate the **magnitude** of the estimate if all variables are continuous and standarzized or by calculating the elasticities. The elasticities are explained and demonstrated in chapter 4. 
#' 
#' 
#' ##### Residuals
#' Let's see how do the residuals behave by plotting them.  
#' 
#' * **Residuals vs Fitted:** This plot is used to detect non-linearity, heteroscedasticity, and outliers. 
#' * **Normal Q-Q:** The quantile-quantile (Q-Q) plot is used to check if the dependent variable follows a normal distribution.
#' * **Scale-Location:** This plot is used to verify if the residuals are spread equally (homoscedasticity) or not (heteroscedasticity) through the sample. 
#' * **Residuals vs Leverage:** This plot is used to detect the impact of the outliers in the model. If the outliers are outside the Cook-distance, this may lead to serious problems in the model. 
#' 
#' Try analyzing the plots and check if the model meets the assumptions. 
par(mfrow=c(2,2))
plot(model)

#' 
#' 
#' ##### Autocorrelation
#' Execute the Durbin-Watson test to evaluate autocorrelation of the residuals
durbinWatsonTest(model)

#' 
#' > **Note:** In the Durbin-Watson test, values of the D-W Statistic vary from 0 to 4. If the values are from 1.8 to 2.2 this means that there is no autocorrelation in the model. 
#' 
#' ##### Multicollinearity
#' Calculate the VIF and TOL to test for multicollinearity.
#' 
ols_vif_tol(model)

#' 
#' > **Note:** Values of VIF > 5, indicate multicollinearity problems.
#' 
#' Calculate the Condition Index to test for multicollinearity
ols_eigen_cindex(model)

#' 
#' > **Note:** Condition index values > 15 indicate multicollinearity problems, and values > 30 indicate serious problems of multicollinearity.
#' 
#' To test both simultaneously, you can run the code below:
ols_coll_diag(model)

