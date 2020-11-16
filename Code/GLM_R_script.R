# INSTITUTO SUPERIOR TECNICO, UNIVERSITY OF LISBON
# TRANSPORT DEMAND MODELLING COURSE, PROF. FILIPE MOURA

# GENERALIZED LINEAR MODELS

# Example: Analyze the number o accidents
# Accident data from California (1993 to 1998) and Michigan (1993 to 1997) 
# Variables:

## ACCIDENT: Count on injury accidents over observation period
## STATE: indicator variable for state: 
    ## 0: California
    ## 1: Michigan
## AADT1: Average annual daily traffic on major road
## AADT2: Average annual daily traffic on minor road
## Median: Median width on major road in feet
## DRIVE: Number of driveways within 250 ft of intersection center


# Import Libraries

# Library used for reading excel files
library(readxl)
# Library used for summary statistics
library(skimr)
# Library used in data science to perform exploratory data analysis
library(tidyverse) 
# Library used for goodness of fit parameters
library(vcd)
# Library used for goodness of fit
library(car)
# Library used for goodness of fit
library(rcompanion)
# Library used for the Vuong test
library(pscl)
# Library used for the Lagrange Multiplier test
library(plm)

# Set working directory

setwd("G:/O meu disco/TDM - Lecture R/Generalised Linear Models")

# Import dataset

dataset <- read_excel("TDM_GZLM_CALMICH_Example.xlsX")

# Let us take a look at the summary and structure of the dataset

summary(dataset)

str(dataset)

# Note: The dataset looks weird. Use the view function to take a better look at the dataset

view(dataset)

# Treat the dataset

# The best way to treat the dataset is to import it again. Use the following steps:
  ## 1. Go to "Import Dataset" on the "Environment" window at the upper right display;
  ## 2. Click on "From excel";
  ## 3. Check if "First row as names" is checked;
  ## 4. Put the number of rows you want to skip and click on "import"
  ## 5. This will generate a code, which you can copy and use it the next time you open the file.

# Therefore, here is the following code:
  TDM_GZLM_CALMICH_Example <- read_excel("TDM_GZLM_CALMICH_Example.xlsx", skip = 5)
 
 ## Transform "TDM_GZLM_CALMICH_Example" into a dataframe to better manage the dataset

 df <- data.frame(TDM_GZLM_CALMICH_Example)
 view(df)
 
# Now we can analyze the descriptive statistics of the dataframe
 
 str(df)
 
 skim(df)

# Take a look at the histograms of the variables

hist(df$ACCIDENT, xlab = "ACCIDENT", main = "Histogram of ACCIDENT") 
hist(df$STATE, xlab = "STATE", main = "Histogram of STATE")
hist(df$AADT1, xlab = "AADT1", main = "Histogram of AADT1")
hist(df$AADT2, xlab = "AADT2", main = "Histogram of AADT2")
hist(df$MEDIAN, xlab = "MEDIAN", main = "Histogram of MEDIAN")
hist(df$DRIVE, xlab = "DRIVE", main = "Histogram of DRIVE")

# Plot the density function of the variable ACCIDENTS
plot(density(df$ACCIDENT), main="Density estimate of ACCIDENTS")


# As the dependent variable is "count data", as has discrete values, then a poisson distribution should be more adequate.
# Poisson assumption: 
  # But first, take a look at the mean and the variance of the Dependent variable. 
  # Check if they are equal to each other. 

mean(df$ACCIDENT)
var(df$ACCIDENT)

#Coefficient of variance:

var(df$ACCIDENT)/mean(df$ACCIDENT)

  ## Note: If the coefficient of variance > 1, then you have overdispersion.

# Try estimating goodness of fit parameter for the PDF of ACCIDENT. Use the Maximum Likelihood method.
gf<-goodfit(df$ACCIDENT,type= "poisson", method= "ML")
summary(gf)

  ## Note: The null hypothesis is that it is a Poisson distribution. 
  ## Therefore, for it to be a poisson distribution, the pvalue > 0.05.

# Now let us run the many possible models
# Begin with a Poisson model:
 
 model1 = glm(ACCIDENT ~ as.factor(STATE) + AADT2 + MEDIAN + DRIVE + offset(log(AADT1)), family = poisson(link = "log"), data = df, method = "glm.fit")

  ## Note: The method "glm.fit" uses iteratively reweighted least squares to fit the model. 
  ## Try looking for other methods and see the difference. 
  
# There are many families and links that can be used, depending on the characteristics of your data.
 
# Family                Link
# gaussian              identity
# binomial              logit, probit, cloglog
# poisson               log, identity, sqrt
# gamma                 inverse, identity, log
# inverse.gaussian      1/mu^2

summary(model1)

# Note: If we obtain: 
 ## residuals > degrees of freedom (overdispersion); 
 ## residuals < degrees of freedom (underdispersion);
 ## residuals = degrees of freedom (mean = variance).
 
# In overdispersion, the estimates are reliable but the standard errors tend to be smaller. 
 

#botar o lagrange


# Calculate the pseudo-Rsquare and perform an Omnibus test
nagelkerke(model1)

  ## Note: The likelihood ratio test (Omnibus test) compares the fitted model ("Model") with 
  ## the only-intercept model ("Null"). This test verifies if the explained variance is higher
  ## than the the unexplained variance.

#Note: Ho: The model fits the data. Therefore, we want to not reject the null hypothesis (pvalue > 0.05).

# Calculate the Type III test.
Anova(model1, type = "III", test = "Wald")

  ## Note: Type III tests examine the significance of each partial effect. 
  ## Thus, it considers the significance of an effect with all the other effects in the model. The Chisq tests
  ## the significance of the effect added to the model by having all of the other effects.


# Let us correct the standard errors with an overdispersed poisson 
 
 model2 = glm(ACCIDENT ~ as.factor(STATE) + AADT2 + MEDIAN + DRIVE + offset(log(AADT1)), family = quasipoisson(link = "log"), data = df, method = "glm.fit")
 
 summary(model2)

# Calculate the pseudo-Rsquare and perform an Omnibus test 
 
 nagelkerke(model2)

# Calculate the Type III test. 
 
 Anova(model2, type = "III", test = "Wald") 
 
 # Note: The estimates are the same, but the standard errors have increased because they are 
 # adjusted by the scale parameter

# Let us try the negative binomial distribution
 
 model3 = glm.nb(ACCIDENT ~ as.factor(STATE) + AADT2 + MEDIAN + DRIVE + offset(log(AADT1)), data = df)
 
 summary(model3)

# Calculate the pseudo-Rsquare and perform an Omnibus test   
 Anova(model3, type = "III", test = "Wald") 

# Calculate the Type III test.  
 nagelkerke(model3)

# Calculate elasticities 
 
 
# Compare models:
 
## Calculate the Akaike’s Information Criteria (AIC) and the Bayesian Information Criteria (BIC) 
AIC(model1, model2, model3)
BIC(model1, model2, model3) 

  ## Note: AIC and BIC evaluates the quality of a finite set of models.
  
  ## Note: AIC and BIC consider the maximum likelihood and the number of parameters in assessing the quality of the models.
  ## Nonetheless, the diference between both methods is that the BIC takes into account the number of observations of dataset. 
  
  ## Note: The smaller the values of AIC and BIC, the better the model 