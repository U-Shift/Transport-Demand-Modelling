#' 
#' #### Example Exercise: Grunfeld Investment data  
#' This data consists of 10 large US manufacturing firms from 1935 to 1954.
#' 
#' **Your Task:** Analyze the many types of panel models.
#' 
#' This code was based on the paper: Croissant, Y., Milo, G.(2008). [_Panel Data Econometrics in R: The plm Package_](https://www.jstatsoft.org/index.php/jss/article/view/v027i02/v27i02.pdf), Journal of Statistical Software, 27(2).
#' 
#' ## Data
#' #### Variables:  
#' 
#' * `invest`: Gross investment, defined as additions to plant and equipment plus maintenance and repairs in millions of dollars deflated by the implicit price deflator of producers' durable equipment (base 1947);  
#' * `value`: Market value of the firm, defined as the price of common shares at December 31 (base 1947);
#' * `capital`: Stock of plant and equipment, defined as the accumulated sum of net additions to plant and equipment deflated by the implicit price deflator for producers' durable equipment (base 1947);
#' * `firm`: American manufacturing firms;
#' * `year`: Year of data; 
#' * `firmcod`: Numeric code that identifies each firm.
#' 
#' ## Startup
#' #### Import libraries
library(readxl) #read excel files
library(skimr) #summary statistics
library(foreign) #panel data models
library(plm) # Lagrange multiplier test and panel models

#' 
#' ##### Import dataset

data <- read_excel("Data/Grunfeld_data.xlsx")
df <- data.frame(data)

#' 
#' ##### Take a first look at your data

#' #### Prepare your data
#' ##### Take out the "firmcod" variale from the dataset

df$firmcod = NULL #another way or removing a variable

#' 
#' ##### Factor categorical variables  
#' `firm` is a categorical nominal variable, and should be treated as so in the modeling processes. And for this example `year` should also be considered as a categorical ordinal variable, instead of a continuous one.

df$firm = factor(df$firm)
df$year = factor(df$year, ordered = T)

#' Take a look at the summary of your data. See the differences regarding the categorical ones?

summary(df)


#' 
#' ## Ordinary least square model  
#' First run an Ordinary least square model without the `firm` variable. Compare the results from this model to the many panel data models.

mlr = lm(invest ~ value + capital, data = df)
summary(mlr)

#' 
#' ## Panel Data Models
#' 
#' Panel data models use __one way__ and __two way__ component models to overcome heterogeneity, correlation in the disturbance terms, and heteroscedasticity.
#' 
#' * **One way error component model:** variable-intercept models across individuals **or** time;
#' * **Two way error component model:** variable-intercept models across individuals **and** time. 
#' 
#' Modelling Specifications: 
#' 
#' * **With fixed-effects:** effects that are in the sample. Fixed-effects explore the causes of change within a person or entity (In this example the entity is the _firms_);
#' 
#' * **With random-effects:** effect randomly drawn from a population. The random effects model is an appropriate specification if we are drawing _n_ individuals randomly from a large population.
#' 
#' 
#' You can also try other types of model estimation: 
#' 

#' 
#' See `?plm` for more options, regarding the effects and instrumental variable transformation types.
#' 
#' 
#' ### One way
#' ##### One way fixed effects model

fixed = plm(
  invest ~ value + capital,
  data = df,
  index = c("firm", "year"), #panel settings
  model = "within" #fixed effects option
)
summary(fixed)

#' 
#' ##### One way random effects model

random = plm(
  invest ~ value + capital,
  data = df,
  index = c("firm", "year"),
  model = "random" #random effects option
)
summary(random)

#' 
#' #### Haussman test  
#' Use the Hausman test to evaluate when to use fixed or random effects

phtest(random, fixed)

#' 
#' > **Note:** The null hypothesis is that random effect model is more appropriate than the fixed effect model. 
#' 
#' ### Two-way  
#' ##### Two-way Fixed effects model

fixed_tw <-
  plm(
    invest ~ value + capital,
    data = df,
    effect = "twoways", #effects option
    model = "within", #fixed
    index = c("firm", "year") #panel settings
  )
summary(fixed_tw)

#' 
#' ##### Two-way Random effects model

random_tw <-
  plm(
    invest ~ value + capital,
    data = df,
    effect = "twoways",
    model = "random",
    index = c("firm", "year"),
    random.method = "amemiya"
  )
summary(random_tw)

#' 
#' #### Lagrange Multiplier Test  
#' The Lagrange multiplier statistic, is used to test the null hypothesis that there are no group effects in the Random Effects model.   
#' Large values of the Lagrange Multiplier indicate that effects model is more suitable than the classical model with no common effects.

plmtest(random_tw)


#' >**Note:** Large values of H indicate that the fixed effects model is prefered over the random effects model. While, A large value of the LM statistic in the presence of a small H statistic indicate that the random effects model is more suitable.
