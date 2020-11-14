# INSTITUTO SUPERIOR TECNICO, UNIVERSITY OF LISBON
# TRANSPORT DEMAND MODELLING COURSE, PROF. FILIPE MOURA

# EXPLORATORY FACTOR ANALYSIS (FA) 

# EXAMPLE EXERCISE: 
# "Residential location satisfaction in the Lisbon metropolitan area"

# Reference: Martínez, L. G., de Abreu e Silva, J., & Viegas, J. M. (2010). 
# Assessment of residential location satisfaction in the Lisbon metropolitan 
# area, TRB (No. 10-1161).

## The aim of this study was to examine the perception of households
## towards their residential location considering several land use and
## accessibility factors as well as household socioeconomic and attitudinal
## characteristics.


# Variables: 

## DWELCLAS: Classification of the dwelling;
## INCOME: Income of the household;
## CHILD13: Number of children under 13 years old;
## H18: Number of household members above 18 years old;
## HEMPLOY: Number of household members employed;
## HSIZE: Household size;
## IAGE: Age of the respondent;
## ISEX: Sex of the respondent;
## NCARS: Number of cars in the household;
## AREA: Area of the dwelling;
## BEDROOM: Number of bedrooms in the dwelling;
## PARK: Number of parking spaces in the dwelling; 
## BEDSIZE: BEDROOM/HSIZE;
## PARKSIZE: PARK/NCARS;
## RAGE10: 1 if Dwelling age <= 10;
## TCBD: Private car distance in time to CBD;
## DISTTC: Euclidean distance to heavy public transport system stops;
## TWCBD: Private car distance in time of workplace to CBD;
## TDWWK: Private car distance in time of dwelling to work place;
## HEADH: 1 if Head of the Household;
## POPDENS: Population density per hectare;
## EQUINDEX: Number of undergraduate students/Population over 20 years old (500m)



# Rules of thumb: 
# At least 10 variables
# n < 50 (Unacceptable); n > 200 (recommended)
# Recommended to use continuous variables. If your data contains categorical variables, 
# it is recommended to transform them to dummy variables. 

# Assumptions: 
## Normality; 
## linearity; 
## Homogeneity;
## Homoscedasticity (some multicollinearity is desirable);
## Correlations between variables < 0.3 (not appropriate to use Factor Analysis)


# Let's start!

# Import Libraries

# Library used to read SPSS files
library(foreign)
# Library used for factor analysis
library(nFactors)
# Library used in data science to perform exploratory data analysis
library(tidyverse) 
# Summary of the dataset
library(summarytools)
# Library used for factor analysis
library(psych)
# Library used for factor analysis
library(GPArotation)


# Set working directory

setwd("G:/Meu Drive/TDM - Lecture R/Factor Analysis")


# Import dataset

data <- read.spss("example_fact.sav")

# Take a look at the main characteristics of the dataset
str(data)

# Transform dataset into dataframe

df <- data.frame(data)

#Check summary statistics of variables

descriptive_stats <- dfSummary(df, style='grid', plain.ascii = FALSE, graph.col = TRUE)

view(descriptive_stats)

summary(df)

## Note: I used other functions for summary statistics in last exercise.

#Take a look at the dataset
head(df,10)
summary(df)

# Evaluating the assumptions for factoral analysis: 

# Let's run a random regression model in order to evaluate some assumptions

random = rchisq(nrow(df), 32)
fake = lm(random ~ ., data = df)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

## Normality
hist(standardized)

## Linearity 
qqnorm(standardized)
abline(0,1)

## Homogeneity 
plot(fitted, standardized)
abline(0,0)
abline(v=0)

# Examine correlations
corr_matrix <- cor(df, method = "pearson")
corr_matrix

# Check for correlation adequacy (pattern between variables) - Bartlett's Test
cortest.bartlett(corr_matrix, n = nrow(df))

# Check for sampling adequacy - KMO test
KMO(corr_matrix)

#Note: We want at least 0.7 of the overall Mean Sample Adequacy (MSA)

# Determine the number of factors to extract

  ## 1. Paralel Analysis
num_factors = fa.parallel(df, fm = "ml", fa = "fa")

    ## fm = factor math; ml = maximum likelihood; fa = factor analysis

  ## 2. Kaiser Criterion - How many factors with eigenvalue > 1
sum(num_factors$fa.values > 1)

  ## 3. Principal Component Analysis (PCA)

    df_pca <- princomp(df, cor=TRUE)
    
    ### Note: cor = TRUE, standardizes your dataset
    ### print variance that explains the components
    summary(df_pca) 
    ### Principal components loadings
    loadings(df_pca) 
    ### Scree Plot
    plot(df_pca,type="lines") 
    fit$scores # the principal components
    biplot(df_pca)
    

# EXPLORATORY FACTOR ANALYSIS

## Model 1: No rotation

df_factor <- factanal(df, factors = 4, rotation = "none", scores=c("regression"), fm = "ml")


## Model 2: Rotation Varimax

df_factor_var <- factanal(df, factors = 4, rotation = "varimax", scores=c("regression"), fm = "ml")


## Model 3: Rotation Oblimin

df_factor_obl <- factanal(df, factors = 4, rotation = "oblimin", scores=c("regression"), fm = "ml")



# Let's print out the results of df_factor_obl, and take a look. 

print(df_factor, digits=2, cutoff=.3, sort=TRUE)

## Note: We used a cutoff of 0.3 due to the sample size is higher than 350 obs.

## Note: Variability contained in the factors = Communality + Uniqueness

## Note: Varimax assigns orthogonal rotation, 
## and oblimin assigns oblique rotation.


#Plot factor 1 against factor 2, and compare the results of different rotations

## No Rotation
plot(df_factor$loadings[,1], 
     df_factor$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")
abline(h = 0, v = 0)
load <- df_factor$loadings[,1:2]
text(text(load,labels=names(df),cex=.7, col="blue"))
abline(h = 0, v = 0)

## Varimax rotation
plot(df_factor_var$loadings[,1], 
     df_factor_var$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")
     abline(h = 0, v = 0)
#Note: If you also want to put the variable label here, just take out the #.
#load <- df_factor_var$loadings[,1:2]
#text(text(load,labels=names(df),cex=.7, col="red"))


# Oblimin Rotation
plot(df_factor_obl$loadings[,1], 
     df_factor_obl$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Oblimin rotation")
abline(h = 0, v = 0)
#load <- df_factor_obl$loadings[,1:2]
#text(text(load,labels=names(df),cex=.7, col="yellow"))
#abline(h = 0, v = 0)




