Exploratory Factor Analysis
================

## EXAMPLE EXERCISE:

### “Residential location satisfaction in the Lisbon metropolitan area”

The aim of this study was to examine the perception of households
towards their residential location considering several land use and
accessibility factors as well as household socioeconomic and attitudinal
characteristics.

*Reference:* **Martinez, L. G., de Abreu e Silva, J., & Viegas, J. M.
(2010). Assessment of residential location satisfaction in the Lisbon
metropolitan area, TRB (No. 10-1161).**

> Your task: Analyse the data and create meaningful latent factors.

### Variables:

  - `DWELCLAS`: Classification of the dwelling;
  - `INCOME`: Income of the household;
  - `CHILD13`: Number of children under 13 years old;
  - `H18`: Number of household members above 18 years old;
  - `HEMPLOY`: Number of household members employed;
  - `HSIZE`: Household size;
  - `IAGE`: Age of the respondent;
  - `ISEX`: Sex of the respondent;
  - `NCARS`: Number of cars in the household;
  - `AREA`: Area of the dwelling;
  - `BEDROOM`: Number of bedrooms in the dwelling;
  - `PARK`: Number of parking spaces in the dwelling;
  - `BEDSIZE`: BEDROOM/HSIZE;
  - `PARKSIZE`: PARK/NCARS;
  - `RAGE10`: 1 if Dwelling age \<= 10;
  - `TCBD`: Private car distance in time to CBD;
  - `DISTTC`: Euclidean distance to heavy public transport system stops;
  - `TWCBD`: Private car distance in time of workplace to CBD;
  - `TDWWK`: Private car distance in time of dwelling to work place;
  - `HEADH`: 1 if Head of the Household;
  - `POPDENS`: Population density per hectare;
  - `EQUINDEX`: Number of undergraduate students/Population over 20
    years old (500m)

### Rules of thumb:

  - At least 10 variables
  - n \< 50 (Unacceptable); n \> 200 (recommended)
  - It is recommended to use continuous variables. If your data contains
    categorical variables, you should transform them to dummy variables.

### Assumptions:

  - Normality;
  - linearity;
  - Homogeneity;
  - Homoscedasticity (some multicollinearity is desirable);
  - Correlations between variables \< 0.3 (not appropriate to use Factor
    Analysis)

#### Import Libraries

Let’s start\!

``` r
library(foreign) # Library used to read SPSS files
library(nFactors) # Library used for factor analysis
library(tidyverse) # Library used in data science to perform exploratory data analysis
library(summarytools) # Library used for checking the summary of the dataset
library(psych) # Library used for factor analysis
library(GPArotation) # Library used for factor analysis
```

#### Set working directory

``` r
setwd("G:/O meu disco/TDM - Lecture R/TDM github/Transport-Demand-Modelling/")
```

#### Import dataset

``` r
data <- read.spss("Data/example_fact.sav")
```

#### Take a look at the main characteristics of the dataset

``` r
str(data)
```

    ## List of 32
    ##  $ RespondentID: num [1:470] 7.99e+08 7.98e+08 7.98e+08 7.98e+08 7.98e+08 ...
    ##  $ DWELCLAS    : num [1:470] 5 6 6 5 6 6 4 2 6 5 ...
    ##  $ INCOME      : num [1:470] 7500 4750 4750 7500 2750 1500 12500 1500 1500 1500 ...
    ##  $ CHILD13     : num [1:470] 1 0 2 0 1 0 0 0 0 0 ...
    ##  $ H18         : num [1:470] 2 1 2 3 1 3 3 4 1 1 ...
    ##  $ HEMPLOY     : num [1:470] 2 1 2 2 1 2 0 2 1 1 ...
    ##  $ HSIZE       : num [1:470] 3 1 4 4 2 3 3 4 1 1 ...
    ##  $ AVADUAGE    : num [1:470] 32 31 41.5 44.7 33 ...
    ##  $ IAGE        : num [1:470] 32 31 42 52 33 47 62 21 34 25 ...
    ##  $ ISEX        : num [1:470] 1 1 0 1 0 1 1 0 0 0 ...
    ##  $ NCARS       : num [1:470] 2 1 2 3 1 1 2 3 1 1 ...
    ##  $ AREA        : num [1:470] 100 90 220 120 90 100 178 180 80 50 ...
    ##  $ BEDROOM     : num [1:470] 2 2 4 3 2 2 5 3 2 1 ...
    ##  $ PARK        : num [1:470] 1 1 2 0 0 0 2 0 0 1 ...
    ##  $ BEDSIZE     : num [1:470] 0.667 2 1 0.75 1 ...
    ##  $ PARKSIZE    : num [1:470] 0.5 1 1 0 0 0 1 0 0 1 ...
    ##  $ RAGE10      : num [1:470] 1 0 1 0 0 0 0 0 1 1 ...
    ##  $ TCBD        : num [1:470] 36.79 15.47 24.1 28.72 7.28 ...
    ##  $ DISTHTC     : num [1:470] 629 551 548 2351 698 ...
    ##  $ TWCBD       : num [1:470] 10 15.5 12.71 3.17 5.36 ...
    ##  $ TDWWK       : num [1:470] 31.1 0 20.4 32.9 13 ...
    ##  $ HEADH       : num [1:470] 1 1 1 1 1 1 1 0 1 1 ...
    ##  $ POPDENS     : num [1:470] 85.7 146.4 106.6 36.8 181.6 ...
    ##  $ EDUINDEX    : num [1:470] 0.0641 0.2672 0.1 0.0867 0.1309 ...
    ##  $ GRAVCPC     : num [1:470] 0.249 0.329 0.24 0.273 0.285 ...
    ##  $ GRAVCPT     : num [1:470] 0.249 0.31 0.29 0.249 0.291 ...
    ##  $ GRAVPCPT    : num [1:470] 1 1.062 0.826 1.099 0.98 ...
    ##  $ NSTRTC      : num [1:470] 38 34 33 6 31 45 12 6 4 22 ...
    ##  $ DISTHW      : num [1:470] 2036 748 2279 1196 3507 ...
    ##  $ DIVIDX      : num [1:470] 0.323 0.348 0.324 0.327 0.355 ...
    ##  $ ACTDENS     : num [1:470] 0.672 2.486 1.625 1.766 11.325 ...
    ##  $ DISTCBD     : num [1:470] 9776 3524 11036 6257 1265 ...
    ##  - attr(*, "label.table")=List of 32
    ##   ..$ RespondentID: NULL
    ##   ..$ DWELCLAS    : NULL
    ##   ..$ INCOME      : NULL
    ##   ..$ CHILD13     : NULL
    ##   ..$ H18         : NULL
    ##   ..$ HEMPLOY     : NULL
    ##   ..$ HSIZE       : NULL
    ##   ..$ AVADUAGE    : NULL
    ##   ..$ IAGE        : NULL
    ##   ..$ ISEX        : NULL
    ##   ..$ NCARS       : NULL
    ##   ..$ AREA        : NULL
    ##   ..$ BEDROOM     : NULL
    ##   ..$ PARK        : NULL
    ##   ..$ BEDSIZE     : NULL
    ##   ..$ PARKSIZE    : NULL
    ##   ..$ RAGE10      : NULL
    ##   ..$ TCBD        : NULL
    ##   ..$ DISTHTC     : NULL
    ##   ..$ TWCBD       : NULL
    ##   ..$ TDWWK       : NULL
    ##   ..$ HEADH       : NULL
    ##   ..$ POPDENS     : NULL
    ##   ..$ EDUINDEX    : NULL
    ##   ..$ GRAVCPC     : NULL
    ##   ..$ GRAVCPT     : NULL
    ##   ..$ GRAVPCPT    : NULL
    ##   ..$ NSTRTC      : NULL
    ##   ..$ DISTHW      : NULL
    ##   ..$ DIVIDX      : NULL
    ##   ..$ ACTDENS     : NULL
    ##   ..$ DISTCBD     : NULL
    ##  - attr(*, "codepage")= int 1252
    ##  - attr(*, "variable.labels")= Named chr(0) 
    ##   ..- attr(*, "names")= chr(0)

# Transform dataset into dataframe

df \<- data.frame(data)

\#Check summary statistics of variables

descriptive\_stats \<- dfSummary(df, style=‘grid’, plain.ascii = FALSE,
graph.col = TRUE)

view(descriptive\_stats)

summary(df)

## Note: I used other functions for summary statistics in last exercise.

\#Take a look at the dataset head(df,10) summary(df)

# Make ID as row names or case number

df\<-data.frame(df, row.names = 1)

# Evaluating the assumptions for factoral analysis:

# Let’s run a random regression model in order to evaluate some assumptions

random = rchisq(nrow(df), 32) fake = lm(random \~ ., data = df)
standardized = rstudent(fake) fitted = scale(fake$fitted.values)

## Normality

hist(standardized)

## Linearity

qqnorm(standardized) abline(0,1)

## Homogeneity

plot(fitted, standardized) abline(0,0) abline(v=0)

# Examine correlations

corr\_matrix \<- cor(df, method = “pearson”) corr\_matrix

# Check for correlation adequacy (pattern between variables) - Bartlett’s Test

cortest.bartlett(corr\_matrix, n = nrow(df))

# Check for sampling adequacy - KMO test

KMO(corr\_matrix)

\#Note: We want at least 0.7 of the overall Mean Sample Adequacy (MSA)

# Determine the number of factors to extract

\#\# 1. Paralel Analysis num\_factors = fa.parallel(df, fm = “ml”, fa =
“fa”)

    ## fm = factor math; ml = maximum likelihood; fa = factor analysis

\#\# 2. Kaiser Criterion - How many factors with eigenvalue \> 1
sum(num\_factors$fa.values \> 1)

\#\# 3. Principal Component Analysis (PCA)

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

df\_factor \<- factanal(df, factors = 4, rotation = “none”,
scores=c(“regression”), fm = “ml”)

## Model 2: Rotation Varimax

df\_factor\_var \<- factanal(df, factors = 4, rotation = “varimax”,
scores=c(“regression”), fm = “ml”)

## Model 3: Rotation Oblimin

df\_factor\_obl \<- factanal(df, factors = 4, rotation = “oblimin”,
scores=c(“regression”), fm = “ml”)

# Let’s print out the results of df\_factor\_obl, and take a look.

print(df\_factor, digits=2, cutoff=0.3, sort=TRUE)

## Note: We used a cutoff of 0.3 due to the sample size is higher than 350 obs.

## Note: Variability contained in the factors = Communality + Uniqueness

## Note: Varimax assigns orthogonal rotation,

## and oblimin assigns oblique rotation.

\#Plot factor 1 against factor 2, and compare the results of different
rotations

## No Rotation

plot(df\_factor\(loadings[,1],  df_factor\)loadings\[,2\], xlab =
“Factor 1”, ylab = “Factor 2”, ylim = c(-1,1), xlim = c(-1,1), main =
“No rotation”) abline(h = 0, v = 0) load \<-
df\_factor$loadings\[,1:2\] text(text(load,labels=names(df),cex=.7,
col=“blue”)) abline(h = 0, v = 0)

## Varimax rotation

plot(df\_factor\_var\(loadings[,1],  df_factor_var\)loadings\[,2\], xlab
= “Factor 1”, ylab = “Factor 2”, ylim = c(-1,1), xlim = c(-1,1), main =
“Varimax rotation”) abline(h = 0, v = 0) \#Note: If you also want to
put the variable label here, just take out the \#. \#load \<-
df\_factor\_var$loadings\[,1:2\]
\#text(text(load,labels=names(df),cex=.7, col=“red”))

# Oblimin Rotation

plot(df\_factor\_obl\(loadings[,1],  df_factor_obl\)loadings\[,2\], xlab
= “Factor 1”, ylab = “Factor 2”, ylim = c(-1,1), xlim = c(-1,1), main =
“Oblimin rotation”) abline(h = 0, v = 0) \#load \<-
df\_factor\_obl$loadings\[,1:2\]
\#text(text(load,labels=names(df),cex=.7, col=“yellow”)) \#abline(h = 0,
v = 0)

knitr::spin(hair = “Factor\_Analysis\_TDMLecture.R”)
