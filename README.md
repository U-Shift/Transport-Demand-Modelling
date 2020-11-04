Multiple Linear Regression
================

**INSTITUTO SUPERIOR TECNICO, UNIVERSITY OF LISBON**  
**TRANSPORT DEMAND MODELLING COURSE, PROF. FILIPE MOURA**

## Chicago example exercise

Trip production of 57 Traffic Assignment Zones of Chicago in 1960’s

> Your task: Estimate a linear regression model that predicts trips per
> occupied dwelling unit.

### Variables:

  - `TODU`: Motorized Trips (private car or Public Transportation) per
    occupied dwelling unit;
  - ACO: Average car ownership (cars per dwelling);
  - AHS: Average household size;
  - SRI: Social Rank Index:  
    1\. proportion of blue-collar workers (e.g., construction,
    mining);  
    2\. proportion of people with age higher than 25 years that have
    completed at least 8 year of education;  
    **Note:** The SRI has its maximum value when there are no
    blue-collar workers and all adults have education of at least 8
    years.

> UI: Urbanization Index: 1. fertility rate, defined as the ratio of
> children under 5 years of age to the female population of childbearing
> age;  
> 2\. female labor force participation rate, meaning the % of women who
> are in the labor force;  
> 3\. % of single family units to total dwelling units.

``` 
  ## The degree of urbanization index would be increased by
    ### a) lower fertility rate,
    ### b) higher female labor force participation rate, and
    ### c) higher proportion of single dwelling units.

  ## Note: High values for this index imply less attachment to the home
```

SI:Segregation Index \#\# It measures the proportion of an area to which
minority groups \#\# (e.g: non-whites, foreign-born, Eastern Europeans)
live in isolation.

``` 
  ## Note: High values for this index imply that those communities are less
  ## prone to leaving their living areas and as such to having lower
  ## levels of mobility.
```

### Import Libraries

Let’s begin\!

For the first time, you will need to install some of the packages. Step
by step: 1. Go to Packages on the lower right display window and click
install 2. write the library you want to install and click “install” Or…
`install.packages("readxl","tidyverse")` etc…

``` r
library(readxl) #Library used to import excel files
library(tidyverse) # Library used in data science to perform exploratory data analysis
library(skimr) # Library used for providing a summary of the data
library(DataExplorer) # Library used in data science to perform exploratory data analysis
library(corrplot) # Library used for correlation plots
library(car) # Library used for testing autocorrelation (Durbin Watson)
library(olsrr) # Library used for testing multicollinearity (VIF, TOL, etc.)
```

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](README_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
