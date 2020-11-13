## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE, paged.print=TRUE--------------------------------------------------------------------
library(readxl) #Library used to import excel files
library(tidyverse) # Library used in data science to perform exploratory data analysis
library(skimr) # Library used for providing a summary of the data
library(DataExplorer) # Library used in data science to perform exploratory data analysis
library(corrplot) # Library used for correlation plots
library(car) # Library used for testing autocorrelation (Durbin Watson)
library(olsrr) # Library used for testing multicollinearity (VIF, TOL, etc.)


## ----------------------------------------------------------------------------------------------------------------------
dataset <- read_excel("Data/TDM_Class3_MLR_Chicago_Example.xls") 


## ----------------------------------------------------------------------------------------------------------------------
str(dataset)


## ----paged.print=TRUE--------------------------------------------------------------------------------------------------
head(dataset, 10)


## ----echo=TRUE, eval=FALSE---------------------------------------------------------------------------------------------
## #este apenas mostra o codigo, o seguinte apenas mostra os outputs
## typeof(dataset)
## class(dataset)


## ----echo=FALSE--------------------------------------------------------------------------------------------------------
typeof(dataset)
class(dataset)


## ----------------------------------------------------------------------------------------------------------------------
df <- data.frame(dataset)


## ----------------------------------------------------------------------------------------------------------------------
str(dataset)
str(df)


## ----------------------------------------------------------------------------------------------------------------------
head(df, 10)


## ----------------------------------------------------------------------------------------------------------------------
skim(df)


## ----------------------------------------------------------------------------------------------------------------------
plot_missing(df)

