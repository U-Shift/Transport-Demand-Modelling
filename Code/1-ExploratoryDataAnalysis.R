
#' We will use the example of the following Multiple linear regression chapter to perform an EDA.
#' 
#' #### Example exercise: Trip production of 57 Traffic Assignment Zones of Chicago in 1960's.
#' 
#' **Your task**: Explore and analyze the dataset before going to the Multiple linear regression chapter.  
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
#' ## Let's begin with R!
#' 
#' Working directory
#  Step by step:
  
#  click on “Session”;
#  Click on “Set working directory”;
#  Click on “Choose directory” and select the folder that contains the dataset.
#  Or you could just run

setwd("G:/O meu disco/TDM - Lecture R/TDM github/Transport-Demand-Modelling") #for instance

#' 
#' 
#' ##### Import Libraries
#' For the first time, you will need to install some of the packages. 
#' Step by step: 
#' 
#'   1. Go to Packages on the lower right display window and click install
#'   2. Write the library you want to install and click "install"
#'   
#' Or... `install.packages("readxl","tidyverse")` etc...
#' 
#' Depending on the version of your R, `DataExplorer` may need to be installed from source, such as
#' 
#' ```r
#' if (!require(devtools)) install.packages("devtools")
#' devtools::install_github("boxuancui/DataExplorer")
#' ```
#' 
#' ##### Import Libraries
#' Now, import these libraries:
library(readxl) #Library used to import excel files
library(tidyverse) # Pack of most used libraries
library(skimr) # Library used for providing a summary of the data
library(DataExplorer) # Library used in data science to perform exploratory data analysis
library(corrplot) # Library used for correlation plots

#' 
#' ## Get to know your data
#' ##### Import dataset
#' 
dataset <- read_excel("Data/TDM_Class3_MLR_Chicago_Example.xls") 

#' 
#' ##### Check the structure of the dataset 
str(dataset)

#' 
#' ##### Take a first look at the dataset
#' 
head(dataset, 10)

#' 
#' ##### Check the type and class of the dataset
#' 
typeof(dataset)
class(dataset)

#' 
#' ##### Transform the dataset into a dataframe
#' 
df <- data.frame(dataset)

#' 
#' ##### Compare the structure of the `dataset` with `df`
#' 
str(dataset)
str(df)

class(dataset)
class(df)

#' 
#' > **Note:** The dataframe function transforms columns into variables and rows into observations. 
#' 
#' ##### Take a look at the dataframe
#' 
head(df, 10)

#' 
#' ##### Show summary statistics
#' 
skim(df)

#' 
#' ### Deal with missing data
#' 
#' Is there missing data? How many?
table(is.na(df))

#' 
#' > **Note:** We do not have any missing data in the dataset. 
#' 
#' ##### Dataset with missing data
#' For the sake of the example, I deleted some values in the dataset and created a copy file. Import the file and take a look at some functions that you can use to treat missing data.
#' 
df_missing <- read_excel("Data/TDM_Class3_MLR_Chicago_Example_md.xls")
df_missing <- data.frame(df_missing)

#' 
#' How many missing data?
table(is.na(df_missing))

#' 
#' Plot the percentage of missing data
plot_missing(df_missing)

#' 
#' #### Treat missing data
#' 
#' * **Listwise deletion**. Delete observation (row) with incomplete information. 
df_missingListwise = na.omit(df_missing) #removes all rows with at least one NA in any variable

#' 
#' * **Pairwise deletion**. Delete only the row of missing value if the variable is used.   
df_missingPairwise = df_missing[!is.na(df_missing$ACO),] #removes all rows with NA in ACO variable

#' 
#' > **Note:** Listwise deletion may lose a lot of information, while pairwise deletion considers diferent sizes of variables in the analysis, which may be a problem. Choosing one method or the other depends on the number of missing data, sample size and characteristics of your data.
#' 
#' * **Replace missing value with mean or median**
#' 
df_missing$ACO[is.na(df_missing$ACO)] <- mean(df_missing$ACO, na.rm = TRUE)
df_missing$ACO[is.na(df_missing$ACO)] <- median(df_missing$ACO, na.rm = TRUE)

#' 
#' > **Note**: Here are just some examples of how to treat missing data. Take a look at other methods such as the prediction model or K-nearest neighbor imputation. 
#' 
#' ### Detect outliers  
#' 
#' * Examine the boxplots
#'   
df_no_outliers <- df
boxplot(df_no_outliers)

#' 
#' * Take the out the outliers from the variable SI
#' 
outlier <- function(x){
  quant <- quantile(x, probs=c(0.25, 0.75))
  caps <- quantile(x, probs=c(0.05, 0.95))
  H <- 1.5* IQR(x, na.rm = TRUE)
  x[x < (quant[1] - H)] <- caps[1]
  x[x > (quant[2] + H)] <- caps[2]
  return(x)
}

df_no_outliers$SI = outlier(df_no_outliers$SI)

#' 
#' * Take a look again at the boxplots
#' 
boxplot(df_no_outliers)

#' 
#' * Compare results of the dataset with and without the outliers  
#' *mean**
mean(df$SI)
mean(df_no_outliers$SI)

#' 
#' **median**
median(df$SI)
median(df_no_outliers$SI)

#' 
#' **variance**
var(df$SI)
var(df_no_outliers$SI)

#' 
#' 
#' > **Note:** There are many methods to treat outliers. This is just one of them. Try using other methods and evaluate the difference. In the next chapter we will demonstrate other methods of detecting outliers such as the Cook distance and QQ plot.    
#' 
#' 
#' ### Histograms
#' Plot histograms of all the continuous variables
#' 
plot_histogram(df, ncol = 3) #with 3 columns

#' 
#' > **Note**: Take a special look at TODU, and see if the variable looks like a normal distribution.  
#' 
#' ##### How do the other variables behave regarding TODU?
#' Plot boxplots of each independent variable with TODU
#' 
plot_boxplot(df, by = "TODU", ncol = 3)

#' 
#' > **Note**: If you increase the average car ownership (ACO) it will tend to increase the number of trips per dwelling unit (TODU). This makes sense. Try analyzing the other relations and check if it is coherent. 
#' 
#' ### Correlations
#' Plot correlation heatmaps
res <- cor.mtest(df, conf.level = .95) #store the results so you can call the p-value at the corrplot

corrplot(cor(df), p.mat = res$p, method = "number", type = "upper", order="hclust", sig.level = 0.05)

#' 
#' > **Note:** The pairwise correlations that are crossed are statistically insignificant. The null hypothesis is that correlation is zero. This means that the correlations are only significant when you reject the null hypothesis (pvalue < 0.05).   
#' See `?corrplot` for more options.  
#' Try putting into method "color" or "circle", and see the diference.  
#' 
#' Therefore, take a look at this example and check the _pvalue_ of a crossed pair correlation: 
#' 
cor.test(df$AHS, df$SI)

#' The default for `cor.test` is Pearson, two-sided, with a 95% confident level. Check `?cor.test` for more options.  
#' 
#' 
#' Now that you have done some descriptive analysis of the data, go to the next chapter. There you will learn how to perform a [Multiple Linear Regression model](2-MultipleLinearRegression.md)!
#' 