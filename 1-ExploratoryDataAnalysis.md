Exploratory Data Analysis
================

#### Example exercise: Trip production of 57 Traffic Assignment Zones of Chicago in the 1960’s.

**Your task**: Explore and analyse the dataset before going to the
Multiple linear regression chapter.

#### Variables:

  - `TODU`: Motorized Trips (private car or Public Transportation) per
    occupied dwelling unit;

  - `ACO`: Average car ownership (cars per dwelling);

  - `AHS`: Average household size;

  - `SRI`: Social Rank Index:  
    1\. proportion of blue-collar workers (e.g., construction,
    mining);  
    2\. proportion of people with age higher than 25 years that have
    completed at least 8 year of education; (***Note:** The SRI has its
    maximum value when there are no blue-collar workers and all adults
    have education of at least 8 years*)

  - `UI`: Urbanization Index:  
    1\. fertility rate, defined as the ratio of children under 5 years
    of age to the female population of childbearing age;  
    2\. female labor force participation rate, meaning the % of women
    who are in the labor force;  
    3\. % of single family units to total dwelling units.
    
    The degree of urbanization index would be increased by a) lower
    fertility rate, b) higher female labor force participation rate, and
    c) higher proportion of single dwelling units. (***Note:** High
    values for this index imply less attachment to the home*)

  - `SI`:Segregation Index It measures the proportion of an area to
    which minority groups (e.g: non-whites, foreign-born, Eastern
    Europeans) live in isolation. (***Note:** High values for this index
    imply that those communities are less prone to leaving their living
    areas and as such to having lower levels of mobility*)

## Let’s begin with R\!

##### Import Libraries

For the first time, you will need to install some of the packages. Step
by step:

1.  Go to Packages on the lower right display window and click install
2.  Write the library you want to install and click “install”

Or… `install.packages("readxl","tidyverse")` etc…

Depending on the version of your R, `DataExplorer` may need to be
installed from source, such as

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("boxuancui/DataExplorer")
```

Now, import these libraries:

``` r
library(readxl) #Library used to import excel files
library(tidyverse) # Pack of most used libraries
library(skimr) # Library used for providing a summary of the data
library(DataExplorer) # Library used in data science to perform exploratory data analysis
library(corrplot) # Library used for correlation plots
```

## Get to know your data

##### Import dataset

``` r
dataset <- read_excel("Data/TDM_Class3_MLR_Chicago_Example.xls") 
```

##### Check the structure of the dataset

``` r
str(dataset)
```

    ## tibble [57 x 6] (S3: tbl_df/tbl/data.frame)
    ##  $ TODU: num [1:57] 3.18 3.89 3.98 4.16 3.6 4.1 4.36 4.87 5.85 4.97 ...
    ##  $ ACO : num [1:57] 0.59 0.57 0.61 0.61 0.63 0.66 0.71 0.77 0.84 0.74 ...
    ##  $ AHS : num [1:57] 3.26 3.13 3.02 3.14 3.75 3.24 2.77 2.74 3.02 2.84 ...
    ##  $ SI  : num [1:57] 21 21.6 12.6 17.6 35.3 ...
    ##  $ SRI : num [1:57] 28.3 20.9 26 28.5 27.2 ...
    ##  $ UI  : num [1:57] 60.1 65.7 63.2 66.2 58.4 ...

##### Take a first look at the dataset

``` r
head(dataset, 10)
```

    ## # A tibble: 10 x 6
    ##     TODU   ACO   AHS    SI   SRI    UI
    ##    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1  3.18 0.59   3.26 21.0   28.3  60.1
    ##  2  3.89 0.570  3.13 21.6   20.9  65.7
    ##  3  3.98 0.61   3.02 12.6   26.0  63.2
    ##  4  4.16 0.61   3.14 17.6   28.5  66.2
    ##  5  3.6  0.63   3.75 35.3   27.2  58.4
    ##  6  4.1  0.66   3.24 14.7   28.0  59.6
    ##  7  4.36 0.71   2.77 11.6   39.9  64.6
    ##  8  4.87 0.77   2.74 10.7   48.4  67.9
    ##  9  5.85 0.84   3.02  8.2   42.2  56.9
    ## 10  4.97 0.74   2.84  7.94  38.1  62.4

##### Check the type and class of the dataset

``` r
typeof(dataset)
```

    ## [1] "list"

``` r
class(dataset)
```

    ## [1] "tbl_df"     "tbl"        "data.frame"

##### Transform the dataset into a dataframe

``` r
df <- data.frame(dataset)
```

##### Compare the structure of the `dataset` with `df`

``` r
str(dataset)
```

    ## tibble [57 x 6] (S3: tbl_df/tbl/data.frame)
    ##  $ TODU: num [1:57] 3.18 3.89 3.98 4.16 3.6 4.1 4.36 4.87 5.85 4.97 ...
    ##  $ ACO : num [1:57] 0.59 0.57 0.61 0.61 0.63 0.66 0.71 0.77 0.84 0.74 ...
    ##  $ AHS : num [1:57] 3.26 3.13 3.02 3.14 3.75 3.24 2.77 2.74 3.02 2.84 ...
    ##  $ SI  : num [1:57] 21 21.6 12.6 17.6 35.3 ...
    ##  $ SRI : num [1:57] 28.3 20.9 26 28.5 27.2 ...
    ##  $ UI  : num [1:57] 60.1 65.7 63.2 66.2 58.4 ...

``` r
str(df)
```

    ## 'data.frame':    57 obs. of  6 variables:
    ##  $ TODU: num  3.18 3.89 3.98 4.16 3.6 4.1 4.36 4.87 5.85 4.97 ...
    ##  $ ACO : num  0.59 0.57 0.61 0.61 0.63 0.66 0.71 0.77 0.84 0.74 ...
    ##  $ AHS : num  3.26 3.13 3.02 3.14 3.75 3.24 2.77 2.74 3.02 2.84 ...
    ##  $ SI  : num  21 21.6 12.6 17.6 35.3 ...
    ##  $ SRI : num  28.3 20.9 26 28.5 27.2 ...
    ##  $ UI  : num  60.1 65.7 63.2 66.2 58.4 ...

> **Note:** The dataframe function transforms columns into variables and
> rows into observations.

##### Take a look at the dataframe

``` r
head(df, 10)
```

    ##    TODU  ACO  AHS    SI   SRI    UI
    ## 1  3.18 0.59 3.26 21.01 28.32 60.10
    ## 2  3.89 0.57 3.13 21.61 20.89 65.71
    ## 3  3.98 0.61 3.02 12.57 25.99 63.19
    ## 4  4.16 0.61 3.14 17.61 28.52 66.24
    ## 5  3.60 0.63 3.75 35.32 27.18 58.36
    ## 6  4.10 0.66 3.24 14.73 27.95 59.58
    ## 7  4.36 0.71 2.77 11.61 39.91 64.64
    ## 8  4.87 0.77 2.74 10.71 48.36 67.88
    ## 9  5.85 0.84 3.02  8.20 42.15 56.86
    ## 10 4.97 0.74 2.84  7.94 38.14 62.44

##### Show summary statistics

``` r
skim(df)
```

|                                                  |      |
| :----------------------------------------------- | :--- |
| Name                                             | df   |
| Number of rows                                   | 57   |
| Number of columns                                | 6    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| numeric                                          | 6    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |  mean |    sd |    p0 |   p25 |   p50 |   p75 |  p100 | hist  |
| :------------- | ---------: | -------------: | ----: | ----: | ----: | ----: | ----: | ----: | ----: | :---- |
| TODU           |          0 |              1 |  5.37 |  1.33 |  3.02 |  4.54 |  5.10 |  6.13 |  9.14 | ▃▇▅▃▁ |
| ACO            |          0 |              1 |  0.81 |  0.18 |  0.50 |  0.67 |  0.79 |  0.92 |  1.32 | ▆▇▇▃▁ |
| AHS            |          0 |              1 |  3.19 |  0.39 |  1.83 |  3.00 |  3.19 |  3.37 |  4.50 | ▁▂▇▂▁ |
| SI             |          0 |              1 | 13.07 | 12.19 |  2.17 |  6.82 |  9.86 | 15.08 | 62.53 | ▇▂▁▁▁ |
| SRI            |          0 |              1 | 49.56 | 15.84 | 20.89 | 38.14 | 49.37 | 60.85 | 87.38 | ▅▆▇▅▂ |
| UI             |          0 |              1 | 52.62 | 13.46 | 24.08 | 44.80 | 55.51 | 61.09 | 83.66 | ▃▅▇▅▁ |

### Deal with missing data

Is there missing data? How many?

``` r
table(is.na(df))
```

    ## 
    ## FALSE 
    ##   342

> **Note:** We do not have any missing data in the dataset.

##### Dataset with missing data

For the sake of the example, I deleted some values in the dataset and
created a copy file. Import the file and take a look at some functions
that you can use to treat missing data.

``` r
df_missing <- read_excel("Data/TDM_Class3_MLR_Chicago_Example_md.xls")
df_missing <- data.frame(df_missing)
```

How many missing data?

``` r
table(is.na(df_missing))
```

    ## 
    ## FALSE  TRUE 
    ##   337     5

Plot the percentage of missing data

``` r
plot_missing(df_missing)
```

![](RmdFiles/1-ExploratoryDataAnalysis/unnamed-chunk-13-1.png)<!-- -->

#### Treat missing data

  - **Listwise deletion**. Delete observation (row) with incomplete
    information.

<!-- end list -->

``` r
df_missingListwise = na.omit(df_missing) #removes all rows with at least one NA in any variable
```

  - **Pairwise deletion**. Delete only the row of missing value if the
    variable is used.

<!-- end list -->

``` r
df_missingPairwise = df_missing[!is.na(df_missing$ACO),] #removes all rows with NA in ACO variable
```

> **Note:** Listwise deletion may lose a lot of information, while
> pairwise deletion considers diferent sizes of variables in the
> analysis, which may be a problem. Choosing one method or the other
> depends on the number of missing data, sample size and characteristics
> of your data.

  - **Replace missing value with mean or median**

<!-- end list -->

``` r
df_missing$ACO[is.na(df_missing$ACO)] <- mean(df_missing$ACO, na.rm = TRUE)
df_missing$ACO[is.na(df_missing$ACO)] <- median(df_missing$ACO, na.rm = TRUE)
```

> **Note**: Here are just some examples of how to treat missing data.
> Take a look at other methods such as the prediction model or K-nearest
> neighbor imputation.

### Detect outliers

  - Examine the boxplots

<!-- end list -->

``` r
df_no_outliers <- df
boxplot(df_no_outliers)
```

![](RmdFiles/1-ExploratoryDataAnalysis/unnamed-chunk-17-1.png)<!-- -->

  - Take out the outliers from the variable SI

<!-- end list -->

``` r
outlier <- function(x){
  quant <- quantile(x, probs=c(0.25, 0.75))
  caps <- quantile(x, probs=c(0.05, 0.95))
  H <- 1.5* IQR(x, na.rm = TRUE)
  x[x < (quant[1] - H)] <- caps[1]
  x[x > (quant[2] + H)] <- caps[2]
  return(x)
}

df_no_outliers$SI = outlier(df_no_outliers$SI)
```

  - Take a look again at the boxplots

<!-- end list -->

``` r
boxplot(df_no_outliers)
```

![](RmdFiles/1-ExploratoryDataAnalysis/unnamed-chunk-19-1.png)<!-- -->

  - Compare results of the dataset with and without the outliers  
    **mean**

<!-- end list -->

``` r
mean(df$SI)
```

    ## [1] 13.07316

``` r
mean(df_no_outliers$SI)
```

    ## [1] 12.14681

**median**

``` r
median(df$SI)
```

    ## [1] 9.86

``` r
median(df_no_outliers$SI)
```

    ## [1] 9.86

**variance**

``` r
var(df$SI)
```

    ## [1] 148.5175

``` r
var(df_no_outliers$SI)
```

    ## [1] 80.62296

> **Note:** There are many methods to treat outliers. This is just one
> of them. Try using other methods and evaluate the difference. In the
> next chapter we will demonstrate other methods of detecting outliers
> such as the Cook distance and QQ plot.

### Histograms

Plot histograms of all the continuous variables

``` r
plot_histogram(df, ncol = 3) #with 3 columns
```

![](RmdFiles/1-ExploratoryDataAnalysis/unnamed-chunk-23-1.png)<!-- -->

> **Note**: Take a special look at TODU, and see if the variable looks
> like a normal distribution.

##### How do the other variables behave regarding TODU?

Plot boxplots of each independent variable with TODU

``` r
plot_boxplot(df, by = "TODU", ncol = 3)
```

![](RmdFiles/1-ExploratoryDataAnalysis/unnamed-chunk-24-1.png)<!-- -->

> **Note**: If you increase the average car ownership (ACO) it will tend
> to increase the number of trips per dwelling unit (TODU). This makes
> sense. Try analyzing the other relations and check if it is coherent.

### Correlations

Plot correlation heatmaps

``` r
res <- cor.mtest(df, conf.level = .95) #store the results so you can call the p-value at the corrplot

corrplot(cor(df), p.mat = res$p, method = "number", type = "upper", order="hclust", sig.level = 0.05)
```

![](RmdFiles/1-ExploratoryDataAnalysis/unnamed-chunk-25-1.png)<!-- -->

> **Note:** The pairwise correlations that are crossed are statistically
> insignificant. The null hypothesis is that the correlation is zero.
> This means that the correlations are only significant when you reject
> the null hypothesis (pvalue \< 0.05).  
> See `?corrplot` for more options.  
> Try putting into method “color” or “circle”, and see the diference.

Therefore, take a look at this example and check the *pvalue* of a
crossed pair correlation:

``` r
cor.test(df$AHS, df$SI)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$AHS and df$SI
    ## t = 0.63199, df = 55, p-value = 0.53
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1796317  0.3379997
    ## sample estimates:
    ##        cor 
    ## 0.08491026

The default for `cor.test` is Pearson, two-sided, with a 95% confident
level. Check `?cor.test` for more options.  
<!-- > **Note:** Correlation heatmaps only consider pairwise correlations and does not demonstrate multicollinearity.   -->

Now that you have done some descriptive analysis of the data, go to the
next chapter. There you will learn how to perform a [Multiple Linear
Regression model](2-MultipleLinearRegression.md)\!
