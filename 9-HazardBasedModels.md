Hazard-Based Duration Models
================

## Example: Work-to-home departure delay

A survey of 204 Seattle-area commuters was conducted to examine the
duration of time that commuters delay their work-to-home trips in an
effort to avoid peak period traffic congestion. Of the 204 commuters
surveyed, 96 indicated that they sometimes delayed their work-to-home
trip to avoid traffic congestion. These commuters provided their average
time delay. Thus, each commuter has a completed delay duration so that
neither left nor right censoring is present in the data.

> **Your task:** i) Plot the Kaplan-Meier estimate of the duration of
> time that commuters delay their work-to-home trips; ii) Determine the
> significant factors that affect the duration of commuters’ delay using
> a Cox model; iii) Examine the work-to-home departure delay using
> exponential, Weibull, and log-logistic proportional-hazards models.

#### Import Libraries

``` r
library(readxl)
library(skimr)
library(survival)
library(coxme)
library(survminer)
```

#### Set working directory

``` r
setwd("G:/O meu disco/TDM - Lecture R/TDM github/Transport-Demand-Modelling")
```

#### Import dataset, tranform in dataframe, and take a first look.

``` r
data.delay <- read_excel("Data/ExerciseHBDM.xlsx")

data.delay <- data.frame(data.delay)

head(data.delay)
```

    ##     id X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14 X15   X16  X17  X18  X19
    ## 1 1000  0  0  0  1  2  1  1  1  2   0   5   1  12   1 1.4 37070 1024 1295 5610
    ## 2 1001 30  1  1  1  2  1  2  0  2   0   5   0  14   1 1.8 24410 6430 3206 1997
    ## 3 1002  0  0  0  1  5  1  4  1  1   0   3   0   5   1 1.3 24410 6430 3206 1997
    ## 4 1003  0  0  0  1  5  1  4  1  1   0   2   0   5   1 1.3 24410 6430 3206 1997
    ## 5 1004  0  0  0  2  2  1  2  1  1   0   3   0   3   0 1.2  5079 1024  899 4092
    ## 6 1005 38  1  3  1  2  1  7  1  2   0   5   0  14   1 1.8 24410 6430 3206 1997

> **Note:** The variables do not have names. Use the algorithm below to
> create new columns with the assigned variables.

#### Create new variables and assign the values to each one respectively.

``` r
data.delay["minutes"] <- NA

data.delay$minutes <-  data.delay$X1

data.delay["activity"] <- NA

data.delay$activity <-  data.delay$X2

data.delay["number_of_times"] <- NA

data.delay$number_of_times <-  data.delay$X3

data.delay["mode"] <- NA

data.delay$mode <-  data.delay$X4

data.delay["route"] <- NA

data.delay$route <-  data.delay$X5

data.delay["congested"] <- NA

data.delay$congested <-  data.delay$X6

data.delay["age"] <- NA

data.delay$age <-  data.delay$X7

data.delay["gender"] <- NA

data.delay$gender <-  data.delay$X8

data.delay["number_cars"] <- NA

data.delay$number_cars <-  data.delay$X9

data.delay["number_children"] <- NA

data.delay$number_children <-  data.delay$X10

data.delay["income"] <- NA

data.delay$income <-  data.delay$X11

data.delay["flexible"] <- NA

data.delay$flexible <-  data.delay$X12

data.delay["distance"] <- NA

data.delay$distance <-  data.delay$X13

data.delay["LOSD"] <- NA

data.delay$LOSD <-  data.delay$X14

data.delay["rate_of_travel"] <- NA

data.delay$rate_of_travel <-  data.delay$X15

data.delay["population"] <- NA

data.delay$population <-  data.delay$X16

data.delay["retail"] <- NA

data.delay$retail <-  data.delay$X17

data.delay["service"] <- NA

data.delay$service <-  data.delay$X18

data.delay["size"] <- NA

data.delay$size <-  data.delay$X19    
```

#### Take a look at the structure

``` r
str(data.delay)
```

    ## 'data.frame':    204 obs. of  39 variables:
    ##  $ id             : num  1000 1001 1002 1003 1004 ...
    ##  $ X1             : num  0 30 0 0 0 38 0 0 45 0 ...
    ##  $ X2             : num  0 1 0 0 0 1 0 0 1 0 ...
    ##  $ X3             : num  0 1 0 0 0 3 0 0 0 0 ...
    ##  $ X4             : num  1 1 1 1 2 1 2 1 2 1 ...
    ##  $ X5             : num  2 2 5 5 2 2 2 1 2 5 ...
    ##  $ X6             : num  1 1 1 1 1 1 1 0 1 0 ...
    ##  $ X7             : num  1 2 4 4 2 7 2 7 3 6 ...
    ##  $ X8             : num  1 0 1 1 1 1 1 1 1 1 ...
    ##  $ X9             : num  2 2 1 1 1 2 2 2 2 3 ...
    ##  $ X10            : num  0 0 0 0 0 0 0 0 0 1 ...
    ##  $ X11            : num  5 5 3 2 3 5 6 4 4 5 ...
    ##  $ X12            : num  1 0 0 0 0 0 0 0 0 0 ...
    ##  $ X13            : num  12 14 5 5 3 14 11 13 22 6 ...
    ##  $ X14            : num  1 1 1 1 0 1 1 0 1 0 ...
    ##  $ X15            : chr  "1.4" "1.8" "1.3" "1.3" ...
    ##  $ X16            : num  37070 24410 24410 24410 5079 ...
    ##  $ X17            : num  1024 6430 6430 6430 1024 ...
    ##  $ X18            : num  1295 3206 3206 3206 899 ...
    ##  $ X19            : num  5610 1997 1997 1997 4092 ...
    ##  $ minutes        : num  0 30 0 0 0 38 0 0 45 0 ...
    ##  $ activity       : num  0 1 0 0 0 1 0 0 1 0 ...
    ##  $ number_of_times: num  0 1 0 0 0 3 0 0 0 0 ...
    ##  $ mode           : num  1 1 1 1 2 1 2 1 2 1 ...
    ##  $ route          : num  2 2 5 5 2 2 2 1 2 5 ...
    ##  $ congested      : num  1 1 1 1 1 1 1 0 1 0 ...
    ##  $ age            : num  1 2 4 4 2 7 2 7 3 6 ...
    ##  $ gender         : num  1 0 1 1 1 1 1 1 1 1 ...
    ##  $ number_cars    : num  2 2 1 1 1 2 2 2 2 3 ...
    ##  $ number_children: num  0 0 0 0 0 0 0 0 0 1 ...
    ##  $ income         : num  5 5 3 2 3 5 6 4 4 5 ...
    ##  $ flexible       : num  1 0 0 0 0 0 0 0 0 0 ...
    ##  $ distance       : num  12 14 5 5 3 14 11 13 22 6 ...
    ##  $ LOSD           : num  1 1 1 1 0 1 1 0 1 0 ...
    ##  $ rate_of_travel : chr  "1.4" "1.8" "1.3" "1.3" ...
    ##  $ population     : num  37070 24410 24410 24410 5079 ...
    ##  $ retail         : num  1024 6430 6430 6430 1024 ...
    ##  $ service        : num  1295 3206 3206 3206 899 ...
    ##  $ size           : num  5610 1997 1997 1997 4092 ...

> **Note:** We have repeated variables. Let us exclude the original ones
> without labels.

#### Exclude original variables

``` r
drop <- c("X1","X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19")
df = data.delay[,!(names(data.delay) %in% drop)]
```

``` r
skim(df)
```

|                                                  |      |
| :----------------------------------------------- | :--- |
| Name                                             | df   |
| Number of rows                                   | 204  |
| Number of columns                                | 20   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| character                                        | 1    |
| numeric                                          | 19   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: character**

| skim\_variable   | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :--------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| rate\_of\_travel |          0 |              1 |   3 |   3 |     0 |        15 |          0 |

**Variable type: numeric**

| skim\_variable    | n\_missing | complete\_rate |     mean |       sd |   p0 |      p25 |     p50 |      p75 |  p100 | hist  |
| :---------------- | ---------: | -------------: | -------: | -------: | ---: | -------: | ------: | -------: | ----: | :---- |
| id                |          0 |              1 |  1101.50 |    59.03 | 1000 |  1050.75 |  1101.5 |  1152.25 |  1203 | ▇▇▇▇▇ |
| minutes           |          0 |              1 |    24.14 |    36.27 |    0 |     0.00 |     0.0 |    30.00 |   240 | ▇▁▁▁▁ |
| activity          |          0 |              1 |     0.78 |     0.97 |    0 |     0.00 |     0.0 |     1.00 |     3 | ▇▃▁▂▁ |
| number\_of\_times |          0 |              1 |     0.86 |     1.31 |    0 |     0.00 |     0.0 |     2.00 |     5 | ▇▂▁▁▁ |
| mode              |          0 |              1 |     2.08 |     1.49 |    1 |     1.00 |     1.0 |     4.00 |     5 | ▇▂▁▂▂ |
| route             |          0 |              1 |     3.49 |     1.46 |    1 |     2.00 |     3.5 |     5.00 |     5 | ▂▆▂▂▇ |
| congested         |          0 |              1 |     0.63 |     0.48 |    0 |     0.00 |     1.0 |     1.00 |     1 | ▅▁▁▁▇ |
| age               |          0 |              1 |     2.83 |     1.69 |    1 |     2.00 |     2.0 |     4.00 |     7 | ▇▃▂▁▁ |
| gender            |          0 |              1 |     0.64 |     0.48 |    0 |     0.00 |     1.0 |     1.00 |     1 | ▅▁▁▁▇ |
| number\_cars      |          0 |              1 |     1.80 |     1.04 |    0 |     1.00 |     2.0 |     2.00 |     7 | ▇▇▂▁▁ |
| number\_children  |          0 |              1 |     0.70 |     1.05 |    0 |     0.00 |     0.0 |     1.00 |     5 | ▇▂▁▁▁ |
| income            |          0 |              1 |     2.78 |     1.56 |    1 |     1.00 |     3.0 |     4.00 |     6 | ▇▃▂▂▁ |
| flexible          |          0 |              1 |     0.62 |     0.49 |    0 |     0.00 |     1.0 |     1.00 |     1 | ▅▁▁▁▇ |
| distance          |          0 |              1 |     7.15 |     4.81 |    1 |     4.00 |     6.0 |    10.00 |    25 | ▇▆▃▁▁ |
| LOSD              |          0 |              1 |     0.65 |     0.48 |    0 |     0.00 |     1.0 |     1.00 |     1 | ▅▁▁▁▇ |
| population        |          0 |              1 | 25367.32 | 10232.19 | 1303 | 23026.00 | 24410.0 | 34895.00 | 37070 | ▃▁▁▇▇ |
| retail            |          0 |              1 |  4604.66 |  4334.42 |  616 |  1866.00 |  3906.0 |  3966.00 | 16523 | ▆▇▁▁▂ |
| service           |          0 |              1 |  9733.06 | 10552.66 |  595 |  1606.00 | 10582.0 | 10582.00 | 38607 | ▇▇▁▁▂ |
| size              |          0 |              1 |  3087.51 |  1598.03 |  475 |  2472.00 |  2753.0 |  4447.75 |  5653 | ▃▇▇▁▆ |

#### Sort the data by time

``` r
df <- df[order(df$minutes),]
```

#### Create graph

``` r
with(df, plot(minutes, type="h"))
```

![](README_files/8-HazardBasedModels/unnamed-chunk-9-1.png)<!-- -->

#### Create the life table survival object for df

``` r
data.delay2 <-subset(df, minutes>0)
df.survfit = survfit(Surv(minutes) ~ 1, data= data.delay2)
summary(df.survfit)
```

    ## Call: survfit(formula = Surv(minutes) ~ 1, data = data.delay2)
    ## 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     4     96       1   0.9896  0.0104      0.96948       1.0000
    ##    10     95       4   0.9479  0.0227      0.90450       0.9934
    ##    15     91       7   0.8750  0.0338      0.81128       0.9437
    ##    20     84       1   0.8646  0.0349      0.79878       0.9358
    ##    24     83       1   0.8542  0.0360      0.78640       0.9278
    ##    25     82       1   0.8438  0.0371      0.77416       0.9196
    ##    30     81      31   0.5208  0.0510      0.42990       0.6310
    ##    38     50       1   0.5104  0.0510      0.41961       0.6209
    ##    40     49       3   0.4792  0.0510      0.38897       0.5903
    ##    45     46      10   0.3750  0.0494      0.28965       0.4855
    ##    53     36       1   0.3646  0.0491      0.27997       0.4748
    ##    60     35      16   0.1979  0.0407      0.13231       0.2961
    ##    80     19       1   0.1875  0.0398      0.12364       0.2843
    ##    90     18       7   0.1146  0.0325      0.06571       0.1998
    ##   100     11       1   0.1042  0.0312      0.05794       0.1873
    ##   105     10       1   0.0937  0.0297      0.05033       0.1746
    ##   120      9       6   0.0312  0.0178      0.01026       0.0952
    ##   130      3       1   0.0208  0.0146      0.00529       0.0821
    ##   150      2       1   0.0104  0.0104      0.00148       0.0732
    ##   240      1       1   0.0000     NaN           NA           NA

> **Note:** The functions survfit() and Surv() create a life table
> survival object.

#### Plot the Kaplan-Meier curve

``` r
plot(df.survfit, xlab = "Time (minutes)", ylab="Survival
probability", conf.int=TRUE)
ggsurvplot(df.survfit, xlab = "Time (minutes)", xlim =
range(0:250) , conf.int = TRUE, pallete = "red", ggtheme =
theme_minimal())
```

![](README_files/8-HazardBasedModels/unnamed-chunk-11-1.png)<!-- -->![](README_files/8-HazardBasedModels/unnamed-chunk-11-2.png)<!-- -->

#### Cox Proportional Hazard Model Estimates of the Duration of Commuter Work-To-Home Delay to Avoid Congestion

``` r
result.cox <- coxph(Surv(minutes) ~ gender + rate_of_travel + distance + population, data= data.delay2)
summary(result.cox)
```

    ## Call:
    ## coxph(formula = Surv(minutes) ~ gender + rate_of_travel + distance + 
    ##     population, data = data.delay2)
    ## 
    ##   n= 96, number of events= 96 
    ## 
    ##                         coef  exp(coef)   se(coef)      z Pr(>|z|)  
    ## gender             1.617e-01  1.176e+00  2.608e-01  0.620   0.5352  
    ## rate_of_travel1.3 -3.625e-01  6.960e-01  1.280e+00 -0.283   0.7771  
    ## rate_of_travel1.4  1.233e+00  3.430e+00  1.251e+00  0.985   0.3245  
    ## rate_of_travel1.5 -5.127e-01  5.989e-01  1.186e+00 -0.432   0.6656  
    ## rate_of_travel1.6  3.660e-02  1.037e+00  1.147e+00  0.032   0.9746  
    ## rate_of_travel1.7  1.018e+00  2.767e+00  1.341e+00  0.759   0.4478  
    ## rate_of_travel1.8 -8.239e-01  4.387e-01  1.093e+00 -0.754   0.4510  
    ## rate_of_travel1.9 -9.901e-01  3.716e-01  1.142e+00 -0.867   0.3860  
    ## rate_of_travel2.0 -9.167e-01  3.998e-01  1.115e+00 -0.822   0.4112  
    ## rate_of_travel2.1 -1.468e+00  2.303e-01  1.086e+00 -1.353   0.1762  
    ## rate_of_travel2.2 -1.138e+00  3.204e-01  1.119e+00 -1.017   0.3089  
    ## rate_of_travel2.3 -1.853e+00  1.568e-01  1.109e+00 -1.670   0.0949 .
    ## rate_of_travel2.4 -1.515e+00  2.197e-01  1.117e+00 -1.357   0.1748  
    ## rate_of_travel2.5 -1.510e+00  2.208e-01  1.114e+00 -1.355   0.1753  
    ## distance          -7.486e-02  9.279e-01  3.403e-02 -2.200   0.0278 *
    ## population        -2.157e-05  1.000e+00  1.319e-05 -1.635   0.1020  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                   exp(coef) exp(-coef) lower .95 upper .95
    ## gender               1.1755     0.8507   0.70512    1.9597
    ## rate_of_travel1.3    0.6960     1.4369   0.05662    8.5547
    ## rate_of_travel1.4    3.4301     0.2915   0.29538   39.8330
    ## rate_of_travel1.5    0.5989     1.6698   0.05858    6.1226
    ## rate_of_travel1.6    1.0373     0.9641   0.10950    9.8255
    ## rate_of_travel1.7    2.7666     0.3615   0.19994   38.2812
    ## rate_of_travel1.8    0.4387     2.2793   0.05150    3.7373
    ## rate_of_travel1.9    0.3716     2.6914   0.03962    3.4847
    ## rate_of_travel2.0    0.3998     2.5010   0.04492    3.5593
    ## rate_of_travel2.1    0.2303     4.3425   0.02743    1.9335
    ## rate_of_travel2.2    0.3204     3.1208   0.03578    2.8698
    ## rate_of_travel2.3    0.1568     6.3758   0.01784    1.3792
    ## rate_of_travel2.4    0.2197     4.5514   0.02462    1.9612
    ## rate_of_travel2.5    0.2208     4.5287   0.02486    1.9616
    ## distance             0.9279     1.0777   0.86801    0.9919
    ## population           1.0000     1.0000   0.99995    1.0000
    ## 
    ## Concordance= 0.677  (se = 0.036 )
    ## Likelihood ratio test= 30.65  on 16 df,   p=0.01
    ## Wald test            = 32.27  on 16 df,   p=0.009
    ## Score (logrank) test = 36  on 16 df,   p=0.003

#### Testing proportional Hazards assumption

``` r
test.ph <- cox.zph(result.cox)
plot(test.ph)
```

![](README_files/8-HazardBasedModels/unnamed-chunk-13-1.png)<!-- -->![](README_files/8-HazardBasedModels/unnamed-chunk-13-2.png)<!-- -->![](README_files/8-HazardBasedModels/unnamed-chunk-13-3.png)<!-- -->

``` r
ggcoxzph(test.ph)
```

![](README_files/8-HazardBasedModels/unnamed-chunk-13-4.png)<!-- -->![](README_files/8-HazardBasedModels/unnamed-chunk-13-5.png)<!-- -->

> **Note:** Include an interaction between the covariate and a function
> of time (or distance). Log time often used but could be any function.
> If significant then assumption violated.

> **Note:** Test the proportional hazards assumption on the basis of
> partial residuals. Type of residual known as Schoenfeld residuals.

> **Note:** For each covariate, the function cox.zph() correlates the
> corresponding set of scaled Schoenfeld residuals with time, to test
> for independence between residuals and time. Additionally, it performs
> a global test for the model as a whole.

> **Note:** In principle, the Schoenfeld residuals are independent of
> time. A plot that shows a non-random pattern against time is evidence
> of violation of the PH assumption.

#### Plot the baseline survival function

``` r
#  slide 56
```
