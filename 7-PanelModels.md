Panel Data Models
================

``` r
library(readxl)
library(skimr)
library(foreign)
library(plm)
```

``` r
data <- read_excel("Data/Grunfeld_data.xlsx")
```

``` r
df <- data.frame(data)
```

``` r
head(df)
```

    ##   invest  value capital           firm year firmcod
    ## 1  317.6 3078.5     2.8 General Motors 1935       6
    ## 2  391.8 4661.7    52.6 General Motors 1936       6
    ## 3  410.6 5387.1   156.9 General Motors 1937       6
    ## 4  257.7 2792.2   209.2 General Motors 1938       6
    ## 5  330.8 4313.2   203.4 General Motors 1939       6
    ## 6  461.2 4643.9   207.2 General Motors 1940       6

``` r
skim(df)
```

|                                                  |      |
| :----------------------------------------------- | :--- |
| Name                                             | df   |
| Number of rows                                   | 220  |
| Number of columns                                | 6    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| character                                        | 1    |
| numeric                                          | 5    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| firm           |          0 |              1 |   3 |  17 |     0 |        11 |          0 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |      sd |      p0 |     p25 |     p50 |     p75 |   p100 | hist  |
| :------------- | ---------: | -------------: | ------: | ------: | ------: | ------: | ------: | ------: | -----: | :---- |
| invest         |          0 |              1 |  133.31 |  210.59 |    0.93 |   27.38 |   52.36 |   99.78 | 1486.7 | ▇▁▁▁▁ |
| value          |          0 |              1 |  988.58 | 1287.30 |   30.28 |  160.32 |  404.65 | 1605.93 | 6241.7 | ▇▂▁▁▁ |
| capital        |          0 |              1 |  257.11 |  293.23 |    0.80 |   67.10 |  180.10 |  344.50 | 2226.3 | ▇▁▁▁▁ |
| year           |          0 |              1 | 1944.50 |    5.78 | 1935.00 | 1939.75 | 1944.50 | 1949.25 | 1954.0 | ▇▇▇▇▇ |
| firmcod        |          0 |              1 |    6.00 |    3.17 |    1.00 |    3.00 |    6.00 |    9.00 |   11.0 | ▇▅▅▅▅ |

``` r
drop <- c("firmcod")
df = df[,!(names(df) %in% drop)]
```

#### Ordinary least square model

``` r
mlr = lm(invest ~ value + capital, data = df)
summary(mlr)
```

    ## 
    ## Call:
    ## lm(formula = invest ~ value + capital, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -290.33  -25.76   11.06   29.74  377.94 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -38.410054   8.413371  -4.565 8.35e-06 ***
    ## value         0.114534   0.005519  20.753  < 2e-16 ***
    ## capital       0.227514   0.024228   9.390  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 90.28 on 217 degrees of freedom
    ## Multiple R-squared:  0.8179, Adjusted R-squared:  0.8162 
    ## F-statistic: 487.3 on 2 and 217 DF,  p-value: < 2.2e-16

#### One way fixed effects model

``` r
fixed = plm(invest ~ value + capital, data = df, index = c("firm", "year"), model = "within")
summary(fixed)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = invest ~ value + capital, data = df, model = "within", 
    ##     index = c("firm", "year"))
    ## 
    ## Balanced Panel: n = 11, T = 20, N = 220
    ## 
    ## Residuals:
    ##       Min.    1st Qu.     Median    3rd Qu.       Max. 
    ## -184.00792  -15.66024    0.27161   16.41421  250.75337 
    ## 
    ## Coefficients:
    ##         Estimate Std. Error t-value  Pr(>|t|)    
    ## value    0.11013    0.01130  9.7461 < 2.2e-16 ***
    ## capital  0.31003    0.01654 18.7439 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    2244500
    ## Residual Sum of Squares: 523720
    ## R-Squared:      0.76667
    ## Adj. R-Squared: 0.75314
    ## F-statistic: 340.079 on 2 and 207 DF, p-value: < 2.22e-16

#### One way random effects model

``` r
random = plm(invest ~ value + capital, data = df, index = c("firm", "year"), model = "random")
summary(random)
```

    ## Oneway (individual) effect Random Effect Model 
    ##    (Swamy-Arora's transformation)
    ## 
    ## Call:
    ## plm(formula = invest ~ value + capital, data = df, model = "random", 
    ##     index = c("firm", "year"))
    ## 
    ## Balanced Panel: n = 11, T = 20, N = 220
    ## 
    ## Effects:
    ##                   var std.dev share
    ## idiosyncratic 2530.04   50.30  0.29
    ## individual    6201.93   78.75  0.71
    ## theta: 0.8586
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -178.0540  -18.9286    4.2636   14.8933  253.3183 
    ## 
    ## Coefficients:
    ##                Estimate  Std. Error z-value Pr(>|z|)    
    ## (Intercept) -53.9436014  25.6969760 -2.0992   0.0358 *  
    ## value         0.1093053   0.0099138 11.0256   <2e-16 ***
    ## capital       0.3080360   0.0163873 18.7972   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    2393800
    ## Residual Sum of Squares: 550610
    ## R-Squared:      0.76999
    ## Adj. R-Squared: 0.76787
    ## Chisq: 726.428 on 2 DF, p-value: < 2.22e-16

You can also try other types of model estimation:

``` r
model <-c ("Fixed effects", "Pooling model", "First-diference model", "Between model", "Random effects")
code <-c("within", "pooling", "fd", "between", "random")
table <- data.frame(model,code)
knitr::kable(table, align = "l")
```

| model                 | code    |
| :-------------------- | :------ |
| Fixed effects         | within  |
| Pooling model         | pooling |
| First-diference model | fd      |
| Between model         | between |
| Random effects        | random  |

#### Use the Hausman test to evaluate when to use fixed or random effects

``` r
phtest(random, fixed)
```

    ## 
    ##  Hausman Test
    ## 
    ## data:  invest ~ value + capital
    ## chisq = 3.9675, df = 2, p-value = 0.1376
    ## alternative hypothesis: one model is inconsistent

> **Note:** The null hypothesis is that random effect model is more
> appropriate than the fixed effect model.

#### Two-way Fixed effects model

``` r
fixed_tw <- plm(invest ~ value + capital, data = df, effect = "twoways", model = "within", index = c("firm", "year"))
summary(fixed_tw)
```

    ## Twoways effects Within Model
    ## 
    ## Call:
    ## plm(formula = invest ~ value + capital, data = df, effect = "twoways", 
    ##     model = "within", index = c("firm", "year"))
    ## 
    ## Balanced Panel: n = 11, T = 20, N = 220
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -163.4113  -17.6747   -1.8345   17.9490  217.1070 
    ## 
    ## Coefficients:
    ##         Estimate Std. Error t-value  Pr(>|t|)    
    ## value   0.116681   0.012933  9.0219 < 2.2e-16 ***
    ## capital 0.351436   0.021049 16.6964 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    1672200
    ## Residual Sum of Squares: 459400
    ## R-Squared:      0.72527
    ## Adj. R-Squared: 0.67997
    ## F-statistic: 248.15 on 2 and 188 DF, p-value: < 2.22e-16

#### Two-way Random effects model

``` r
random_tw <- plm(invest ~ value + capital, data = df, effect = "twoways", model = "random", index = c("firm", "year"), random.method = "amemiya")
summary(random_tw)
```

    ## Twoways effects Random Effect Model 
    ##    (Amemiya's transformation)
    ## 
    ## Call:
    ## plm(formula = invest ~ value + capital, data = df, effect = "twoways", 
    ##     model = "random", random.method = "amemiya", index = c("firm", 
    ##         "year"))
    ## 
    ## Balanced Panel: n = 11, T = 20, N = 220
    ## 
    ## Effects:
    ##                   var std.dev share
    ## idiosyncratic 2417.89   49.17 0.256
    ## individual    6859.38   82.82 0.726
    ## time           175.63   13.25 0.019
    ## theta: 0.8684 (id) 0.2544 (time) 0.2535 (total)
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -176.9380  -16.0966    3.9358   15.6184  237.6072 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z-value Pr(>|z|)    
    ## (Intercept) -58.498376  27.083483 -2.1599  0.03078 *  
    ## value         0.110623   0.010299 10.7413  < 2e-16 ***
    ## capital       0.320686   0.017630 18.1897  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    2119700
    ## Residual Sum of Squares: 523110
    ## R-Squared:      0.75321
    ## Adj. R-Squared: 0.75094
    ## Chisq: 662.3 on 2 DF, p-value: < 2.22e-16

#### Langrage Multiplier Test

``` r
plmtest(random_tw)
```

    ## 
    ##  Lagrange Multiplier Test - (Honda) for balanced panels
    ## 
    ## data:  invest ~ value + capital
    ## normal = 29.576, p-value < 2.2e-16
    ## alternative hypothesis: significant effects
