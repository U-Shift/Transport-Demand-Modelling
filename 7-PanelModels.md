Panel Data Models
================

#### Example Exercise: Grunfeld Investment data

This data consists of 10 large US manufacturing firms from 1935 to 1954.

**Your Task:** Analyze the many types of panel models.

This code was based on the paper: Croissant, Y., Milo, G.(2008). [*Panel
Data Econometrics in R: The plm
Package*](https://www.jstatsoft.org/index.php/jss/article/view/v027i02/v27i02.pdf),
Journal of Statistical Software, 27(2).

## Data

#### Variables:

-   `invest`: Gross investment, defined as additions to plant and
    equipment plus maintenance and repairs in millions of dollars
    deflated by the implicit price deflator of producers’ durable
    equipment (base 1947);  
-   `value`: Market value of the firm, defined as the price of common
    shares at December 31 (base 1947);
-   `capital`: Stock of plant and equipment, defined as the accumulated
    sum of net additions to plant and equipment deflated by the implicit
    price deflator for producers’ durable equipment (base 1947);
-   `firm`: American manufacturing firms;
-   `year`: Year of data;
-   `firmcod`: Numeric code that identifies each firm.

## Startup

#### Import libraries

``` r
library(readxl) #read excel files
library(skimr) #summary statistics
library(foreign) #panel data models
library(plm) # Lagrange multiplier test and panel models
```

##### Import dataset

``` r
data <- read_excel("Data/Grunfeld_data.xlsx")
df <- data.frame(data)
```

##### Take a first look at your data

| invest |  value | capital | firm           | year | firmcod |
|-------:|-------:|--------:|:---------------|-----:|--------:|
|  317.6 | 3078.5 |     2.8 | General Motors | 1935 |       6 |
|  391.8 | 4661.7 |    52.6 | General Motors | 1936 |       6 |
|  410.6 | 5387.1 |   156.9 | General Motors | 1937 |       6 |
|  257.7 | 2792.2 |   209.2 | General Motors | 1938 |       6 |
|  330.8 | 4313.2 |   203.4 | General Motors | 1939 |       6 |
|  461.2 | 4643.9 |   207.2 | General Motors | 1940 |       6 |
|  512.0 | 4551.2 |   255.2 | General Motors | 1941 |       6 |
|  448.0 | 3244.1 |   303.7 | General Motors | 1942 |       6 |
|  499.6 | 4053.7 |   264.1 | General Motors | 1943 |       6 |
|  547.5 | 4379.3 |   201.6 | General Motors | 1944 |       6 |

#### Prepare your data

##### Take out the “firmcod” variale from the dataset

``` r
df$firmcod = NULL #another way or removing a variable
```

##### Factor categorical variables

`firm` is a categorical nominal variable, and should be treated as so in
the modeling processes. And for this example `year` should also be
considered as a categorical ordinal variable, instead of a continuous
one.

``` r
df$firm = factor(df$firm)
df$year = factor(df$year, ordered = T)
```

Take a look at the summary of your data. See the differences regarding
the categorical ones?

``` r
summary(df)
```

    ##      invest            value            capital                      firm    
    ##  Min.   :   0.93   Min.   :  30.28   Min.   :   0.8   American Steel   : 20  
    ##  1st Qu.:  27.38   1st Qu.: 160.32   1st Qu.:  67.1   Atlantic Refining: 20  
    ##  Median :  52.37   Median : 404.65   Median : 180.1   Chrysler         : 20  
    ##  Mean   : 133.31   Mean   : 988.58   Mean   : 257.1   Diamond Match    : 20  
    ##  3rd Qu.:  99.78   3rd Qu.:1605.92   3rd Qu.: 344.5   General Electric : 20  
    ##  Max.   :1486.70   Max.   :6241.70   Max.   :2226.3   General Motors   : 20  
    ##                                                       (Other)          :100  
    ##       year    
    ##  1935   : 11  
    ##  1936   : 11  
    ##  1937   : 11  
    ##  1938   : 11  
    ##  1939   : 11  
    ##  1940   : 11  
    ##  (Other):154

## Ordinary least square model

First run an Ordinary least square model without the `firm` variable.
Compare the results from this model to the many panel data models.

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

## Panel Data Models

Panel data models use **one way** and **two way** component models to
overcome heterogeneity, correlation in the disturbance terms, and
heteroscedasticity.

-   **One way error component model:** variable-intercept models across
    individuals **or** time;
-   **Two way error component model:** variable-intercept models across
    individuals **and** time.

Modelling Specifications:

-   **With fixed-effects:** effects that are in the sample.
    Fixed-effects explore the causes of change within a person or entity
    (In this example the entity is the *firms*);

-   **With random-effects:** effect randomly drawn from a population.
    The random effects model is an appropriate specification if we are
    drawing *n* individuals randomly from a large population.

You can also try other types of model estimation:

| model                 | argument |
|:----------------------|:---------|
| Fixed effects         | within   |
| Random effects        | random   |
| Pooling model         | pooling  |
| First-diference model | fd       |
| Between model         | between  |

See `?plm` for more options, regarding the effects and instrumental
variable transformation types.

### One way

##### One way fixed effects model

``` r
fixed = plm(
  invest ~ value + capital,
  data = df,
  index = c("firm", "year"), #panel settings
  model = "within" #fixed effects option
)
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

##### One way random effects model

``` r
random = plm(
  invest ~ value + capital,
  data = df,
  index = c("firm", "year"),
  model = "random" #random effects option
)
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

#### Haussman test

Use the Hausman test to evaluate when to use fixed or random effects

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

### Two-way

##### Two-way Fixed effects model

``` r
fixed_tw <-
  plm(
    invest ~ value + capital,
    data = df,
    effect = "twoways", #effects option
    model = "within", #fixed
    index = c("firm", "year") #panel settings
  )
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

##### Two-way Random effects model

``` r
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

#### Lagrange Multiplier Test

The Lagrange multiplier statistic, is used to test the null hypothesis
that there are no group effects in the Random Effects model.  
Large values of the Lagrange Multiplier indicate that effects model is
more suitable than the classical model with no common effects.

``` r
plmtest(random_tw)
```

    ## 
    ##  Lagrange Multiplier Test - (Honda) for balanced panels
    ## 
    ## data:  invest ~ value + capital
    ## normal = 29.576, p-value < 2.2e-16
    ## alternative hypothesis: significant effects

> **Note:** Large values of H indicate that the fixed effects model is
> prefered over the random effects model. While, A large value of the LM
> statistic in the presence of a small H statistic indicate that the
> random effects model is more suitable.
