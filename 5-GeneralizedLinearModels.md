Generalized Linear Models
================

#### Example exercise: Accident data from California (1993 to 1998) and Michigan (1993 to 1997)

**Your task**: Analyze the number o accidents.

#### Variables:

-   `ACCIDENT`: Count on injury accidents over observation period;  
-   `STATE`: indicator variable for state \[0: California \| 1:
    Michigan\]  
-   `AADT1`: Average annual daily traffic on major road;  
-   `AADT2`: Average annual daily traffic on minor road;  
-   `Median`: Median width on major road in feet;  
-   `DRIVE`: Number of driveways within 250 ft of intersection center.

## Startup

##### Import Libraries

``` r
library(readxl) # reading excel files
library(skimr) # summary statistics
library(tidyverse) # Pack of useful tools
library(DataExplorer) # Exploratory data analysis
library(MASS) # Negative binomial regression
library(vcd) # Godness of fit parameters
library(car) # Goodness of fit
library(rcompanion) # Goodness of fit
library(popbio) # calculate elasticities
```

##### Import dataset

``` r
dataset <- read_excel("Data/TDM_GZLM_CALMICH_Example.xlsX")
view(dataset)
```

The dataset looks weird. It is better to import it again, using the
RStudio menus, as follows:

1.  Go to “Import Dataset” on the “Environment” window at the upper
    right display;  
2.  Click on “From excel”;  
3.  Check if “First row as names” is checked;  
4.  Put the number of rows you want to skip and click on “import”;  
5.  This will generate a code, which you can copy and use it the next
    time you open the file.

Therefore, here is the given code:

``` r
dataset <- read_excel("Data/TDM_GZLM_CALMICH_Example.xlsx", skip = 5) #skipping the first 5 rows
view(dataset)
```

## Get to know your data

##### Summary statistics

Look at the descriptive statistics of the dataframe

``` r
df <- dataset
str(df)
```

    ## tibble [84 x 6] (S3: tbl_df/tbl/data.frame)
    ##  $ STATE   : num [1:84] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ACCIDENT: num [1:84] 0 0 0 0 2 8 2 4 3 12 ...
    ##  $ AADT1   : num [1:84] 6633 6633 6633 6633 12700 ...
    ##  $ AADT2   : num [1:84] 180 51 100 51 21 1700 51 701 45 65 ...
    ##  $ MEDIAN  : num [1:84] 16 16 16 16 0 0 0 0 0 0 ...
    ##  $ DRIVE   : num [1:84] 1 1 0 1 13 7 15 12 8 14 ...

``` r
summary(df)
```

    ##      STATE           ACCIDENT          AADT1           AADT2       
    ##  Min.   :0.0000   Min.   : 0.000   Min.   : 2367   Min.   :  15.0  
    ##  1st Qu.:0.0000   1st Qu.: 0.000   1st Qu.: 7307   1st Qu.: 101.0  
    ##  Median :0.0000   Median : 1.000   Median :12050   Median : 348.5  
    ##  Mean   :0.2857   Mean   : 2.619   Mean   :12870   Mean   : 595.9  
    ##  3rd Qu.:1.0000   3rd Qu.: 4.000   3rd Qu.:16659   3rd Qu.: 917.5  
    ##  Max.   :1.0000   Max.   :13.000   Max.   :33058   Max.   :3001.0  
    ##      MEDIAN           DRIVE       
    ##  Min.   : 0.000   Min.   : 0.000  
    ##  1st Qu.: 0.000   1st Qu.: 0.000  
    ##  Median : 0.000   Median : 1.000  
    ##  Mean   : 3.798   Mean   : 3.095  
    ##  3rd Qu.: 6.000   3rd Qu.: 5.250  
    ##  Max.   :36.000   Max.   :15.000

##### Preparing your data

Have a look at the variable `STATE`. This is a **binary variable**, 0:
California and 1: Michigan. It does not mean that Michigan is somehow
higher or better than California, just because it is coded as 1. We then
should prepare the data, letting R know that this should not be treated
as a numeric variable, but as a categorical nominal one.  
We use the `factor` function to do so, and we can also say which values
mean what, so it gets easier to read plots or model results.

``` r
df$STATE <- factor(df$STATE, labels = c("California", "Michigan"))
table(df$STATE)
```

    ## 
    ## California   Michigan 
    ##         60         24

Now if we look again to the summary statistics, the variable `STATE`
will show up as a different one from the others.

``` r
skim(df)
```

|                                                  |      |
|:-------------------------------------------------|:-----|
| Name                                             | df   |
| Number of rows                                   | 84   |
| Number of columns                                | 6    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| factor                                           | 1    |
| numeric                                          | 5    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts      |
|:---------------|-----------:|---------------:|:--------|----------:|:-----------------|
| STATE          |          0 |              1 | FALSE   |         2 | Cal: 60, Mic: 24 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |     mean |      sd |   p0 |     p25 |     p50 |      p75 |  p100 | hist  |
|:---------------|-----------:|---------------:|---------:|--------:|-----:|--------:|--------:|---------:|------:|:------|
| ACCIDENT       |          0 |              1 |     2.62 |    3.36 |    0 |    0.00 |     1.0 |     4.00 |    13 | ▇▂▁▁▁ |
| AADT1          |          0 |              1 | 12869.71 | 6797.85 | 2367 | 7307.25 | 12050.0 | 16658.50 | 33058 | ▇▇▅▂▁ |
| AADT2          |          0 |              1 |   595.86 |  679.27 |   15 |  101.00 |   348.5 |   917.50 |  3001 | ▇▂▁▁▁ |
| MEDIAN         |          0 |              1 |     3.80 |    6.09 |    0 |    0.00 |     0.0 |     6.00 |    36 | ▇▁▁▁▁ |
| DRIVE          |          0 |              1 |     3.10 |    3.90 |    0 |    0.00 |     1.0 |     5.25 |    15 | ▇▂▂▁▁ |

##### Histograms

Take a look at the histograms of the variables

``` r
plot_histogram(df, ncol = 3) #with 3 columns
```

![](README_files/GLM/unnamed-chunk-7-1.png)<!-- -->

## Generalized Linear Models

For this example, we will have the variable `ACCIDENTS` as the dependent
variable.

Let’s start by plotting the **density function** of the dependent
variable.

``` r
plot(density(df$ACCIDENT), main="Density estimate of ACCIDENTS")
```

![](README_files/GLM/unnamed-chunk-8-1.png)<!-- -->

As the dependent variable is “count data”, and has discrete values (it
is not continuous), then a Poisson distribution should be more adequate.

##### Poisson assumption

Take a look at the mean and the variance of the dependent variable.
Check if they are equal to each other.

``` r
mean(df$ACCIDENT)
```

    ## [1] 2.619048

``` r
var(df$ACCIDENT)
```

    ## [1] 11.29891

``` r
var(df$ACCIDENT)/mean(df$ACCIDENT) #coefficient of variance
```

    ## [1] 4.314129

> **Note**: If the coefficient of variance &gt; 1, you have
> overdispersion.

##### Goodness of fit

Estimate goodness of fit parameter for the PDF of ACCIDENT.

``` r
gf<-goodfit(df$ACCIDENT,  type= "poisson", method= "ML") #Maximum Likelihood method
summary(gf)
```

    ## 
    ##   Goodness-of-fit test for poisson distribution
    ## 
    ##                       X^2 df     P(> X^2)
    ## Likelihood Ratio 155.7106 11 1.012305e-27

> **Note**: The null hypothesis is that it is a Poisson distribution.
> Therefore, for it to be a Poisson distribution, the pvalue &gt; 0.05.

### Different models

There are many families and links that can be used, depending on the
characteristics of your data.

| family           | link                   |
|:-----------------|:-----------------------|
| Gaussian         | identity               |
| Binomial         | logit, probit, cloglog |
| Poisson          | log, identity, sqrt    |
| Gamma            | inverse, identity, log |
| inverse.gaussian | 1/mu^2                 |

Now let us try two possible models to fit this data:

1.  Poisson Model
2.  Overdispersed Poisson Model
3.  Negative Binomial distribution

### 1. Poisson model

We start by declaring what type of modelling we are performing. `gml()`
stands for Generalized Linear Models.  
The dependent variable should be declared before an `~` and then all the
independent variables we want to try. If you want to try with all the
variables in the dataset without any changes, you can just write `~ .`
and the dot assumes that are *all the other*.

``` r
model1 = glm(
  ACCIDENT ~ STATE + AADT2 + MEDIAN + DRIVE + offset(log(AADT1)),
  family = poisson(link = "log"),
  data = df,
  method = "glm.fit"
)
```

> **Note**: The method “glm.fit” uses iteratively reweighted least
> squares to fit the model. Try looking for other methods and see the
> difference.

``` r
summary(model1)
```

    ## 
    ## Call:
    ## glm(formula = ACCIDENT ~ STATE + AADT2 + MEDIAN + DRIVE + offset(log(AADT1)), 
    ##     family = poisson(link = "log"), data = df, method = "glm.fit")
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.1238  -1.2665  -0.5184   0.3952   3.7088  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -8.997e+00  1.656e-01 -54.320  < 2e-16 ***
    ## STATEMichigan -2.000e-01  1.590e-01  -1.258   0.2084    
    ## AADT2          5.136e-04  7.406e-05   6.935 4.06e-12 ***
    ## MEDIAN        -5.212e-02  2.147e-02  -2.427   0.0152 *  
    ## DRIVE          6.552e-02  1.634e-02   4.011 6.04e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 255.07  on 83  degrees of freedom
    ## Residual deviance: 167.31  on 79  degrees of freedom
    ## AIC: 339.29
    ## 
    ## Number of Fisher Scoring iterations: 5

What can we say about the **residuals** and **degrees of freedom**?

-   residuals &gt; degrees of freedom -&gt; **overdispersion**
-   residuals &lt; degrees of freedom -&gt; **underdispersion**
-   residuals = degrees of freedom -&gt; **mean = variance**

> **Note:** In overdispersion, the estimates are reliable but the
> standard errors tend to be smaller.

##### Pseudo-Rsquare and Log-likelihood ratio test

Calculate the pseudo-Rsquare and perform an Omnibus test

``` r
nagelkerke(model1)
```

    ## $Models
    ##                                                                                                                   
    ## Model: "glm, ACCIDENT ~ STATE + AADT2 + MEDIAN + DRIVE + offset(log(AADT1)), poisson(link = \"log\"), df, glm.fit"
    ## Null:  "glm, ACCIDENT ~ 1, poisson(link = \"log\"), df, glm.fit"                                                  
    ## 
    ## $Pseudo.R.squared.for.model.vs.null
    ##                              Pseudo.R.squared
    ## McFadden                             0.331213
    ## Cox and Snell (ML)                   0.856499
    ## Nagelkerke (Cragg and Uhler)         0.858945
    ## 
    ## $Likelihood.ratio.test
    ##  Df.diff LogLik.diff  Chisq    p.value
    ##       -4      -81.54 163.08 3.1953e-34
    ## 
    ## $Number.of.observations
    ##          
    ## Model: 84
    ## Null:  84
    ## 
    ## $Messages
    ## [1] "Note: For models fit with REML, these statistics are based on refitting with ML"
    ## 
    ## $Warnings
    ## [1] "None"

The likelihood ratio test (Omnibus test) compares the fitted model
(“Model”) with the only-intercept model (“Null”). This test verifies if
the explained variance is higher than the the unexplained variance.

> **Note**: *h*<sub>0</sub> There is no overdispersion in the model.
> Therefore, if pvalue &lt; 0.05, there is overdispersion, and we should
> choose to use a Negative Binomial model.

##### Wald test

Calculate the Type III test.

``` r
Anova(model1, type = "III", test = "Wald")
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: ACCIDENT
    ##             Df     Chisq Pr(>Chisq)    
    ## (Intercept)  1 2950.6671  < 2.2e-16 ***
    ## STATE        1    1.5825    0.20841    
    ## AADT2        1   48.0944  4.062e-12 ***
    ## MEDIAN       1    5.8922    0.01521 *  
    ## DRIVE        1   16.0891  6.043e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Type III tests examine the significance of each partial effect. Thus, it
considers the significance of an effect with all the other effects in
the model. The *χ*<sup>2</sup> (Chisq) tests the significance of the
effect added to the model by having all of the other effects.

### 2. Overdispersed Poisson Model

Let us correct the standard errors with an overdispersed poisson model.

``` r
model2 = glm(
  ACCIDENT ~ STATE + AADT2 + MEDIAN + DRIVE + offset(log(AADT1)),
  family = quasipoisson(link = "log"),
  data = df,
  method = "glm.fit"
)
summary(model2)
```

    ## 
    ## Call:
    ## glm(formula = ACCIDENT ~ STATE + AADT2 + MEDIAN + DRIVE + offset(log(AADT1)), 
    ##     family = quasipoisson(link = "log"), data = df, method = "glm.fit")
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.1238  -1.2665  -0.5184   0.3952   3.7088  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -8.9972283  0.2387734 -37.681  < 2e-16 ***
    ## STATEMichigan -0.2000225  0.2292178  -0.873  0.38551    
    ## AADT2          0.0005136  0.0001068   4.811 7.08e-06 ***
    ## MEDIAN        -0.0521227  0.0309546  -1.684  0.09616 .  
    ## DRIVE          0.0655221  0.0235482   2.782  0.00675 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for quasipoisson family taken to be 2.07814)
    ## 
    ##     Null deviance: 255.07  on 83  degrees of freedom
    ## Residual deviance: 167.31  on 79  degrees of freedom
    ## AIC: NA
    ## 
    ## Number of Fisher Scoring iterations: 5

> **Note**: The estimates are the same, but the standard errors have
> increased because they are adjusted by the scale parameter.

### 3. Negative Binomial distribution

We use `glm.nb()` for this one.

``` r
model3 = glm.nb(ACCIDENT ~ STATE + AADT2 + MEDIAN + DRIVE + offset(log(AADT1)),
                data = df)
summary(model3)
```

    ## 
    ## Call:
    ## glm.nb(formula = ACCIDENT ~ STATE + AADT2 + MEDIAN + DRIVE + 
    ##     offset(log(AADT1)), data = df, init.theta = 2.19225058, link = log)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.2604  -0.9918  -0.3235   0.2587   2.3277  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -8.9883516  0.2416863 -37.190  < 2e-16 ***
    ## STATEMichigan -0.2736628  0.2574693  -1.063   0.2878    
    ## AADT2          0.0005680  0.0001419   4.004 6.23e-05 ***
    ## MEDIAN        -0.0633165  0.0301447  -2.100   0.0357 *  
    ## DRIVE          0.0597809  0.0279771   2.137   0.0326 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Negative Binomial(2.1923) family taken to be 1)
    ## 
    ##     Null deviance: 125.481  on 83  degrees of freedom
    ## Residual deviance:  87.601  on 79  degrees of freedom
    ## AIC: 313.72
    ## 
    ## Number of Fisher Scoring iterations: 1
    ## 
    ## 
    ##               Theta:  2.192 
    ##           Std. Err.:  0.762 
    ## 
    ##  2 x log-likelihood:  -301.719

##### Pseudo-Rsquare and Log-likelihood ratio test

Calculate the pseudo-Rsquare and perform an Omnibus test

``` r
Anova(model3, type = "III", test = "Wald") 
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: ACCIDENT
    ##             Df     Chisq Pr(>Chisq)    
    ## (Intercept)  1 1383.1076  < 2.2e-16 ***
    ## STATE        1    1.1297    0.28783    
    ## AADT2        1   16.0325  6.227e-05 ***
    ## MEDIAN       1    4.4118    0.03569 *  
    ## DRIVE        1    4.5658    0.03262 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##### Wald test

Calculate the Type III test.

``` r
nagelkerke(model3)
```

    ## $Models
    ##                                                                                                     
    ## Model: "glm.nb, ACCIDENT ~ STATE + AADT2 + MEDIAN + DRIVE + offset(log(AADT1)), df, 2.19225058, log"
    ## Null:  "glm.nb, ACCIDENT ~ 1, df, 0.6629432212, log"                                                
    ## 
    ## $Pseudo.R.squared.for.model.vs.null
    ##                              Pseudo.R.squared
    ## McFadden                             0.150312
    ## Cox and Snell (ML)                   0.470285
    ## Nagelkerke (Cragg and Uhler)         0.477249
    ## 
    ## $Likelihood.ratio.test
    ##  Df.diff LogLik.diff  Chisq    p.value
    ##       -4     -26.687 53.375 7.1132e-11
    ## 
    ## $Number.of.observations
    ##          
    ## Model: 84
    ## Null:  84
    ## 
    ## $Messages
    ## [1] "Note: For models fit with REML, these statistics are based on refitting with ML"
    ## 
    ## $Warnings
    ## [1] "None"

#### Comparing model 1 and model 3

##### AIC and BIC

Akaike’s Information Criteria (AIC) and Bayesian Information Criteria
(BIC) evaluates the quality of a finite set of models.  
AIC and BIC consider the maximum likelihood and the number of parameters
in assessing the quality of the models. Nonetheless, the difference
between both methods is that the BIC takes into account the number of
observations of dataset.  
Calculate the AIC and the BIC.

``` r
aic <- data.frame(model1 = AIC(model1), model3 = AIC(model3))
bic <- data.frame(model1 = BIC(model1), model3 = BIC(model3))
```

<table class="kable_wrapper">
<caption>
AIC and BIC
</caption>
<tbody>
<tr>
<td>

|   model1 |   model3 |
|---------:|---------:|
| 339.2905 | 313.7189 |

</td>
<td>

|   model1 |   model3 |
|---------:|---------:|
| 351.4446 | 328.3038 |

</td>
</tr>
</tbody>
</table>

> **Note**: The smaller the values of AIC and BIC, the better the model

### Elasticities

Calculate the elasticities of the negative binomial model. You can also
try to calculate for the other models.

``` r
el1 <- as.numeric(model3$coefficients["AADT1"] * mean(df$AADT1)/mean(df$ACCIDENT))
el2 <- as.numeric(model3$coefficients["AADT2"] * mean(df$AADT2)/mean(df$ACCIDENT))
el3 <- as.numeric(model3$coefficients["MEDIAN"] * mean(df$MEDIAN)/mean(df$ACCIDENT))
el4 <- as.numeric(model3$coefficients["DRIVE"] * mean(df$DRIVE)/mean(df$ACCIDENT))

elasticity <- data.frame(variable = c("AADT1", "AADT2", "MEDIAN", "DRIVE"),
                         elasticity = c(el1, el2, el3, el4))
```

| variable | elasticity |
|:---------|:-----------|
| AADT1    | NA         |
| AADT2    | 0.1292241  |
| MEDIAN   | -0.0918089 |
| DRIVE    | 0.0706501  |

> **Note:** `AADT1` does not have a value because it is the offset of
> the model. **Note:** `STATE` is a categorical variable. We would need
> to calculate the pseudo-elasticities in this case. Follow the same
> logic of the code and try to calculate for yourselves.
