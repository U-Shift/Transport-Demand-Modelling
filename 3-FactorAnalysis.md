Exploratory Factor Analysis
================

#### Example exercise: “Residential location satisfaction in the Lisbon metropolitan area”

The aim of this study was to examine the perception of households
towards their residential location considering several land use and
accessibility factors as well as household socioeconomic and attitudinal
characteristics.

*Reference:* Martinez, L. G., de Abreu e Silva, J., & Viegas, J. M.
(2010). Assessment of residential location satisfaction in the Lisbon
metropolitan area, TRB (No. 10-1161).

**Your task:** Analyze the data and create meaningful latent factors.

## Data

#### Variables:

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
- `EQUINDEX`: Number of undergraduate students/Population over 20 years
  old (500m)

#### Rules of thumb:

- At least 10 variables
- n \< 50 (Unacceptable); n \> 200 (recommended); 10 observations per
  variable or higher
- It is recommended to use continuous variables. If your data contains
  categorical variables, you should transform them to dummy variables.

#### Assumptions:

- Normality;
- linearity;
- Homoscedasticity (some multicollinearity is desirable);
- Correlations between variables \< 0.3 (not appropriate to use Factor
  Analysis)

## Let’s start!

#### Import Libraries

``` r
library(foreign) # Library used to read SPSS files
library(nFactors) # Library used for factor analysis
library(tidyverse) # Library used in data science to perform exploratory data analysis
library(summarytools) # Library used for checking the summary of the dataset
library(psych) # Library used for factor analysis
library(GPArotation) # Library used for factor analysis
library(corrplot) # Library used for correlation analysis
library(car) #For linear regression and VIF calculation (testing multicollinearity)
```

### Get to know your dataset

##### Import dataset

``` r
df <- read.spss("Data/example_fact.sav", to.data.frame = T) #transforms a list into a data.frame directly
```

##### Select a dataset with the first variables explained above

``` r
df = df[,1:24]
```

##### Make ID as row names or case number

``` r
df<-data.frame(df, row.names = 1)
```

##### Check summary statistics of variables

``` r
descriptive_stats <- dfSummary(df)
view(descriptive_stats)
```

### Data Frame Summary

#### df

**Dimensions:** 470 x 23  
**Duplicates:** 0

<table style="width:96%;">
<colgroup>
<col style="width: 4%" />
<col style="width: 11%" />
<col style="width: 29%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 9%" />
</colgroup>
<thead>
<tr class="header">
<th>No</th>
<th>Variable</th>
<th>Stats / Values</th>
<th>Freqs (% of Valid)</th>
<th>Graph</th>
<th>Missing</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>1</td>
<td>DWELCLAS<br />
[numeric]</td>
<td>Mean (sd) : 5.1 (1.3)<br />
min &lt; med &lt; max:<br />
1 &lt; 5 &lt; 7<br />
IQR (CV) : 2 (0.2)</td>
<td>1 : 5 ( 1.1%)<br />
2 : 14 ( 3.0%)<br />
3 : 31 ( 6.6%)<br />
4 : 75 (16.0%)<br />
5 : 130 (27.7%)<br />
6 : 162 (34.5%)<br />
7 : 53 (11.3%)</td>
<td><img src="tmp/ds0855.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="even">
<td>2</td>
<td>INCOME<br />
[numeric]</td>
<td>Mean (sd) : 4259.6 (3001.8)<br />
min &lt; med &lt; max:<br />
700 &lt; 2750 &lt; 12500<br />
IQR (CV) : 2000 (0.7)</td>
<td>700 : 20 ( 4.3%)<br />
1500 : 96 (20.4%)<br />
2750 : 142 (30.2%)<br />
4750 : 106 (22.6%)<br />
7500 : 75 (16.0%)<br />
12500 : 31 ( 6.6%)</td>
<td><img src="tmp/ds0856.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="odd">
<td>3</td>
<td>CHILD13<br />
[numeric]</td>
<td>Mean (sd) : 0.4 (0.8)<br />
min &lt; med &lt; max:<br />
0 &lt; 0 &lt; 4<br />
IQR (CV) : 0 (2)</td>
<td>0 : 353 (75.1%)<br />
1 : 62 (13.2%)<br />
2 : 41 ( 8.7%)<br />
3 : 13 ( 2.8%)<br />
4 : 1 ( 0.2%)</td>
<td><img src="tmp/ds0857.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="even">
<td>4</td>
<td>H18<br />
[numeric]</td>
<td>Mean (sd) : 2.1 (0.9)<br />
min &lt; med &lt; max:<br />
0 &lt; 2 &lt; 6<br />
IQR (CV) : 0.8 (0.4)</td>
<td>0 : 1 ( 0.2%)<br />
1 : 112 (23.8%)<br />
2 : 239 (50.9%)<br />
3 : 77 (16.4%)<br />
4 : 35 ( 7.4%)<br />
5 : 3 ( 0.6%)<br />
6 : 3 ( 0.6%)</td>
<td><img src="tmp/ds0858.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="odd">
<td>5</td>
<td>HEMPLOY<br />
[numeric]</td>
<td>Mean (sd) : 1.5 (0.7)<br />
min &lt; med &lt; max:<br />
0 &lt; 2 &lt; 5<br />
IQR (CV) : 1 (0.5)</td>
<td>0 : 39 ( 8.3%)<br />
1 : 171 (36.4%)<br />
2 : 237 (50.4%)<br />
3 : 21 ( 4.5%)<br />
4 : 1 ( 0.2%)<br />
5 : 1 ( 0.2%)</td>
<td><img src="tmp/ds0859.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="even">
<td>6</td>
<td>HSIZE<br />
[numeric]</td>
<td>Mean (sd) : 2.6 (1.3)<br />
min &lt; med &lt; max:<br />
1 &lt; 2 &lt; 7<br />
IQR (CV) : 2 (0.5)</td>
<td>1 : 104 (22.1%)<br />
2 : 147 (31.3%)<br />
3 : 96 (20.4%)<br />
4 : 96 (20.4%)<br />
5 : 20 ( 4.3%)<br />
6 : 5 ( 1.1%)<br />
7 : 2 ( 0.4%)</td>
<td><img src="tmp/ds0860.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="odd">
<td>7</td>
<td>AVADUAGE<br />
[numeric]</td>
<td>Mean (sd) : 37.8 (9.9)<br />
min &lt; med &lt; max:<br />
0 &lt; 36 &lt; 78<br />
IQR (CV) : 12.7 (0.3)</td>
<td>126 distinct values</td>
<td><img src="tmp/ds0861.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="even">
<td>8</td>
<td>IAGE<br />
[numeric]</td>
<td>Mean (sd) : 36.9 (11.6)<br />
min &lt; med &lt; max:<br />
0 &lt; 34 &lt; 78<br />
IQR (CV) : 15 (0.3)</td>
<td>53 distinct values</td>
<td><img src="tmp/ds0862.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="odd">
<td>9</td>
<td>ISEX<br />
[numeric]</td>
<td>Min : 0<br />
Mean : 0.5<br />
Max : 1</td>
<td>0 : 214 (45.5%)<br />
1 : 256 (54.5%)</td>
<td><img src="tmp/ds0863.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="even">
<td>10</td>
<td>NCARS<br />
[numeric]</td>
<td>Mean (sd) : 1.7 (0.9)<br />
min &lt; med &lt; max:<br />
0 &lt; 2 &lt; 5<br />
IQR (CV) : 1 (0.5)</td>
<td>0 : 23 ( 4.9%)<br />
1 : 182 (38.7%)<br />
2 : 193 (41.1%)<br />
3 : 56 (11.9%)<br />
4 : 13 ( 2.8%)<br />
5 : 3 ( 0.6%)</td>
<td><img src="tmp/ds0864.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="odd">
<td>11</td>
<td>AREA<br />
[numeric]</td>
<td>Mean (sd) : 133 (121.5)<br />
min &lt; med &lt; max:<br />
30 &lt; 110 &lt; 2250<br />
IQR (CV) : 60 (0.9)</td>
<td>76 distinct values</td>
<td><img src="tmp/ds0865.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="even">
<td>12</td>
<td>BEDROOM<br />
[numeric]</td>
<td>Mean (sd) : 2.9 (1.1)<br />
min &lt; med &lt; max:<br />
0 &lt; 3 &lt; 7<br />
IQR (CV) : 1 (0.4)</td>
<td>0 : 1 ( 0.2%)<br />
1 : 28 ( 6.0%)<br />
2 : 153 (32.6%)<br />
3 : 180 (38.3%)<br />
4 : 73 (15.5%)<br />
5 : 26 ( 5.5%)<br />
6 : 7 ( 1.5%)<br />
7 : 2 ( 0.4%)</td>
<td><img src="tmp/ds0866.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="odd">
<td>13</td>
<td>PARK<br />
[numeric]</td>
<td>Mean (sd) : 0.8 (1)<br />
min &lt; med &lt; max:<br />
0 &lt; 1 &lt; 4<br />
IQR (CV) : 1 (1.2)</td>
<td>0 : 224 (47.7%)<br />
1 : 136 (28.9%)<br />
2 : 84 (17.9%)<br />
3 : 18 ( 3.8%)<br />
4 : 8 ( 1.7%)</td>
<td><img src="tmp/ds0867.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="even">
<td>14</td>
<td>BEDSIZE<br />
[numeric]</td>
<td>Mean (sd) : 1.4 (0.8)<br />
min &lt; med &lt; max:<br />
0 &lt; 1 &lt; 5<br />
IQR (CV) : 0.7 (0.6)</td>
<td>22 distinct values</td>
<td><img src="tmp/ds0868.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="odd">
<td>15</td>
<td>PARKSIZE<br />
[numeric]</td>
<td>Mean (sd) : 0.5 (0.6)<br />
min &lt; med &lt; max:<br />
0 &lt; 0.2 &lt; 3<br />
IQR (CV) : 1 (1.2)</td>
<td>13 distinct values</td>
<td><img src="tmp/ds0869.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="even">
<td>16</td>
<td>RAGE10<br />
[numeric]</td>
<td>Min : 0<br />
Mean : 0.2<br />
Max : 1</td>
<td>0 : 356 (75.7%)<br />
1 : 114 (24.3%)</td>
<td><img src="tmp/ds0870.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="odd">
<td>17</td>
<td>TCBD<br />
[numeric]</td>
<td>Mean (sd) : 24.7 (16.2)<br />
min &lt; med &lt; max:<br />
0.8 &lt; 23.8 &lt; 73.3<br />
IQR (CV) : 25.7 (0.7)</td>
<td>434 distinct values</td>
<td><img src="tmp/ds0871.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="even">
<td>18</td>
<td>DISTHTC<br />
[numeric]</td>
<td>Mean (sd) : 1347 (1815.8)<br />
min &lt; med &lt; max:<br />
49 &lt; 719 &lt; 17732.7<br />
IQR (CV) : 1125 (1.3)</td>
<td>434 distinct values</td>
<td><img src="tmp/ds0872.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="odd">
<td>19</td>
<td>TWCBD<br />
[numeric]</td>
<td>Mean (sd) : 17 (16.2)<br />
min &lt; med &lt; max:<br />
0.3 &lt; 9.9 &lt; 67.8<br />
IQR (CV) : 20 (1)</td>
<td>439 distinct values</td>
<td><img src="tmp/ds0873.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="even">
<td>20</td>
<td>TDWWK<br />
[numeric]</td>
<td>Mean (sd) : 23.5 (17.1)<br />
min &lt; med &lt; max:<br />
0 &lt; 22.2 &lt; 80.7<br />
IQR (CV) : 23.6 (0.7)</td>
<td>414 distinct values</td>
<td><img src="tmp/ds0874.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="odd">
<td>21</td>
<td>HEADH<br />
[numeric]</td>
<td>Min : 0<br />
Mean : 0.9<br />
Max : 1</td>
<td>0 : 64 (13.6%)<br />
1 : 406 (86.4%)</td>
<td><img src="tmp/ds0875.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="even">
<td>22</td>
<td>POPDENS<br />
[numeric]</td>
<td>Mean (sd) : 92 (58.2)<br />
min &lt; med &lt; max:<br />
0 &lt; 83.2 &lt; 255.6<br />
IQR (CV) : 89.2 (0.6)</td>
<td>431 distinct values</td>
<td><img src="tmp/ds0876.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
<tr class="odd">
<td>23</td>
<td>EDUINDEX<br />
[numeric]</td>
<td>Mean (sd) : 0.2 (0.1)<br />
min &lt; med &lt; max:<br />
0 &lt; 0.2 &lt; 0.7<br />
IQR (CV) : 0.2 (0.6)</td>
<td>434 distinct values</td>
<td><img src="tmp/ds0877.png" /></td>
<td>0<br />
(0.0%)</td>
</tr>
</tbody>
</table>

> **Note:** I used a different library of the MLR chapter for perfoming
> the summary statistics. “R” allows you to do the same or similar tasks
> with different packages.

##### Take a look at the main characteristics of the dataset

``` r
class(df) #type of data
```

    ## [1] "data.frame"

``` r
str(df)
```

    ## 'data.frame':    470 obs. of  23 variables:
    ##  $ DWELCLAS: num  5 6 6 5 6 6 4 2 6 5 ...
    ##  $ INCOME  : num  7500 4750 4750 7500 2750 1500 12500 1500 1500 1500 ...
    ##  $ CHILD13 : num  1 0 2 0 1 0 0 0 0 0 ...
    ##  $ H18     : num  2 1 2 3 1 3 3 4 1 1 ...
    ##  $ HEMPLOY : num  2 1 2 2 1 2 0 2 1 1 ...
    ##  $ HSIZE   : num  3 1 4 4 2 3 3 4 1 1 ...
    ##  $ AVADUAGE: num  32 31 41.5 44.7 33 ...
    ##  $ IAGE    : num  32 31 42 52 33 47 62 21 34 25 ...
    ##  $ ISEX    : num  1 1 0 1 0 1 1 0 0 0 ...
    ##  $ NCARS   : num  2 1 2 3 1 1 2 3 1 1 ...
    ##  $ AREA    : num  100 90 220 120 90 100 178 180 80 50 ...
    ##  $ BEDROOM : num  2 2 4 3 2 2 5 3 2 1 ...
    ##  $ PARK    : num  1 1 2 0 0 0 2 0 0 1 ...
    ##  $ BEDSIZE : num  0.667 2 1 0.75 1 ...
    ##  $ PARKSIZE: num  0.5 1 1 0 0 0 1 0 0 1 ...
    ##  $ RAGE10  : num  1 0 1 0 0 0 0 0 1 1 ...
    ##  $ TCBD    : num  36.79 15.47 24.1 28.72 7.28 ...
    ##  $ DISTHTC : num  629 551 548 2351 698 ...
    ##  $ TWCBD   : num  10 15.5 12.71 3.17 5.36 ...
    ##  $ TDWWK   : num  31.1 0 20.4 32.9 13 ...
    ##  $ HEADH   : num  1 1 1 1 1 1 1 0 1 1 ...
    ##  $ POPDENS : num  85.7 146.4 106.6 36.8 181.6 ...
    ##  $ EDUINDEX: num  0.0641 0.2672 0.1 0.0867 0.1309 ...

> **Note:** When importing the dataset to R, some categorical variables
> were assigned as continuous variables, which is wrong. You should
> either assign them as categories, ou remove the variables.

##### Remove categorical variables or transform them into dummy (factor)

``` r
df = select(df,c(-DWELCLAS, -ISEX, -RAGE10, -HEADH))
```

\#Standardize variables (Zscore = xi - xmean)/sd

``` r
mean <- apply(df, 2, mean) # The "2" in the function is used to select the columns. MARGIN: c(1,2)
sd <- apply(df, 2, sd)
df_scaled <- data.frame(scale(df, mean, sd))
```

##### Take a look at the first values of the dataset

``` r
head(df_scaled,5)
```

    ##               INCOME    CHILD13        H18    HEMPLOY      HSIZE   AVADUAGE
    ## 799161661  1.0794858  0.7729638 -0.1241308  0.6432185  0.3332871 -0.5822216
    ## 798399409  0.1633759 -0.5107570 -1.2045288 -0.7124438 -1.2651308 -0.6828538
    ## 798374392  0.1633759  2.0566847 -0.1241308  0.6432185  1.1324961  0.3737844
    ## 798275277  1.0794858 -0.5107570  0.9562671  0.6432185  1.1324961  0.6924531
    ## 798264250 -0.5028859  0.7729638 -1.2045288 -0.7124438 -0.4659218 -0.4815894
    ##                 IAGE      NCARS       AREA    BEDROOM       PARK    BEDSIZE
    ## 799161661 -0.4215659  0.3285921 -0.2711861 -0.8178146  0.1760564 -0.8541923
    ## 798399409 -0.5078245 -0.7986947 -0.3534645 -0.8178146  0.1760564  0.8118945
    ## 798374392  0.4410200  0.3285921  0.7161546  1.0480730  1.2103874 -0.4376706
    ## 798275277  1.3036059  1.4558789 -0.1066293  0.1151292 -0.8582747 -0.7500619
    ## 798264250 -0.3353073 -0.7986947 -0.3534645 -0.8178146 -0.8582747 -0.4376706
    ##              PARKSIZE        TCBD    DISTHTC       TWCBD      TDWWK    POPDENS
    ## 799161661  0.05980121  0.74438167 -0.3953480 -0.43070769  0.4455778 -0.1082622
    ## 798399409  0.94365558 -0.56854349 -0.4385995 -0.09209531 -1.3766218  0.9352160
    ## 798374392  0.94365558 -0.03734795 -0.4400940 -0.26411662 -0.1839164  0.2509393
    ## 798275277 -0.82405316  0.24759440  0.5527119 -0.85160498  0.5508766 -0.9487325
    ## 798264250 -0.82405316 -1.07291599 -0.3572443 -0.71640988 -0.6136298  1.5398646
    ##             EDUINDEX
    ## 799161661 -1.0852110
    ## 798399409  0.5511548
    ## 798374392 -0.7960217
    ## 798275277 -0.9028004
    ## 798264250 -0.5467555

##### Compare the boxplots of the original dataset and standardized one

``` r
boxplot(df)
```

![](RmdFiles/3-FactorAnalysis/unnamed-chunk-10-1.png)<!-- -->

``` r
boxplot(df_scaled)
```

![](RmdFiles/3-FactorAnalysis/unnamed-chunk-10-2.png)<!-- -->

### Evaluating the assumptions for factorial analysis

**Adequate Sample Size:** Ideally, you should have at least 10
observations per variable for reliable factor analysis results. More is
generally better.

``` r
n_obs <- nrow(df_scaled)
n_vars <- ncol(df_scaled)
ratio=n_obs/n_vars

n_obs
```

    ## [1] 470

``` r
n_vars
```

    ## [1] 19

``` r
ratio
```

    ## [1] 24.73684

- **Normality**

``` r
shapiro.test(df_scaled$CHILD13)  # Test normality of each variable
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df_scaled$CHILD13
    ## W = 0.57099, p-value < 2.2e-16

Make normality tests to all variables

``` r
normality_tests <- sapply(df_scaled, function(x) {
  if (is.numeric(x)) {
    shapiro.test(x)
  } else {
    NA  # Skip non-numeric variables
  }
})
```

Print the p-values for each variable

``` r
normality_tests
```

    ##           INCOME                        CHILD13                      
    ## statistic 0.8261471                     0.5709899                    
    ## p.value   3.299416e-22                  2.331789e-32                 
    ## method    "Shapiro-Wilk normality test" "Shapiro-Wilk normality test"
    ## data.name "x"                           "x"                          
    ##           H18                           HEMPLOY                      
    ## statistic 0.8375837                     0.8244585                    
    ## p.value   1.590027e-21                  2.633268e-22                 
    ## method    "Shapiro-Wilk normality test" "Shapiro-Wilk normality test"
    ## data.name "x"                           "x"                          
    ##           HSIZE                         AVADUAGE                     
    ## statistic 0.8994548                     0.932016                     
    ## p.value   4.904234e-17                  8.595638e-14                 
    ## method    "Shapiro-Wilk normality test" "Shapiro-Wilk normality test"
    ## data.name "x"                           "x"                          
    ##           IAGE                          NCARS                        
    ## statistic 0.9353388                     0.8671536                    
    ## p.value   2.101209e-13                  1.41819e-19                  
    ## method    "Shapiro-Wilk normality test" "Shapiro-Wilk normality test"
    ## data.name "x"                           "x"                          
    ##           AREA                          BEDROOM                      
    ## statistic 0.3810604                     0.8987106                    
    ## p.value   4.702747e-37                  4.225014e-17                 
    ## method    "Shapiro-Wilk normality test" "Shapiro-Wilk normality test"
    ## data.name "x"                           "x"                          
    ##           PARK                          BEDSIZE                      
    ## statistic 0.7906915                     0.8056548                    
    ## p.value   3.957969e-24                  2.373473e-23                 
    ## method    "Shapiro-Wilk normality test" "Shapiro-Wilk normality test"
    ## data.name "x"                           "x"                          
    ##           PARKSIZE                      TCBD                         
    ## statistic 0.7774897                     0.9378631                    
    ## p.value   8.826135e-25                  4.232035e-13                 
    ## method    "Shapiro-Wilk normality test" "Shapiro-Wilk normality test"
    ## data.name "x"                           "x"                          
    ##           DISTHTC                       TWCBD                        
    ## statistic 0.6022255                     0.8209078                    
    ## p.value   1.970889e-31                  1.647601e-22                 
    ## method    "Shapiro-Wilk normality test" "Shapiro-Wilk normality test"
    ## data.name "x"                           "x"                          
    ##           TDWWK                         POPDENS                      
    ## statistic 0.9527661                     0.9670161                    
    ## p.value   4.122546e-11                  8.739157e-09                 
    ## method    "Shapiro-Wilk normality test" "Shapiro-Wilk normality test"
    ## data.name "x"                           "x"                          
    ##           EDUINDEX                     
    ## statistic 0.9392883                    
    ## p.value   6.337856e-13                 
    ## method    "Shapiro-Wilk normality test"
    ## data.name "x"

Or Extract p-values from the normality test results ONLY

``` r
normality_pvalues <- sapply(df_scaled, function(x) {
  if (is.numeric(x)) {
    shapiro.test(x)$p.value
  } else {
    NA  # Skip non-numeric variables
  }
})
```

# Print the p-values for each variable

``` r
normality_pvalues
```

    ##       INCOME      CHILD13          H18      HEMPLOY        HSIZE     AVADUAGE 
    ## 3.299416e-22 2.331789e-32 1.590027e-21 2.633268e-22 4.904234e-17 8.595638e-14 
    ##         IAGE        NCARS         AREA      BEDROOM         PARK      BEDSIZE 
    ## 2.101209e-13 1.418190e-19 4.702747e-37 4.225014e-17 3.957969e-24 2.373473e-23 
    ##     PARKSIZE         TCBD      DISTHTC        TWCBD        TDWWK      POPDENS 
    ## 8.826135e-25 4.232035e-13 1.970889e-31 1.647601e-22 4.122546e-11 8.739157e-09 
    ##     EDUINDEX 
    ## 6.337856e-13

If you want to focus on which variables do not follow normality, you can
filter the output:

``` r
non_normal_vars <- names(normality_pvalues[normality_pvalues < 0.05])
non_normal_vars
```

    ##  [1] "INCOME"   "CHILD13"  "H18"      "HEMPLOY"  "HSIZE"    "AVADUAGE"
    ##  [7] "IAGE"     "NCARS"    "AREA"     "BEDROOM"  "PARK"     "BEDSIZE" 
    ## [13] "PARKSIZE" "TCBD"     "DISTHTC"  "TWCBD"    "TDWWK"    "POPDENS" 
    ## [19] "EDUINDEX"

If you want to focus on which variables follow normality, you can filter
the output:

``` r
normal_vars <- names(normality_pvalues[normality_pvalues >= 0.05])
normal_vars
```

    ## character(0)

Example of histogram:

``` r
hist(df$H18)
```

![](RmdFiles/3-FactorAnalysis/unnamed-chunk-19-1.png)<!-- -->

> **Note:** There are no normally distributed variables in the dataset.
> Factor analysis can still work with non-normally distributed data, but
> for Maximum Likelihood (ML) extraction, normality is preferable.

- **Linearity Between Variables**

``` r
pairs(df_scaled[,1:10], pch = 19, lower.panel = NULL)  
```

![](RmdFiles/3-FactorAnalysis/unnamed-chunk-20-1.png)<!-- -->

``` r
# Pairwise scatter plots for the first 10 variables. Check the others!
```

You can also create an individual scatterplot between two variables:

``` r
plot(df_scaled$BEDROOM, df_scaled$H18, 
     main = "Scatterplot", 
     xlab = "BEDROOM", 
     ylab = "H18")
abline(a = 0, b = 1, col = "red", lty = 2)
```

![](RmdFiles/3-FactorAnalysis/unnamed-chunk-21-1.png)<!-- -->

> **Note:** Most relationships are non-linear.

**Correlations between variables**

``` r
#' Correlation matrix
corr_matrix <- cor(df_scaled, method = "pearson")
corrplot(corr_matrix, method = "square", type = "upper")
```

![](RmdFiles/3-FactorAnalysis/unnamed-chunk-22-1.png)<!-- -->

Check if the correlation is statistically significant.

``` r
cor.test(df_scaled$H18,df_scaled$HSIZE)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df_scaled$H18 and df_scaled$HSIZE
    ## t = 23.616, df = 468, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.6931681 0.7760734
    ## sample estimates:
    ##       cor 
    ## 0.7373854

The null hypothesis is that the correlation is zero. This means that the
correlations are only significant when you reject the null hypothesis
(pvalue \< 0.05).

**The Bartlett sphericity test**

This test checks if the correlation matrix is significantly different
from an identity matrix (where variables are uncorrelated). If
significant, the data is suitable for factor analysis. If the p-value is
small (p \< 0.05), you can reject the null hypothesis (which states that
the variables are uncorrelated), meaning that FA is appropriate.

``` r
cortest.bartlett(corr_matrix, n = nrow(df_scaled))
```

    ## $chisq
    ## [1] 4794.639
    ## 
    ## $p.value
    ## [1] 0
    ## 
    ## $df
    ## [1] 171

> **Note:** The null hypothesis is that there is no correlation between
> variables. Therefore, in factor analysis you want to reject the null
> hypothesis.

- **Check for sampling adequacy - KMO test**

It assesses whether the correlations between variables are high enough
to justify a factor analysis. It looks at the proportion of variance
that could be common among variables.

``` r
KMO(corr_matrix)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = corr_matrix)
    ## Overall MSA =  0.59
    ## MSA for each item = 
    ##   INCOME  CHILD13      H18  HEMPLOY    HSIZE AVADUAGE     IAGE    NCARS 
    ##     0.86     0.30     0.53     0.87     0.57     0.56     0.53     0.73 
    ##     AREA  BEDROOM     PARK  BEDSIZE PARKSIZE     TCBD  DISTHTC    TWCBD 
    ##     0.55     0.53     0.55     0.59     0.48     0.69     0.77     0.52 
    ##    TDWWK  POPDENS EDUINDEX 
    ##     0.70     0.79     0.75

> **Note:** We want at least 0.7 of the overall Mean Sample Adequacy
> (MSA). If, 0.6 \< MSA \< 0.7, it is not a good value, but acceptable
> in some cases.

MSA for each item (variable-specific KMO scores): The KMO test also
provides an MSA (Measure of Sampling Adequacy) for each variable,
indicating how well each variable fits with the others in terms of
common variance. Here are some important interpretations: \* Good MSA
values (≥ 0.70): Variables such as INCOME (0.86), HEMPLOY (0.87) are
well-suited for factor analysis and share sufficient common variance
with the other variables.

- Mediocre/bad MSA values (0.50 ≤ MSA \< 0.70): Variables like HSIZE
  (0.57), and PARK (0.55) are marginal for factor analysis. These
  variables have some shared variance with the other variables but are
  not as strong contributors.

- Low MSA values (\< 0.50): Variables like CHILD13 (0.30), and PARKSIZE
  (0.48) have very low MSA scores. These variables do not share enough
  common variance with the others and might be poorly suited for factor
  analysis.

You exclude the low MSA value variables:

``` r
new_df <- df_scaled |> select(-CHILD13, -PARKSIZE)
```

**Correlation matrix**

``` r
corr_matrix <- cor(new_df, method = "pearson")
KMO(corr_matrix)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = corr_matrix)
    ## Overall MSA =  0.67
    ## MSA for each item = 
    ##   INCOME      H18  HEMPLOY    HSIZE AVADUAGE     IAGE    NCARS     AREA 
    ##     0.85     0.85     0.88     0.64     0.56     0.55     0.81     0.74 
    ##  BEDROOM     PARK  BEDSIZE     TCBD  DISTHTC    TWCBD    TDWWK  POPDENS 
    ##     0.41     0.76     0.46     0.68     0.77     0.52     0.69     0.74 
    ## EDUINDEX 
    ##     0.76

- **Check for multicollinearity**. Multicollinearity happens when
  variables are very highly correlated (e.g., correlations above 0.9),
  which can distort factor analysis results.

``` r
vif(lm(df_scaled$INCOME ~ ., data = new_df))  # Replace INCOME with other variables
```

    ##      H18  HEMPLOY    HSIZE AVADUAGE     IAGE    NCARS     AREA  BEDROOM 
    ## 2.855273 1.605823 5.841001 1.761272 1.788480 1.828819 1.420604 4.418189 
    ##     PARK  BEDSIZE     TCBD  DISTHTC    TWCBD    TDWWK  POPDENS EDUINDEX 
    ## 1.348317 5.185469 2.332047 1.603804 1.376212 1.485086 1.484629 1.346856

> **Note:** If the VIF of any variable is greater than 10,
> multicollinearity may be an issue. Consider removing or combining
> highly correlated variables. May be the problem here is that VIF are
> too low!?

### Determine the number of factors to extract

**1. Parallel Analysis**

``` r
num_factors = fa.parallel(new_df, fm = "ml", fa = "fa")
```

![](RmdFiles/3-FactorAnalysis/unnamed-chunk-29-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  5  and the number of components =  NA

> **Note:** `fm` = factor math; `ml` = maximum likelihood; `fa` = factor
> analysis

The selection of the number of factors in the Parallel analysis can be
threefold:

- Detect where there is an “elbow” in the graph;
- Detect the intersection between the “FA Actual Data” and the “FA
  Simulated Data”;
- Consider the number of factors with eigenvalue \> 1.

**2. Kaiser Criterion**

``` r
sum(num_factors$fa.values > 1) #Determines the number of factors with eigenvalue > 1
```

    ## [1] 3

You can also consider factors with eigenvalue \> 0.7, since some of the
literature indicate that this value does not overestimate the number of
factors as much as considering an eigenvalue = 1.

**3. Principal Component Analysis (PCA)**

- Print variance that explains the components

``` r
df_pca <- princomp(new_df, cor=FALSE) #cor = TRUE, standardizes your dataset before running a PCA
summary(df_pca)  
```

    ## Importance of components:
    ##                           Comp.1    Comp.2    Comp.3     Comp.4     Comp.5
    ## Standard deviation     1.9370196 1.5918203 1.3929124 1.24631263 1.07780840
    ## Proportion of Variance 0.2211791 0.1493703 0.1143730 0.09156512 0.06847929
    ## Cumulative Proportion  0.2211791 0.3705494 0.4849224 0.57648757 0.64496685
    ##                            Comp.6     Comp.7     Comp.8     Comp.9    Comp.10
    ## Standard deviation     0.98383064 0.89742365 0.83885597 0.77390307 0.73480693
    ## Proportion of Variance 0.05705803 0.04747567 0.04148116 0.03530606 0.03182897
    ## Cumulative Proportion  0.70202488 0.74950056 0.79098172 0.82628777 0.85811674
    ##                           Comp.11    Comp.12    Comp.13    Comp.14    Comp.15
    ## Standard deviation     0.72482791 0.69586966 0.67614397 0.56328154 0.54655748
    ## Proportion of Variance 0.03097034 0.02854512 0.02694973 0.01870368 0.01760953
    ## Cumulative Proportion  0.88908708 0.91763220 0.94458193 0.96328562 0.98089515
    ##                           Comp.16     Comp.17
    ## Standard deviation     0.49692369 0.277773884
    ## Proportion of Variance 0.01455645 0.004548403
    ## Cumulative Proportion  0.99545160 1.000000000

- Scree Plot

``` r
plot(df_pca,type="lines", npcs = 17, las = 2) 
```

![](RmdFiles/3-FactorAnalysis/unnamed-chunk-32-1.png)<!-- -->

> **Note:** Check the cummulative variance of the first components and
> the scree plot, and see if the PCA is a good approach to detect the
> number of factors in this case.

**PCA is not the same thing as Factor Analysis!** PCA only considers the
common information (variance) of the variables, while factor analysis
takes into account also the unique variance of the variable. Both
approaches are often mixed up. In this example we use PCA as only a
first criteria for choosing the number of factors. PCA is very used in
image recognition and data reduction of big data.

## Exploratory Factor Analysis

- **Model 1**: No rotation
- **Model 2**: Rotation Varimax
- **Model 3**: Rotation Oblimin

``` r
# No rotation
df_factor <- factanal(new_df, factors = 4, rotation = "none", scores=c("regression"), fm = "ml")
# Rotation Varimax
df_factor_var <- factanal(new_df, factors = 4, rotation = "varimax", scores=c("regression"), fm = "ml")
# Rotiation Oblimin
df_factor_obl <- factanal(new_df, factors = 4, rotation = "oblimin", scores=c("regression"), fm = "ml")
```

Let’s print out the results, and have a look.

``` r
print(df_factor, digits=2, cutoff=0.3, sort=TRUE) #cutoff of 0.3 due to the sample size is higher than 350 observations.
```

    ## 
    ## Call:
    ## factanal(x = new_df, factors = 4, scores = c("regression"), rotation = "none",     fm = "ml")
    ## 
    ## Uniquenesses:
    ##   INCOME      H18  HEMPLOY    HSIZE AVADUAGE     IAGE    NCARS     AREA 
    ##     0.78     0.37     0.64     0.11     0.48     0.23     0.66     0.77 
    ##  BEDROOM     PARK  BEDSIZE     TCBD  DISTHTC    TWCBD    TDWWK  POPDENS 
    ##     0.00     0.89     0.12     0.13     0.66     0.81     0.73     0.81 
    ## EDUINDEX 
    ##     0.78 
    ## 
    ## Loadings:
    ##          Factor1 Factor2 Factor3 Factor4
    ## H18       0.70            0.38          
    ## HEMPLOY   0.52                          
    ## HSIZE     0.81            0.47          
    ## NCARS     0.53                          
    ## BEDSIZE  -0.89                          
    ## TCBD              0.92                  
    ## DISTHTC           0.58                  
    ## BEDROOM                   1.00          
    ## AVADUAGE                          0.70  
    ## IAGE                              0.85  
    ## INCOME    0.32                          
    ## AREA                      0.45          
    ## PARK                                    
    ## TWCBD             0.42                  
    ## TDWWK             0.47                  
    ## POPDENS          -0.42                  
    ## EDUINDEX         -0.46                  
    ## 
    ##                Factor1 Factor2 Factor3 Factor4
    ## SS loadings       2.70    2.07    1.92    1.35
    ## Proportion Var    0.16    0.12    0.11    0.08
    ## Cumulative Var    0.16    0.28    0.39    0.47
    ## 
    ## Test of the hypothesis that 4 factors are sufficient.
    ## The chi square statistic is 418.72 on 74 degrees of freedom.
    ## The p-value is 1.38e-49

``` r
print(df_factor_var, digits=2, cutoff=0.3, sort=TRUE) 
```

    ## 
    ## Call:
    ## factanal(x = new_df, factors = 4, scores = c("regression"), rotation = "varimax",     fm = "ml")
    ## 
    ## Uniquenesses:
    ##   INCOME      H18  HEMPLOY    HSIZE AVADUAGE     IAGE    NCARS     AREA 
    ##     0.78     0.37     0.64     0.11     0.48     0.23     0.66     0.77 
    ##  BEDROOM     PARK  BEDSIZE     TCBD  DISTHTC    TWCBD    TDWWK  POPDENS 
    ##     0.00     0.89     0.12     0.13     0.66     0.81     0.73     0.81 
    ## EDUINDEX 
    ##     0.78 
    ## 
    ## Loadings:
    ##          Factor1 Factor2 Factor3 Factor4
    ## H18       0.76                          
    ## HEMPLOY   0.56                          
    ## HSIZE     0.89                          
    ## NCARS     0.56                          
    ## BEDSIZE  -0.82            0.46          
    ## TCBD              0.92                  
    ## DISTHTC           0.58                  
    ## BEDROOM                   0.97          
    ## AVADUAGE                          0.72  
    ## IAGE                              0.88  
    ## INCOME    0.37                          
    ## AREA                      0.44          
    ## PARK                                    
    ## TWCBD             0.43                  
    ## TDWWK             0.48                  
    ## POPDENS          -0.42                  
    ## EDUINDEX         -0.46                  
    ## 
    ##                Factor1 Factor2 Factor3 Factor4
    ## SS loadings       2.91    2.09    1.59    1.44
    ## Proportion Var    0.17    0.12    0.09    0.08
    ## Cumulative Var    0.17    0.29    0.39    0.47
    ## 
    ## Test of the hypothesis that 4 factors are sufficient.
    ## The chi square statistic is 418.72 on 74 degrees of freedom.
    ## The p-value is 1.38e-49

``` r
print(df_factor_obl, digits=2, cutoff=0.3, sort=TRUE)
```

    ## 
    ## Call:
    ## factanal(x = new_df, factors = 4, scores = c("regression"), rotation = "oblimin",     fm = "ml")
    ## 
    ## Uniquenesses:
    ##   INCOME      H18  HEMPLOY    HSIZE AVADUAGE     IAGE    NCARS     AREA 
    ##     0.78     0.37     0.64     0.11     0.48     0.23     0.66     0.77 
    ##  BEDROOM     PARK  BEDSIZE     TCBD  DISTHTC    TWCBD    TDWWK  POPDENS 
    ##     0.00     0.89     0.12     0.13     0.66     0.81     0.73     0.81 
    ## EDUINDEX 
    ##     0.78 
    ## 
    ## Loadings:
    ##          Factor1 Factor2 Factor3 Factor4
    ## H18       0.73                          
    ## HEMPLOY   0.54                          
    ## HSIZE     0.85            0.31          
    ## NCARS     0.54                          
    ## BEDSIZE  -0.88            0.45          
    ## TCBD              0.93                  
    ## DISTHTC           0.59                  
    ## BEDROOM                   0.98          
    ## AVADUAGE                          0.72  
    ## IAGE                              0.88  
    ## INCOME    0.35                          
    ## AREA                      0.44          
    ## PARK                                    
    ## TWCBD             0.43                  
    ## TDWWK             0.48                  
    ## POPDENS          -0.43                  
    ## EDUINDEX         -0.46                  
    ## 
    ##                Factor1 Factor2 Factor3 Factor4
    ## SS loadings       2.78    2.11    1.62    1.41
    ## Proportion Var    0.16    0.12    0.10    0.08
    ## Cumulative Var    0.16    0.29    0.38    0.47
    ## 
    ## Factor Correlations:
    ##         Factor1 Factor2 Factor3 Factor4
    ## Factor1   1.000   0.112  -0.061   0.145
    ## Factor2   0.112   1.000  -0.079   0.052
    ## Factor3  -0.061  -0.079   1.000   0.022
    ## Factor4   0.145   0.052   0.022   1.000
    ## 
    ## Test of the hypothesis that 4 factors are sufficient.
    ## The chi square statistic is 418.72 on 74 degrees of freedom.
    ## The p-value is 1.38e-49

> **Note:** The variability contained in the factors = Communality +
> Uniqueness. You want uniquenesses below 0.3 (communality above 0.7).  
> Varimax assigns orthogonal rotation, and oblimin assigns oblique
> rotation.

Plot factor 3 against factor 4, and compare the results of different
rotations

- **No Rotation**

``` r
plot(
  df_factor$loadings[, 3],
  df_factor$loadings[, 4],
  xlab = "Factor 3",
  ylab = "Factor 4",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "No rotation"
)
abline(h = 0, v = 0)
load <- df_factor$loadings[, 3:4]
text(
  load,
  names(df),
  cex = .7,
  col = "blue"
)
```

![](RmdFiles/3-FactorAnalysis/unnamed-chunk-35-1.png)<!-- -->

- **Varimax rotation**

``` r
plot(
  df_factor_var$loadings[, 3],
  df_factor_var$loadings[, 4],
  xlab = "Factor 3",
  ylab = "Factor 4",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "Varimax rotation"
)
abline(h = 0, v = 0)
load <- df_factor_var$loadings[, 3:4]
text(
  load,
  labels = names(df),
  cex = .7,
  col = "red"
)
```

![](RmdFiles/3-FactorAnalysis/unnamed-chunk-36-1.png)<!-- -->

- **Oblimin Rotation**

``` r
plot(
  df_factor_obl$loadings[, 3],
  df_factor_obl$loadings[, 4],
  xlab = "Factor 3",
  ylab = "Factor 4",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "Oblimin rotation"
)
abline(h = 0, v = 0)
load <- df_factor_obl$loadings[, 3:4]
text(
  load,
  labels = names(df),
  cex = .7,
  col = "darkgreen"
)
```

![](RmdFiles/3-FactorAnalysis/unnamed-chunk-37-1.png)<!-- -->

When you have more than two factors it is difficult to analyze the
factors by the plots. Variables that have low explaining variance in the
two factors analyzed, could be highly explained by the other factors not
present in the graph. However, try comparing the plots with the factor
loadings and plot the other graphs to get more familiar with exploratory
factor analysis.
