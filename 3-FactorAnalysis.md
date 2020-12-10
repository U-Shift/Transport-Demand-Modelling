Exploratory Factor Analysis
================

#### EXAMPLE EXERCISE: “Residential location satisfaction in the Lisbon metropolitan area”

The aim of this study was to examine the perception of households
towards their residential location considering several land use and
accessibility factors as well as household socioeconomic and attitudinal
characteristics.

*Reference:* Martinez, L. G., de Abreu e Silva, J., & Viegas, J. M.
(2010). Assessment of residential location satisfaction in the Lisbon
metropolitan area, TRB (No. 10-1161).

**Your task:** Analyse the data and create meaningful latent factors.

## Data

#### Variables:

-   `DWELCLAS`: Classification of the dwelling;
-   `INCOME`: Income of the household;
-   `CHILD13`: Number of children under 13 years old;
-   `H18`: Number of household members above 18 years old;
-   `HEMPLOY`: Number of household members employed;
-   `HSIZE`: Household size;
-   `IAGE`: Age of the respondent;
-   `ISEX`: Sex of the respondent;
-   `NCARS`: Number of cars in the household;
-   `AREA`: Area of the dwelling;
-   `BEDROOM`: Number of bedrooms in the dwelling;
-   `PARK`: Number of parking spaces in the dwelling;
-   `BEDSIZE`: BEDROOM/HSIZE;
-   `PARKSIZE`: PARK/NCARS;
-   `RAGE10`: 1 if Dwelling age &lt;= 10;
-   `TCBD`: Private car distance in time to CBD;
-   `DISTTC`: Euclidean distance to heavy public transport system stops;
-   `TWCBD`: Private car distance in time of workplace to CBD;
-   `TDWWK`: Private car distance in time of dwelling to work place;
-   `HEADH`: 1 if Head of the Household;
-   `POPDENS`: Population density per hectare;
-   `EQUINDEX`: Number of undergraduate students/Population over 20
    years old (500m)

#### Rules of thumb:

-   At least 10 variables
-   n &lt; 50 (Unacceptable); n &gt; 200 (recommended)
-   It is recommended to use continuous variables. If your data contains
    categorical variables, you should transform them to dummy variables.

#### Assumptions:

-   Normality;
-   linearity;
-   Homogeneity;
-   Homoscedasticity (some multicollinearity is desirable);
-   Correlations between variables &lt; 0.3 (not appropriate to use
    Factor Analysis)

## Let’s start!

#### Import Libraries

``` r
library(foreign) # Library used to read SPSS files
library(nFactors) # Library used for factor analysis
library(tidyverse) # Library used in data science to perform exploratory data analysis
library(summarytools) # Library used for checking the summary of the dataset
library(psych) # Library used for factor analysis
library(GPArotation) # Library used for factor analysis
```

### Get to know your dataset

##### Import dataset

``` r
df <- read.spss("Data/example_fact.sav", to.data.frame = T) #transforms a list into a data.frame directly
```

##### Take a look at the main characteristics of the dataset

``` r
class(df) #type of data
```

    ## [1] "data.frame"

``` r
str(df)
```

    ## 'data.frame':    470 obs. of  32 variables:
    ##  $ RespondentID: num  7.99e+08 7.98e+08 7.98e+08 7.98e+08 7.98e+08 ...
    ##  $ DWELCLAS    : num  5 6 6 5 6 6 4 2 6 5 ...
    ##  $ INCOME      : num  7500 4750 4750 7500 2750 1500 12500 1500 1500 1500 ...
    ##  $ CHILD13     : num  1 0 2 0 1 0 0 0 0 0 ...
    ##  $ H18         : num  2 1 2 3 1 3 3 4 1 1 ...
    ##  $ HEMPLOY     : num  2 1 2 2 1 2 0 2 1 1 ...
    ##  $ HSIZE       : num  3 1 4 4 2 3 3 4 1 1 ...
    ##  $ AVADUAGE    : num  32 31 41.5 44.7 33 ...
    ##  $ IAGE        : num  32 31 42 52 33 47 62 21 34 25 ...
    ##  $ ISEX        : num  1 1 0 1 0 1 1 0 0 0 ...
    ##  $ NCARS       : num  2 1 2 3 1 1 2 3 1 1 ...
    ##  $ AREA        : num  100 90 220 120 90 100 178 180 80 50 ...
    ##  $ BEDROOM     : num  2 2 4 3 2 2 5 3 2 1 ...
    ##  $ PARK        : num  1 1 2 0 0 0 2 0 0 1 ...
    ##  $ BEDSIZE     : num  0.667 2 1 0.75 1 ...
    ##  $ PARKSIZE    : num  0.5 1 1 0 0 0 1 0 0 1 ...
    ##  $ RAGE10      : num  1 0 1 0 0 0 0 0 1 1 ...
    ##  $ TCBD        : num  36.79 15.47 24.1 28.72 7.28 ...
    ##  $ DISTHTC     : num  629 551 548 2351 698 ...
    ##  $ TWCBD       : num  10 15.5 12.71 3.17 5.36 ...
    ##  $ TDWWK       : num  31.1 0 20.4 32.9 13 ...
    ##  $ HEADH       : num  1 1 1 1 1 1 1 0 1 1 ...
    ##  $ POPDENS     : num  85.7 146.4 106.6 36.8 181.6 ...
    ##  $ EDUINDEX    : num  0.0641 0.2672 0.1 0.0867 0.1309 ...
    ##  $ GRAVCPC     : num  0.249 0.329 0.24 0.273 0.285 ...
    ##  $ GRAVCPT     : num  0.249 0.31 0.29 0.249 0.291 ...
    ##  $ GRAVPCPT    : num  1 1.062 0.826 1.099 0.98 ...
    ##  $ NSTRTC      : num  38 34 33 6 31 45 12 6 4 22 ...
    ##  $ DISTHW      : num  2036 748 2279 1196 3507 ...
    ##  $ DIVIDX      : num  0.323 0.348 0.324 0.327 0.355 ...
    ##  $ ACTDENS     : num  0.672 2.486 1.625 1.766 11.325 ...
    ##  $ DISTCBD     : num  9776 3524 11036 6257 1265 ...
    ##  - attr(*, "variable.labels")= Named chr(0) 
    ##   ..- attr(*, "names")= chr(0) 
    ##  - attr(*, "codepage")= int 1252

##### Check summary statistics of variables

``` r
descriptive_stats <- dfSummary(df)
view(descriptive_stats)
```

<!--html_preserve-->

<div class="container st-container">

<h3>
Data Frame Summary
</h3>
<h4>
df
</h4>
<strong>Dimensions</strong>: 470 x 32 <br/><strong>Duplicates</strong>:
0 <br/>

<div style="max-height:500px;overflow-y:scroll;margin:10px 2px">

<table class="table table-striped table-bordered st-table st-table-striped st-table-bordered st-multiline ">
<thead>
<tr>
<th align="center" class="st-protect-top-border">
<strong>No</strong>
</th>
<th align="center" class="st-protect-top-border">
<strong>Variable</strong>
</th>
<th align="center" class="st-protect-top-border">
<strong>Stats / Values</strong>
</th>
<th align="center" class="st-protect-top-border">
<strong>Freqs (% of Valid)</strong>
</th>
<th align="center" class="st-protect-top-border">
<strong>Graph</strong>
</th>
<th align="center" class="st-protect-top-border">
<strong>Missing</strong>
</th>
</tr>
</thead>
<tbody>
<tr>
<td align="center">
1
</td>
<td align="left">
RespondentID \[numeric\]
</td>
<td align="left">
Mean (sd) : 784082877.9 (8627500.7) min &lt; med &lt; max: 773001005
&lt; 780248282.5 &lt; 808234671 IQR (CV) : 16634784.8 (0)
</td>
<td align="left" style="vertical-align:middle">
470 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAIJJREFUaN7t18EJgDAMQNGuUDewbmD2382DpEJyaEwLgvx/Ekse3mJLoUxVa1pNZLFD7k4wMDAwMDBtC6yaOCbjbwYDAwMDAwMDAwP7Dut/vyswNQQMDAwM7G+Yros9jtmRB9OHF5gdAYtjfdevwNwLMDAwsCTWXBOYP7HXnakKZboAL6cmJpiqiDQAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjAtMTItMTBUMDM6MTk6MzcrMDA6MDBRkDGvAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIwLTEyLTEwVDAzOjE5OjM3KzAwOjAwIM2JEwAAAABJRU5ErkJggg==">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
2
</td>
<td align="left">
DWELCLAS \[numeric\]
</td>
<td align="left">
Mean (sd) : 5.1 (1.3) min &lt; med &lt; max: 1 &lt; 5 &lt; 7 IQR (CV) :
2 (0.2)
</td>
<td align="left" style="padding:0;vertical-align:middle">
<table style="border-collapse:collapse;border:none;margin:0">
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
5
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
1.1%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
2
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
14
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
3.0%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
3
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
31
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
6.6%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
4
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
75
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
16.0%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
5
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
130
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
27.7%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
6
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
162
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
34.5%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
7
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
53
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
11.3%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
</table>
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAEAAAACwBAMAAABN1XX0AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqb39/f///+DdZCQAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAIZJREFUWMPt08ENwCAMA0BWyAikG9D9d6OiwKsmUtWkPOz3PWLJSWmPqKAQEGwKxB+o5jU4TgKCUGBuUmAiH0fBnbPmFQKCOGBuUmC+/Yue/ARaix4Cgh2AOVqBcXmcGdRipBAQ+AFzkwF/8e5xWhY1764EBC7A3GTkXyAwWhQCghhgbvLvVHGL6Oy1K02MAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTEyLTEwVDAzOjE5OjM3KzAwOjAwUZAxrwAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMCDNiRMAAAAASUVORK5CYII=">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
3
</td>
<td align="left">
INCOME \[numeric\]
</td>
<td align="left">
Mean (sd) : 4259.6 (3001.8) min &lt; med &lt; max: 700 &lt; 2750 &lt;
12500 IQR (CV) : 2000 (0.7)
</td>
<td align="left" style="padding:0;vertical-align:middle">
<table style="border-collapse:collapse;border:none;margin:0">
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
700
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
20
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
4.3%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
1500
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
96
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
20.4%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
2750
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
142
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
30.2%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
4750
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
106
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
22.6%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
7500
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
75
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
16.0%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
12500
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
31
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
6.6%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
</table>
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAADoAAACYBAMAAABHQef+AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqb39/f///+DdZCQAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAHhJREFUWMPt08ERgCAMRFFaoARiB6T/3vSAMyCZVTTCZff6DuTwCWHdRCTFfkU3VSr1heKuoj0HlXap1ePmelSqi+LqptV+zr65LFOpN4q7+q/nwdrRD9VMpT5S3NWcnlOn9c1Uqpfi6r70jPX6oHEzlTqsuKsV2wG3JaTIy1XJ6gAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMFGQMa8AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjAtMTItMTBUMDM6MTk6MzcrMDA6MDAgzYkTAAAAAElFTkSuQmCC">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
4
</td>
<td align="left">
CHILD13 \[numeric\]
</td>
<td align="left">
Mean (sd) : 0.4 (0.8) min &lt; med &lt; max: 0 &lt; 0 &lt; 4 IQR (CV) :
0 (2)
</td>
<td align="left" style="padding:0;vertical-align:middle">
<table style="border-collapse:collapse;border:none;margin:0">
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
0
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
353
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
75.1%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
62
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
13.2%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
2
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
41
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
8.7%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
3
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
13
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
2.8%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
4
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
0.2%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
</table>
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAH0AAACABAMAAADUonvRAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqb39/f///+DdZCQAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAHRJREFUWMPt1LERgDAMQ9GMQDYAZ4Nk/91oOLijs1zgw1/9K6RCrRETs19+LC14PB7/tY/+XxezJfFPI83f+088Ho+v5aP/6VP5vFS+v/d3jo/H4/E/8NH/dMNk3uwI+bHweDy+po/+p4QTea09Ho/H4yvnBMxEjoVPF62pAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTEyLTEwVDAzOjE5OjM3KzAwOjAwUZAxrwAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMCDNiRMAAAAASUVORK5CYII=">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
5
</td>
<td align="left">
H18 \[numeric\]
</td>
<td align="left">
Mean (sd) : 2.1 (0.9) min &lt; med &lt; max: 0 &lt; 2 &lt; 6 IQR (CV) :
0.8 (0.4)
</td>
<td align="left" style="padding:0;vertical-align:middle">
<table style="border-collapse:collapse;border:none;margin:0">
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
0
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
0.2%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
112
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
23.8%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
2
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
239
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
50.8%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
3
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
77
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
16.4%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
4
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
35
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
7.4%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
5
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
3
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
0.6%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
6
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
3
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
0.6%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
</table>
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFkAAACwBAMAAACWbt/WAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqb39/f///+DdZCQAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAIJJREFUWMPt1MENgCAMRmFGkA0EN8D9dzPxZFTkb1RSkvfO36VN2hDGLUWlCY1Gu9US7qLzpSe9rKcKGo3+XtvuMkr501lqrm3wNjQa/U7b7tLnP0lNfdwJGo32oW1XHKX6fR9V7zspaDT6X227y5bzqpsDotFo51rC/BM0Gi3oEdsAJGJ6i77ycl4AAAAldEVYdGRhdGU6Y3JlYXRlADIwMjAtMTItMTBUMDM6MTk6MzcrMDA6MDBRkDGvAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIwLTEyLTEwVDAzOjE5OjM3KzAwOjAwIM2JEwAAAABJRU5ErkJggg==">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
6
</td>
<td align="left">
HEMPLOY \[numeric\]
</td>
<td align="left">
Mean (sd) : 1.5 (0.7) min &lt; med &lt; max: 0 &lt; 2 &lt; 5 IQR (CV) :
1 (0.5)
</td>
<td align="left" style="padding:0;vertical-align:middle">
<table style="border-collapse:collapse;border:none;margin:0">
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
0
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
39
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
8.3%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
171
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
36.4%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
2
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
237
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
50.4%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
3
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
21
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
4.5%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
4
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
0.2%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
5
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
0.2%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
</table>
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFgAAACYBAMAAACSUzOzAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqb39/f///+DdZCQAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAH9JREFUWMPt1lsKgCAQRmG34BIcd1D731t0gYIs54eUhHOevwcZmMEQxsy2UnzvwHleA4PBH2JpB6Or5tieSze8T6MYGAzugKWFlU5BO2yu6tM4m8BgsIClHfzJ3VCPTHLjXP/UgcFgDUs7WIddsOPBYDB4SOyyHBkwGHzBo7UA2AA1ch15KIsAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjAtMTItMTBUMDM6MTk6MzcrMDA6MDBRkDGvAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIwLTEyLTEwVDAzOjE5OjM3KzAwOjAwIM2JEwAAAABJRU5ErkJggg==">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
7
</td>
<td align="left">
HSIZE \[numeric\]
</td>
<td align="left">
Mean (sd) : 2.6 (1.3) min &lt; med &lt; max: 1 &lt; 2 &lt; 7 IQR (CV) :
2 (0.5)
</td>
<td align="left" style="padding:0;vertical-align:middle">
<table style="border-collapse:collapse;border:none;margin:0">
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
104
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
22.1%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
2
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
147
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
31.3%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
3
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
96
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
20.4%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
4
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
96
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
20.4%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
5
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
20
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
4.3%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
6
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
5
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
1.1%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
7
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
2
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
0.4%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
</table>
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAADwAAACwBAMAAAChoBDiAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqb39/f///+DdZCQAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAHJJREFUWMPt08ENgCAMRmFW6AjABrr/bh7URElKA4ol8b3rd4Hkbwi+pTLZOziv9xYY7mVja6L0Cicl5eXnB2C4mY2tDd152xnEgouPwXA/G1tj5/Af2PkMYoXz5bUw/IiNrYnSJ2cAw3OwjOQIw3OwVxtwK9Lty17s7gAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMFGQMa8AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjAtMTItMTBUMDM6MTk6MzcrMDA6MDAgzYkTAAAAAElFTkSuQmCC">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
8
</td>
<td align="left">
AVADUAGE \[numeric\]
</td>
<td align="left">
Mean (sd) : 37.8 (9.9) min &lt; med &lt; max: 0 &lt; 36 &lt; 78 IQR (CV)
: 12.7 (0.3)
</td>
<td align="left" style="vertical-align:middle">
126 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAHhJREFUaN7t17ENgCAARUFWwA3UDWT/3SwUGkNAxcTiXkVCcqGg+SHoSbHakovtmtiajjYYDAaDwWAwGAwGu4FNeZgMwU4jfYN1jKh+LLWfCIPBYDAYDAaDwX6E5T0wj8DyAQarYNcB+gIrNzBYBSsf7sSWIQU9aQfIfu5cRZbNfgAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMFGQMa8AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjAtMTItMTBUMDM6MTk6MzcrMDA6MDAgzYkTAAAAAElFTkSuQmCC">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
9
</td>
<td align="left">
IAGE \[numeric\]
</td>
<td align="left">
Mean (sd) : 36.9 (11.6) min &lt; med &lt; max: 0 &lt; 34 &lt; 78 IQR
(CV) : 15 (0.3)
</td>
<td align="left" style="vertical-align:middle">
53 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAIFJREFUaN7t18EJwCAMQNGsYDeobtDsv5uHNl6sJKhQKP+fBOkjeBArQjOlYdlKfi5W9O4C87DDzn0L9hgKBgYGBgYGBgYGBgYG9gss8LMex9QfEQwMDAxsD2bX97kDswUYGBgY2AvWv5gXsLYDBvYFlrsWsDL8NoC1AYaTzSQ0UwWhRSGej+VBgwAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMFGQMa8AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjAtMTItMTBUMDM6MTk6MzcrMDA6MDAgzYkTAAAAAElFTkSuQmCC">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
10
</td>
<td align="left">
ISEX \[numeric\]
</td>
<td align="left">
Min : 0 Mean : 0.5 Max : 1
</td>
<td align="left" style="padding:0;vertical-align:middle">
<table style="border-collapse:collapse;border:none;margin:0">
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
0
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
214
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
45.5%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
256
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
54.5%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
</table>
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAF4AAAA4BAMAAACGUVIaAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqb39/f///+DdZCQAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAEFJREFUSMdjYBj6QIkgUBQEA6h6ZWNCYFT9qPpR9YNVPan5XZBYMEjVE/YvFBAdnhBgNKp+VP2o+kGnntT8PpQBAGj/Uro4JOe+AAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTEyLTEwVDAzOjE5OjM3KzAwOjAwUZAxrwAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMCDNiRMAAAAASUVORK5CYII=">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
11
</td>
<td align="left">
NCARS \[numeric\]
</td>
<td align="left">
Mean (sd) : 1.7 (0.9) min &lt; med &lt; max: 0 &lt; 2 &lt; 5 IQR (CV) :
1 (0.5)
</td>
<td align="left" style="padding:0;vertical-align:middle">
<table style="border-collapse:collapse;border:none;margin:0">
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
0
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
23
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
4.9%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
182
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
38.7%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
2
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
193
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
41.1%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
3
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
56
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
11.9%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
4
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
13
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
2.8%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
5
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
3
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
0.6%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
</table>
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAEoAAACYBAMAAACxCGJmAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqb39/f///+DdZCQAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAHdJREFUWMPt07ENgDAMRFFW8AhxNoj33w0KhGhiHUVQCP/Xr0nk27Z58yPrd6oa0VCodZR295Y3QHlWub2xHwq1qNLWMdlq3ZU3RkOhPqa0u39/jw9XWzJ1/QQKtYTS7t7yhuyxCKoGCvU/pa3D8kasFoVCddSM7T0S8xUksAJPAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTEyLTEwVDAzOjE5OjM3KzAwOjAwUZAxrwAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMCDNiRMAAAAASUVORK5CYII=">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
12
</td>
<td align="left">
AREA \[numeric\]
</td>
<td align="left">
Mean (sd) : 133 (121.5) min &lt; med &lt; max: 30 &lt; 110 &lt; 2250 IQR
(CV) : 60 (0.9)
</td>
<td align="left" style="vertical-align:middle">
76 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAFpJREFUaN7t2bENwCAMRUGvwAiBDcj+u6EQJnCQkKJ7hcurfucIZaqzq3xrYe1+gsFgMBgMBoPBYDAYDAaDwWAwGAwGg8FgZ7D3/bcJm7fD/oTlB7KwuqVQpgFlTTR4U1FpLwAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMFGQMa8AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjAtMTItMTBUMDM6MTk6MzcrMDA6MDAgzYkTAAAAAElFTkSuQmCC">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
13
</td>
<td align="left">
BEDROOM \[numeric\]
</td>
<td align="left">
Mean (sd) : 2.9 (1.1) min &lt; med &lt; max: 0 &lt; 3 &lt; 7 IQR (CV) :
1 (0.4)
</td>
<td align="left" style="padding:0;vertical-align:middle">
<table style="border-collapse:collapse;border:none;margin:0">
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
0
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
0.2%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
28
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
6.0%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
2
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
153
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
32.6%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
3
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
180
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
38.3%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
4
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
73
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
15.5%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
5
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
26
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
5.5%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
6
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
7
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
1.5%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
7
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
2
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
0.4%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
</table>
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAEYAAADKBAMAAADqcisUAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqb39/f///+DdZCQAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAIpJREFUWMPt1bsNwCAMRVFW8AgOG8D+u0WkivLBbgJGubc+DdKzSCleKp0wmJ8aGWi2lmFyrbVgMAsYz56l16wbfEgvb38Kg4lmPHuOcF9HxrtaBYMJZDx7jvzHvZnT2wsGE9l49iy9PrgvNUx7FwazgvHsWXrNuEG1TcZgFjGePYuMM4rBYG4mUjtIKjxIp0BJ6gAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMFGQMa8AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjAtMTItMTBUMDM6MTk6MzcrMDA6MDAgzYkTAAAAAElFTkSuQmCC">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
14
</td>
<td align="left">
PARK \[numeric\]
</td>
<td align="left">
Mean (sd) : 0.8 (1) min &lt; med &lt; max: 0 &lt; 1 &lt; 4 IQR (CV) : 1
(1.2)
</td>
<td align="left" style="padding:0;vertical-align:middle">
<table style="border-collapse:collapse;border:none;margin:0">
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
0
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
224
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
47.7%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
136
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
28.9%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
2
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
84
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
17.9%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
3
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
18
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
3.8%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
4
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
8
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
1.7%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
</table>
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFQAAACABAMAAABn6lPLAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqb39/f///+DdZCQAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAHRJREFUWMPt1EEKgDAMRNFeIUeovUG9/93cCCJEO6FGKPxZv0USmJSyVjYhJ237MB0Khca7ZUJSqT+dS90LdCgUOksDNTQh/72M+kpvF4BCoZ/RQA1NSPbLqCJtj3tDodAZGqjhiKXT8aTXBaBQqE4D3VolByTa6P1UIB26AAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTEyLTEwVDAzOjE5OjM3KzAwOjAwUZAxrwAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMCDNiRMAAAAASUVORK5CYII=">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
15
</td>
<td align="left">
BEDSIZE \[numeric\]
</td>
<td align="left">
Mean (sd) : 1.4 (0.8) min &lt; med &lt; max: 0 &lt; 1 &lt; 5 IQR (CV) :
0.7 (0.6)
</td>
<td align="left" style="vertical-align:middle">
22 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAIdJREFUaN7t2MEJgDAMQNGuUDdQN2j3300w8RAIJY3Qg/5/EUEfEdoeLIUyVemQ9prLYme/AwMDAwMDAwMDAwMDAwMDAwMDA1uEZX/wuVh2QDCwBDa9dkfY9IBgYGD/wfS0GWCbPhLAXMVictPAvof5CyWA6XsWc+cMYHJpq7Dno+12elmhTBf9u4kf58lJ1gAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMFGQMa8AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjAtMTItMTBUMDM6MTk6MzcrMDA6MDAgzYkTAAAAAElFTkSuQmCC">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
16
</td>
<td align="left">
PARKSIZE \[numeric\]
</td>
<td align="left">
Mean (sd) : 0.5 (0.6) min &lt; med &lt; max: 0 &lt; 0.2 &lt; 3 IQR (CV)
: 1 (1.2)
</td>
<td align="left" style="vertical-align:middle">
13 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAG5JREFUaN7t2bENgCAQQFFW0A3EDWT/3WyIXqEJAY0xeb8iV7xAy6WknnJo6q5iaznaYDAYDAaDwWAwGAwGg8H+ioX/kmUYO48FBoPBYDAYDAaDXWNzw9ayHQvj7XssPm0Yuxm/hsXLVyw/UlJPO5IQx/b/G6/SAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTEyLTEwVDAzOjE5OjM3KzAwOjAwUZAxrwAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMCDNiRMAAAAASUVORK5CYII=">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
17
</td>
<td align="left">
RAGE10 \[numeric\]
</td>
<td align="left">
Min : 0 Mean : 0.2 Max : 1
</td>
<td align="left" style="padding:0;vertical-align:middle">
<table style="border-collapse:collapse;border:none;margin:0">
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
0
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
356
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
75.7%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
114
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
24.3%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
</table>
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAH4AAAA4BAMAAADJDFHKAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqb39/f///+DdZCQAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMlw+HV3QAAAEJJREFUSMdjYBgFSuQCqH5lY/KA0aj+Uf2j+kf1D3H9lJafguSCYaIfM2AUSdKPGX+j+kf1j+of1T8y9FNafo5kAADP3oW6Oxl62QAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMFGQMa8AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjAtMTItMTBUMDM6MTk6MzcrMDA6MDAgzYkTAAAAAElFTkSuQmCC">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
18
</td>
<td align="left">
TCBD \[numeric\]
</td>
<td align="left">
Mean (sd) : 24.7 (16.2) min &lt; med &lt; max: 0.8 &lt; 23.8 &lt; 73.3
IQR (CV) : 25.7 (0.7)
</td>
<td align="left" style="vertical-align:middle">
434 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAIlJREFUaN7t2EsKwCAMRdFsoUuo3UHd/96cmAwapBID/XDfSAgeIgh+REgkRbNvC+nYUXvAwMDAwL6N2emQgWnlDGCukRXMzQUDu2C631KwYatgYGBgYGBgYGBgYNnYxCt2Hqv3LT6Njb90A5gOwMDAwMDAfo75+8ICZpX3YrbeDMwqHSspERJJA82nnOg9BuSuAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTEyLTEwVDAzOjE5OjM3KzAwOjAwUZAxrwAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0xMi0xMFQwMzoxOTozNyswMDowMCDNiRMAAAAASUVORK5CYII=">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
19
</td>
<td align="left">
DISTHTC \[numeric\]
</td>
<td align="left">
Mean (sd) : 1347 (1815.8) min &lt; med &lt; max: 49 &lt; 719 &lt;
17732.7 IQR (CV) : 1125 (1.3)
</td>
<td align="left" style="vertical-align:middle">
434 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAGhJREFUaN7t2bENgDAMRUGvwAiEDWD/3SgwJUgxUSSke02665zmR6hSy9blS4ltxxUMBoPBYDAYDAaDwWAwGAwGg8FgMBjsT9g9GQ3B8t1hsBlYbe58wGrXNRHrXHffsc7PJLE2pFClEyxlXGPK7gcjAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTEyLTEwVDAzOjE5OjM4KzAwOjAwp9hBRgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0xMi0xMFQwMzoxOTozOCswMDowMNaF+foAAAAASUVORK5CYII=">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
20
</td>
<td align="left">
TWCBD \[numeric\]
</td>
<td align="left">
Mean (sd) : 17 (16.2) min &lt; med &lt; max: 0.3 &lt; 9.9 &lt; 67.8 IQR
(CV) : 20 (1)
</td>
<td align="left" style="vertical-align:middle">
439 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAKxJREFUaN7t2FsKgCAQhWG3YDuodqD731spoV2kvAxU+J+HIYQ+RkEfRilSE+0yuYy6PntstmvAwHrE/E2SwrwDBgYGBgYGBgYGBgYGlom1DDcuWEt7YGBgfWPlj9ENVt4jGNhnMH8XpDD3Zb6GhWmwydhrGosD5VCMfW4vjSXKv7DhdI5t2GmtEDu20ogd/+4Ki+cogMW117GwLwksilJYypbCfNmwSSSK1GQB6lHGWNJhqtIAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjAtMTItMTBUMDM6MTk6MzgrMDA6MDCn2EFGAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIwLTEyLTEwVDAzOjE5OjM4KzAwOjAw1oX5+gAAAABJRU5ErkJggg==">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
21
</td>
<td align="left">
TDWWK \[numeric\]
</td>
<td align="left">
Mean (sd) : 23.5 (17.1) min &lt; med &lt; max: 0 &lt; 22.2 &lt; 80.7 IQR
(CV) : 23.6 (0.7)
</td>
<td align="left" style="vertical-align:middle">
414 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAIJJREFUaN7t2DEOgCAMQFGu4BGoN5D7383BdqiLSGtimv8XAsNLSICB1mgl0foWSbF9XIGBgaVhdkdTMB0PsEqY+HoI89PZswwGBgYGBgYGBgYGBgYGVgSb/IiZw2wZDAwMDOxr7OH5focNj/YUTEewStj92IUwW/4vZttVTFJqtNIJJPd1xKW/FjUAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjAtMTItMTBUMDM6MTk6MzgrMDA6MDCn2EFGAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIwLTEyLTEwVDAzOjE5OjM4KzAwOjAw1oX5+gAAAABJRU5ErkJggg==">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
22
</td>
<td align="left">
HEADH \[numeric\]
</td>
<td align="left">
Min : 0 Mean : 0.9 Max : 1
</td>
<td align="left" style="padding:0;vertical-align:middle">
<table style="border-collapse:collapse;border:none;margin:0">
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
0
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
64
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
13.6%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
<tr style="background-color:transparent">
<td style="padding:0 0 0 7px;margin:0;border:0" align="right">
1
</td>
<td style="padding:0 2px;border:0;" align="left">
:
</td>
<td style="padding:0 4px 0 6px;margin:0;border:0" align="right">
406
</td>
<td style="padding:0;border:0" align="left">
(
</td>
<td style="padding:0 2px;margin:0;border:0" align="right">
86.4%
</td>
<td style="padding:0 4px 0 0;border:0" align="left">
)
</td>
</tr>
</table>
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAI4AAAA4BAMAAADZQN1TAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqb39/f///+DdZCQAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAExJREFUSMdjYBgFhIASHCgKkgWg5igbw8CoOaPmjJozas6oOYPJHGqV8+RpHjnmKFEKMOKdPGA0as6oOaPmjJozag5NzKFWOT8KcAMAXKGfOiu8M1UAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjAtMTItMTBUMDM6MTk6MzgrMDA6MDCn2EFGAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIwLTEyLTEwVDAzOjE5OjM4KzAwOjAw1oX5+gAAAABJRU5ErkJggg==">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
23
</td>
<td align="left">
POPDENS \[numeric\]
</td>
<td align="left">
Mean (sd) : 92 (58.2) min &lt; med &lt; max: 0 &lt; 83.2 &lt; 255.6 IQR
(CV) : 89.2 (0.6)
</td>
<td align="left" style="vertical-align:middle">
431 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAALZJREFUaN7t2F0KgCAQRlG3UDsod5D731s1QoZkNTZC0f0ehOnh0A+Tg86RmnRZRkmnTAHzYckEBgYGpsPkPzQYYesSwMDMsHyffITl7fM/TNHt15i/f3vtsPREBlh64WBgX8KkDawwvy1gYGDNsP7qFECFycX3Yiezhx4L5Q9yE4uTpRGWajAwMLAa7PhQuBKLNRgY2E+wfjdiPcekfhu2DW0WWKrBwApY3JSNsFi3wEaTOFKTGXxZnbCXBh8jAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTEyLTEwVDAzOjE5OjM4KzAwOjAwp9hBRgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0xMi0xMFQwMzoxOTozOCswMDowMNaF+foAAAAASUVORK5CYII=">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
24
</td>
<td align="left">
EDUINDEX \[numeric\]
</td>
<td align="left">
Mean (sd) : 0.2 (0.1) min &lt; med &lt; max: 0 &lt; 0.2 &lt; 0.7 IQR
(CV) : 0.2 (0.6)
</td>
<td align="left" style="vertical-align:middle">
434 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAKlJREFUaN7t1sENgCAQRFFawA7UDqT/3hQ8aCKiuxBdzZ8D4fQyATbBOaKJjxlieq/PHhvDErCXsZorPWA19cDAwMDAwMD+iyk/HHlMWe+TmPzgCpi8HhgY2GOYaNqvMFE9MDAzWBqDVlhcJjAwW1h6462womMduzHt97FwXQ8MzCBWHgMhVq4HBmYZ63KzoMZyHTVYKrXt6rAQTo7w99h6uftfUHUc0WQG4hYUA81LLPoAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjAtMTItMTBUMDM6MTk6MzgrMDA6MDCn2EFGAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIwLTEyLTEwVDAzOjE5OjM4KzAwOjAw1oX5+gAAAABJRU5ErkJggg==">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
25
</td>
<td align="left">
GRAVCPC \[numeric\]
</td>
<td align="left">
Mean (sd) : 0.3 (0.1) min &lt; med &lt; max: 0.1 &lt; 0.3 &lt; 0.4 IQR
(CV) : 0.1 (0.2)
</td>
<td align="left" style="vertical-align:middle">
433 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAHpJREFUaN7t17ENgCAQQFFW0BF0A91/Nys9CzTxoND4fkUoXgEEQinKNDxtnI5iMo2tewsMBoPBYDAYDAaDwWBvwk7/3w5YELAWLDalAxYjGAwG+xVWe+DS2HxLfByLleqAHcQKg8Fg11jl4sljFQL2Z+z2dOWxlooybVOQL22Aj76yAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTEyLTEwVDAzOjE5OjM4KzAwOjAwp9hBRgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0xMi0xMFQwMzoxOTozOCswMDowMNaF+foAAAAASUVORK5CYII=">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
26
</td>
<td align="left">
GRAVCPT \[numeric\]
</td>
<td align="left">
Mean (sd) : 0.3 (0.1) min &lt; med &lt; max: 0 &lt; 0.3 &lt; 0.4 IQR
(CV) : 0.1 (0.2)
</td>
<td align="left" style="vertical-align:middle">
434 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAIhJREFUaN7t2MENgCAMQNGuoCPABrD/bhy0PUh6aWuMyf9HSV88EIiKUKQjUNPsSQLr82qAgYGBgYF9jZ16w5VgtzHBwMDAwMACmF1KrQJTY4CBgYGB1WLbaZ3B+tMAAwMDAwMD+w+2/1nMYNsImIf5H88BrLsjPmYvUIHZyhtYc/doBkslFGkBq4MiUp+saxcAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjAtMTItMTBUMDM6MTk6MzgrMDA6MDCn2EFGAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIwLTEyLTEwVDAzOjE5OjM4KzAwOjAw1oX5+gAAAABJRU5ErkJggg==">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
27
</td>
<td align="left">
GRAVPCPT \[numeric\]
</td>
<td align="left">
Mean (sd) : 1.2 (0.3) min &lt; med &lt; max: 0.5 &lt; 1.1 &lt; 2.9 IQR
(CV) : 0.2 (0.3)
</td>
<td align="left" style="vertical-align:middle">
434 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAJBJREFUaN7t2dEJgCAUQFFX0A2qDXT/3aonZAqimRjSvR9BHx3IhESVopZ00irph2WwzR1ZMDAwMDAwMDAwMDAwMDAwMDCwjzDZ7OqFiQMGNg6T+bt0ws6LAwMDA/s5ZsLKIMXqTsgizF1OitX9EwZjt1VR6V3LWHJvZ8b8p++E5R+eEjNhqvhhisbsbYpa2gECvG61R51QJwAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMC0xMi0xMFQwMzoxOTozOCswMDowMKfYQUYAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjAtMTItMTBUMDM6MTk6MzgrMDA6MDDWhfn6AAAAAElFTkSuQmCC">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
28
</td>
<td align="left">
NSTRTC \[numeric\]
</td>
<td align="left">
Mean (sd) : 22.2 (12.6) min &lt; med &lt; max: 0 &lt; 22 &lt; 84 IQR
(CV) : 16 (0.6)
</td>
<td align="left" style="vertical-align:middle">
59 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAINJREFUaN7t2UEKgCAQQFGv4BGauUHe/25BjYsiTQcXWf9vBMGHSIFgCOQpWmJFVxdM094KBjYdZj/CMgQ7hgQGBgYGBgYGNg0mfbehOqZ9O/wXlk96CKbn1WBgYGBgYGBgYGBgYO/FGp+j2rA8DQZWwB6+tz4s1Xf4NUzuM0yGFMjTBvHhIvZ4ZI2gAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTEyLTEwVDAzOjE5OjM4KzAwOjAwp9hBRgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0xMi0xMFQwMzoxOTozOCswMDowMNaF+foAAAAASUVORK5CYII=">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
29
</td>
<td align="left">
DISTHW \[numeric\]
</td>
<td align="left">
Mean (sd) : 1883.4 (1748.3) min &lt; med &lt; max: 74.7 &lt; 1338.7 &lt;
16590.1 IQR (CV) : 1820.5 (0.9)
</td>
<td align="left" style="vertical-align:middle">
434 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAG1JREFUaN7t2b0NgCAQgFFWcARhA91/NwvPUiM/Jmre1xCalysJl5JaytE89RRYWfdgMBgMBoPBYDAYDAaDwWCwn2HHP9oQLM4FBoPBYDAY7EmsbQ94gsX1vVjl2vMaK3UTfh27+dQNLA8pqaUNC3d4ThEcWDAAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjAtMTItMTBUMDM6MTk6MzgrMDA6MDCn2EFGAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIwLTEyLTEwVDAzOjE5OjM4KzAwOjAw1oX5+gAAAABJRU5ErkJggg==">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
30
</td>
<td align="left">
DIVIDX \[numeric\]
</td>
<td align="left">
Mean (sd) : 0.4 (0.1) min &lt; med &lt; max: 0.3 &lt; 0.4 &lt; 0.6 IQR
(CV) : 0.1 (0.2)
</td>
<td align="left" style="vertical-align:middle">
144 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAIpJREFUaN7t2cENgCAMQFFWYARxA7v/bh6kPUiMWHsg6f8XiSQvcCAmUgp5qlrTqqM7tsvVAQYGBgaWF9MPyxaB9aeAgYGBgYGBgYGBgYGBgYGBgfkwuwYYsC8XBB1TQwbMXqyLTex3HpP3JYIlx57/kzowHYRi45H4gdnMupjtNwKzmVxYC6mQpxNnQgP6SfXMJgAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMC0xMi0xMFQwMzoxOTozOCswMDowMKfYQUYAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjAtMTItMTBUMDM6MTk6MzgrMDA6MDDWhfn6AAAAAElFTkSuQmCC">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
31
</td>
<td align="left">
ACTDENS \[numeric\]
</td>
<td align="left">
Mean (sd) : 5.8 (8.5) min &lt; med &lt; max: 0 &lt; 2.5 &lt; 63.2 IQR
(CV) : 3.5 (1.5)
</td>
<td align="left" style="vertical-align:middle">
144 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAHZJREFUaN7t2csNgCAQQEFasASxA+2/N5UYPfhHYkycd9jjBAK3DUE5xVT1sAlruqEWBoPBYDAYDAaDwWAwGAwGg8FgMBgMBvscltZidSFsHN2fsLyt4g6W97rvYctXuXbtQ+zuSddYOkMprJnH9rXPsVikoJx6yE87GiDeenYAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjAtMTItMTBUMDM6MTk6MzgrMDA6MDCn2EFGAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIwLTEyLTEwVDAzOjE5OjM4KzAwOjAw1oX5+gAAAABJRU5ErkJggg==">
</td>
<td align="center">
0 (0%)
</td>
</tr>
<tr>
<td align="center">
32
</td>
<td align="left">
DISTCBD \[numeric\]
</td>
<td align="left">
Mean (sd) : 7967.4 (7442.9) min &lt; med &lt; max: 148.9 &lt; 5542.3
&lt; 44004.6 IQR (CV) : 9777.9 (0.9)
</td>
<td align="left" style="vertical-align:middle">
434 distinct values
</td>
<td align="left" style="vertical-align:middle;padding:0;background-color:transparent">
<img style="border:none;background-color:transparent;padding:0" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAJgAAABuBAMAAAApJ8cWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqby8vL///8shn5hAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5AwKAxMmWuiEZwAAAIRJREFUaN7t2cEJwCAMheGs0BGqG9T9d/PQeIw1aaBI/3cJePgIiCJRhERSNOfxJorVdgcMDAwMDAwMDAwMDAwMDAwMLBsbY5wUTOsFBrYdFptnGljsqIKBgf0ec36tzLHq6xBsC2zx2baGjWWwj7GHXfVhbd5hDDNuphim1cBKSoRE0gEgS65qizIxEwAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMC0xMi0xMFQwMzoxOTozOCswMDowMKfYQUYAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjAtMTItMTBUMDM6MTk6MzgrMDA6MDDWhfn6AAAAAElFTkSuQmCC">
</td>
<td align="center">
0 (0%)
</td>
</tr>
</tbody>
</table>

</div>

<p>
Generated by
<a href='https://github.com/dcomtois/summarytools'>summarytools</a>
0.9.6 (<a href='https://www.r-project.org/'>R</a> version
4.0.3)<br/>2020-12-10
</p>

</div>

<!--/html_preserve-->

> **Note:** I used a different library of the MLR chapter for perfoming
> the summary statistics. “R” allows you to do the same or similar tasks
> with different packages.

##### Take a look at the first values of the dataset

``` r
head(df,5)
```

    ##   RespondentID DWELCLAS INCOME CHILD13 H18 HEMPLOY HSIZE AVADUAGE IAGE ISEX
    ## 1    799161661        5   7500       1   2       2     3 32.00000   32    1
    ## 2    798399409        6   4750       0   1       1     1 31.00000   31    1
    ## 3    798374392        6   4750       2   2       2     4 41.50000   42    0
    ## 4    798275277        5   7500       0   3       2     4 44.66667   52    1
    ## 5    798264250        6   2750       1   1       1     2 33.00000   33    0
    ##   NCARS AREA BEDROOM PARK   BEDSIZE PARKSIZE RAGE10      TCBD   DISTHTC
    ## 1     2  100       2    1 0.6666667      0.5      1 36.791237  629.1120
    ## 2     1   90       2    1 2.0000000      1.0      0 15.472989  550.5769
    ## 3     2  220       4    2 1.0000000      1.0      1 24.098125  547.8633
    ## 4     3  120       3    0 0.7500000      0.0      0 28.724796 2350.5782
    ## 5     1   90       2    0 1.0000000      0.0      0  7.283384  698.3000
    ##       TWCBD    TDWWK HEADH   POPDENS   EDUINDEX   GRAVCPC   GRAVCPT  GRAVPCPT
    ## 1 10.003945 31.14282     1  85.70155 0.06406279 0.2492962 0.2492607 1.0001423
    ## 2 15.502989  0.00000     1 146.43494 0.26723192 0.3293831 0.3102800 1.0615674
    ## 3 12.709374 20.38427     1 106.60810 0.09996816 0.2396229 0.2899865 0.8263245
    ## 4  3.168599 32.94246     1  36.78380 0.08671065 0.2734539 0.2487830 1.0991661
    ## 5  5.364160 13.04013     1 181.62720 0.13091674 0.2854017 0.2913676 0.9795244
    ##   NSTRTC    DISTHW    DIVIDX    ACTDENS   DISTCBD
    ## 1     38 2036.4661 0.3225354  0.6722406  9776.142
    ## 2     34  747.7683 0.3484588  2.4860345  3523.994
    ## 3     33 2279.0577 0.3237884  1.6249059 11036.407
    ## 4      6 1196.4665 0.3272149  1.7664923  6257.262
    ## 5     31 3507.2402 0.3545181 11.3249309  1265.239

##### Make ID as row names or case number

``` r
df<-data.frame(df, row.names = 1)
```

### Evaluating the assumptions for factoral analysis

Let’s run a random regression model in order to evaluate some
assumptions

``` r
random = rchisq(nrow(df), 32)
fake = lm(random ~ ., data = df)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)
```

-   **Normality**

``` r
hist(standardized)
```

![](README_files/3-FactorAnalysis/unnamed-chunk-8-1.png)<!-- -->

-   **Linearity**

``` r
qqnorm(standardized)
abline(0,1)
```

![](README_files/3-FactorAnalysis/unnamed-chunk-9-1.png)<!-- -->

-   **Homogeneity**

``` r
plot(fitted, standardized)
abline(h=0,v=0)
```

![](README_files/3-FactorAnalysis/unnamed-chunk-10-1.png)<!-- -->

### Calculate the correlation matrix

``` r
corr_matrix <- cor(df, method = "pearson")
```

##### Check for correlation adequacy - Bartlett’s Test.

``` r
cortest.bartlett(corr_matrix, n = nrow(df))
```

    ## $chisq
    ## [1] 9880.074
    ## 
    ## $p.value
    ## [1] 0
    ## 
    ## $df
    ## [1] 465

> **Note:** \* The Bartlett test examines if there is equal variance
> (homogeneity) between variables. Thus, it evaluates if there is any
> pattern between variables.  
> \* The null hypothesis is that there is no correlation between
> variables. Therefore, in factor analysis you want to reject the null
> hypothesis.

##### Check for sampling adequacy - KMO test

``` r
KMO(corr_matrix)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = corr_matrix)
    ## Overall MSA =  0.68
    ## MSA for each item = 
    ## DWELCLAS   INCOME  CHILD13      H18  HEMPLOY    HSIZE AVADUAGE     IAGE 
    ##     0.70     0.85     0.33     0.58     0.88     0.59     0.38     0.40 
    ##     ISEX    NCARS     AREA  BEDROOM     PARK  BEDSIZE PARKSIZE   RAGE10 
    ##     0.71     0.74     0.60     0.53     0.62     0.58     0.57     0.84 
    ##     TCBD  DISTHTC    TWCBD    TDWWK    HEADH  POPDENS EDUINDEX  GRAVCPC 
    ##     0.88     0.88     0.82     0.89     0.47     0.82     0.85     0.76 
    ##  GRAVCPT GRAVPCPT   NSTRTC   DISTHW   DIVIDX  ACTDENS  DISTCBD 
    ##     0.71     0.31     0.83     0.76     0.63     0.70     0.86

> **Note:** We want at least 0.7 of the overall Mean Sample Adequacy
> (MSA). If, 0.6 &lt; MSA &lt; 0.7, it is not a good value, but
> acceptable in some cases.

### Determine the number of factors to extract

***1. Parallel Analysis***

``` r
num_factors = fa.parallel(df, fm = "ml", fa = "fa")
```

![](README_files/3-FactorAnalysis/unnamed-chunk-14-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  8  and the number of components =  NA

> **Note:** `fm` = factor math; `ml` = maximum likelihood; `fa` = factor
> analysis

The selection of the number of factors in the Parallel analysis can be
threefold:

-   Detect where there is an “elbow” in the graph;
-   Detect the intersection between the “FA Actual Data” and the “FA
    Simulated Data”;
-   Consider the number of factors with eigenvalue &gt; 1.

***2. Kaiser Criterion***

``` r
sum(num_factors$fa.values > 1)
```

    ## [1] 4

> **Note:** Determines the number of factors with eigenvalue &gt; 1.  
> You can also consider factors with eigenvalue &gt; 0.7, since some of
> the literature indicate that this value does not overestimate the
> number of factors as much as considering an eigenvalue = 1.

***3. Principal Component Analysis (PCA)***

``` r
df_pca <- princomp(df, cor=TRUE)
```

> **Note:** cor = TRUE, standardizes your dataset before running a PCA.

##### Print variance that explains the components

``` r
summary(df_pca)  
```

    ## Importance of components:
    ##                          Comp.1    Comp.2     Comp.3     Comp.4     Comp.5
    ## Standard deviation     2.450253 1.9587909 1.61305418 1.43367870 1.27628545
    ## Proportion of Variance 0.193669 0.1237697 0.08393367 0.06630434 0.05254531
    ## Cumulative Proportion  0.193669 0.3174388 0.40137245 0.46767679 0.52022210
    ##                            Comp.6     Comp.7     Comp.8    Comp.9    Comp.10
    ## Standard deviation     1.26612033 1.22242045 1.11550534 1.0304937 0.99888665
    ## Proportion of Variance 0.05171164 0.04820361 0.04014039 0.0342554 0.03218628
    ## Cumulative Proportion  0.57193374 0.62013734 0.66027774 0.6945331 0.72671941
    ##                           Comp.11    Comp.12   Comp.13    Comp.14    Comp.15
    ## Standard deviation     0.97639701 0.92221635 0.9042314 0.85909928 0.80853555
    ## Proportion of Variance 0.03075326 0.02743494 0.0263753 0.02380812 0.02108806
    ## Cumulative Proportion  0.75747267 0.78490761 0.8112829 0.83509102 0.85617908
    ##                          Comp.16    Comp.17    Comp.18    Comp.19    Comp.20
    ## Standard deviation     0.7877571 0.74436225 0.72574751 0.69380677 0.67269732
    ## Proportion of Variance 0.0200181 0.01787339 0.01699063 0.01552799 0.01459747
    ## Cumulative Proportion  0.8761972 0.89407058 0.91106120 0.92658920 0.94118667
    ##                           Comp.21    Comp.22    Comp.23     Comp.24     Comp.25
    ## Standard deviation     0.63466979 0.61328635 0.55192724 0.397467153 0.384354087
    ## Proportion of Variance 0.01299373 0.01213291 0.00982657 0.005096133 0.004765421
    ## Cumulative Proportion  0.95418041 0.96631331 0.97613988 0.981236017 0.986001438
    ##                            Comp.26     Comp.27     Comp.28     Comp.29
    ## Standard deviation     0.364232811 0.322026864 0.276201256 0.262018088
    ## Proportion of Variance 0.004279534 0.003345203 0.002460875 0.002214628
    ## Cumulative Proportion  0.990280972 0.993626175 0.996087050 0.998301679
    ##                             Comp.30      Comp.31
    ## Standard deviation     0.1712372644 0.1527277294
    ## Proportion of Variance 0.0009458774 0.0007524438
    ## Cumulative Proportion  0.9992475562 1.0000000000

-   Scree Plot

``` r
plot(df_pca,type="lines", npcs = 31) 
```

![](README_files/3-FactorAnalysis/unnamed-chunk-18-1.png)<!-- -->

> **Note:** Check the cummulative variance of the first components and
> the scree plot, and see if the PCA is a good approach to detect the
> number of factors in this case.

**PCA is not the same thing as Factor Analysis!** PCA only considers the
common information (variance) of the variables, while factor analysis
takes into account also the unique variance of the variable. Both
approaches are often mixed up. In this example we use PCA as only a
first criteria for choosing the number of factors. PCA is very used in
image recognition and data reduction of big data.

## EXPLORATORY FACTOR ANALYSIS

-   *Model 1: No rotation*
-   *Model 2: Rotation Varimax*
-   *Model 3: Rotation Oblimin*

``` r
# No rotation
df_factor <- factanal(df, factors = 4, rotation = "none", scores=c("regression"), fm = "ml")
# Rotation Varimax
df_factor_var <- factanal(df, factors = 4, rotation = "varimax", scores=c("regression"), fm = "ml")
# Rotiation Oblimin
df_factor_obl <- factanal(df, factors = 4, rotation = "oblimin", scores=c("regression"), fm = "ml")
```

##### Let’s print out the results of df\_factor\_obl, and have a look.

``` r
print(df_factor, digits=2, cutoff=0.3, sort=TRUE)
```

    ## 
    ## Call:
    ## factanal(x = df, factors = 4, scores = c("regression"), rotation = "none",     fm = "ml")
    ## 
    ## Uniquenesses:
    ## DWELCLAS   INCOME  CHILD13      H18  HEMPLOY    HSIZE AVADUAGE     IAGE 
    ##     0.98     0.82     0.09     0.01     0.73     0.01     0.98     0.99 
    ##     ISEX    NCARS     AREA  BEDROOM     PARK  BEDSIZE PARKSIZE   RAGE10 
    ##     0.98     0.64     0.93     0.79     0.89     0.62     0.92     0.91 
    ##     TCBD  DISTHTC    TWCBD    TDWWK    HEADH  POPDENS EDUINDEX  GRAVCPC 
    ##     0.14     0.54     0.80     0.74     0.67     0.78     0.71     0.04 
    ##  GRAVCPT GRAVPCPT   NSTRTC   DISTHW   DIVIDX  ACTDENS  DISTCBD 
    ##     0.05     0.01     0.85     0.66     0.84     0.80     0.31 
    ## 
    ## Loadings:
    ##          Factor1 Factor2 Factor3 Factor4
    ## TCBD      0.92                          
    ## DISTHTC   0.60            0.31          
    ## EDUINDEX -0.53                          
    ## GRAVCPC  -0.97                          
    ## GRAVCPT  -0.70           -0.67          
    ## DISTHW    0.56                          
    ## DISTCBD   0.79                          
    ## H18               0.93           -0.36  
    ## HEMPLOY           0.51                  
    ## HSIZE             0.93            0.35  
    ## NCARS             0.58                  
    ## BEDSIZE          -0.61                  
    ## GRAVPCPT                  0.98          
    ## CHILD13           0.31            0.90  
    ## DWELCLAS                                
    ## INCOME            0.40                  
    ## AVADUAGE                                
    ## IAGE                                    
    ## ISEX                                    
    ## AREA                                    
    ## BEDROOM           0.44                  
    ## PARK                                    
    ## PARKSIZE                                
    ## RAGE10                                  
    ## TWCBD     0.43                          
    ## TDWWK     0.47                          
    ## HEADH            -0.45            0.35  
    ## POPDENS  -0.39                          
    ## NSTRTC   -0.36                          
    ## DIVIDX   -0.40                          
    ## ACTDENS  -0.41                          
    ## 
    ##                Factor1 Factor2 Factor3 Factor4
    ## SS loadings       5.04    3.54    1.90    1.32
    ## Proportion Var    0.16    0.11    0.06    0.04
    ## Cumulative Var    0.16    0.28    0.34    0.38
    ## 
    ## Test of the hypothesis that 4 factors are sufficient.
    ## The chi square statistic is 3628.43 on 347 degrees of freedom.
    ## The p-value is 0

> **Note:** \* We used a cutoff of 0.3 due to the sample size is higher
> than 350 observations.  
> \* The variability contained in the factors = Communality +
> Uniqueness.  
> \* Varimax assigns orthogonal rotation, and oblimin assigns oblique
> rotation.

##### Plot factor 1 against factor 2, and compare the results of different rotations

-   **No Rotation**

``` r
plot(
  df_factor$loadings[, 1],
  df_factor$loadings[, 2],
  xlab = "Factor 1",
  ylab = "Factor 2",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "No rotation"
)
abline(h = 0, v = 0)
load <- df_factor$loadings[, 1:2]
text(
  load,
  names(df),
  cex = .7,
  col = "blue"
)
```

![](README_files/3-FactorAnalysis/unnamed-chunk-21-1.png)<!-- -->

-   **Varimax rotation**

``` r
plot(
  df_factor_var$loadings[, 1],
  df_factor_var$loadings[, 2],
  xlab = "Factor 1",
  ylab = "Factor 2",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "Varimax rotation"
)
abline(h = 0, v = 0)
load <- df_factor_var$loadings[, 1:2]
text(
  load,
  labels = names(df),
  cex = .7,
  col = "red"
)
```

![](README_files/3-FactorAnalysis/unnamed-chunk-22-1.png)<!-- -->

-   **Oblimin Rotation**

``` r
plot(
  df_factor_obl$loadings[, 1],
  df_factor_obl$loadings[, 2],
  xlab = "Factor 1",
  ylab = "Factor 2",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "Oblimin rotation"
)
abline(h = 0, v = 0)
load <- df_factor_obl$loadings[, 1:2]
text(
  load,
  labels = names(df),
  cex = .7,
  col = "darkgreen"
)
```

![](README_files/3-FactorAnalysis/unnamed-chunk-23-1.png)<!-- -->

> **Note:** When you have more than two factors it is difficult to
> analyse the factors by the plots. Variables that have low explaining
> variance in the two factors analyzed, can have a be highly explained
> by the other factors not present in the graph. However, try comparing
> the plots with the factor loadings and plot the other graphs to get
> more familiar with exploratory factor analysis.
