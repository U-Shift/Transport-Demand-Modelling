Exploratory Factor Analysis
================

## EXAMPLE EXERCISE:

### “Residential location satisfaction in the Lisbon metropolitan area”

The aim of this study was to examine the perception of households
towards their residential location considering several land use and
accessibility factors as well as household socioeconomic and attitudinal
characteristics.

*Reference:* **Martinez, L. G., de Abreu e Silva, J., & Viegas, J. M.
(2010). Assessment of residential location satisfaction in the Lisbon
metropolitan area, TRB (No. 10-1161).**

> Your task: Analyse the data and create meaningful latent factors.

### Variables:

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

### Rules of thumb:

-   At least 10 variables
-   n &lt; 50 (Unacceptable); n &gt; 200 (recommended)
-   It is recommended to use continuous variables. If your data contains
    categorical variables, you should transform them to dummy variables.

### Assumptions:

-   Normality;
-   linearity;
-   Homogeneity;
-   Homoscedasticity (some multicollinearity is desirable);
-   Correlations between variables &lt; 0.3 (not appropriate to use
    Factor Analysis)

#### Import Libraries

Let’s start!

``` r
library(foreign) # Library used to read SPSS files
library(nFactors) # Library used for factor analysis
library(tidyverse) # Library used in data science to perform exploratory data analysis
library(summarytools) # Library used for checking the summary of the dataset
library(psych) # Library used for factor analysis
library(GPArotation) # Library used for factor analysis
```

#### Import dataset

``` r
data <- read.spss("Data/example_fact.sav")
```

#### Take a look at the main characteristics of the dataset

``` r
class(data) #type of data
```

    ## [1] "list"

``` r
str(data)
```

    ## List of 32
    ##  $ RespondentID: num [1:470] 7.99e+08 7.98e+08 7.98e+08 7.98e+08 7.98e+08 ...
    ##  $ DWELCLAS    : num [1:470] 5 6 6 5 6 6 4 2 6 5 ...
    ##  $ INCOME      : num [1:470] 7500 4750 4750 7500 2750 1500 12500 1500 1500 1500 ...
    ##  $ CHILD13     : num [1:470] 1 0 2 0 1 0 0 0 0 0 ...
    ##  $ H18         : num [1:470] 2 1 2 3 1 3 3 4 1 1 ...
    ##  $ HEMPLOY     : num [1:470] 2 1 2 2 1 2 0 2 1 1 ...
    ##  $ HSIZE       : num [1:470] 3 1 4 4 2 3 3 4 1 1 ...
    ##  $ AVADUAGE    : num [1:470] 32 31 41.5 44.7 33 ...
    ##  $ IAGE        : num [1:470] 32 31 42 52 33 47 62 21 34 25 ...
    ##  $ ISEX        : num [1:470] 1 1 0 1 0 1 1 0 0 0 ...
    ##  $ NCARS       : num [1:470] 2 1 2 3 1 1 2 3 1 1 ...
    ##  $ AREA        : num [1:470] 100 90 220 120 90 100 178 180 80 50 ...
    ##  $ BEDROOM     : num [1:470] 2 2 4 3 2 2 5 3 2 1 ...
    ##  $ PARK        : num [1:470] 1 1 2 0 0 0 2 0 0 1 ...
    ##  $ BEDSIZE     : num [1:470] 0.667 2 1 0.75 1 ...
    ##  $ PARKSIZE    : num [1:470] 0.5 1 1 0 0 0 1 0 0 1 ...
    ##  $ RAGE10      : num [1:470] 1 0 1 0 0 0 0 0 1 1 ...
    ##  $ TCBD        : num [1:470] 36.79 15.47 24.1 28.72 7.28 ...
    ##  $ DISTHTC     : num [1:470] 629 551 548 2351 698 ...
    ##  $ TWCBD       : num [1:470] 10 15.5 12.71 3.17 5.36 ...
    ##  $ TDWWK       : num [1:470] 31.1 0 20.4 32.9 13 ...
    ##  $ HEADH       : num [1:470] 1 1 1 1 1 1 1 0 1 1 ...
    ##  $ POPDENS     : num [1:470] 85.7 146.4 106.6 36.8 181.6 ...
    ##  $ EDUINDEX    : num [1:470] 0.0641 0.2672 0.1 0.0867 0.1309 ...
    ##  $ GRAVCPC     : num [1:470] 0.249 0.329 0.24 0.273 0.285 ...
    ##  $ GRAVCPT     : num [1:470] 0.249 0.31 0.29 0.249 0.291 ...
    ##  $ GRAVPCPT    : num [1:470] 1 1.062 0.826 1.099 0.98 ...
    ##  $ NSTRTC      : num [1:470] 38 34 33 6 31 45 12 6 4 22 ...
    ##  $ DISTHW      : num [1:470] 2036 748 2279 1196 3507 ...
    ##  $ DIVIDX      : num [1:470] 0.323 0.348 0.324 0.327 0.355 ...
    ##  $ ACTDENS     : num [1:470] 0.672 2.486 1.625 1.766 11.325 ...
    ##  $ DISTCBD     : num [1:470] 9776 3524 11036 6257 1265 ...
    ##  - attr(*, "label.table")=List of 32
    ##   ..$ RespondentID: NULL
    ##   ..$ DWELCLAS    : NULL
    ##   ..$ INCOME      : NULL
    ##   ..$ CHILD13     : NULL
    ##   ..$ H18         : NULL
    ##   ..$ HEMPLOY     : NULL
    ##   ..$ HSIZE       : NULL
    ##   ..$ AVADUAGE    : NULL
    ##   ..$ IAGE        : NULL
    ##   ..$ ISEX        : NULL
    ##   ..$ NCARS       : NULL
    ##   ..$ AREA        : NULL
    ##   ..$ BEDROOM     : NULL
    ##   ..$ PARK        : NULL
    ##   ..$ BEDSIZE     : NULL
    ##   ..$ PARKSIZE    : NULL
    ##   ..$ RAGE10      : NULL
    ##   ..$ TCBD        : NULL
    ##   ..$ DISTHTC     : NULL
    ##   ..$ TWCBD       : NULL
    ##   ..$ TDWWK       : NULL
    ##   ..$ HEADH       : NULL
    ##   ..$ POPDENS     : NULL
    ##   ..$ EDUINDEX    : NULL
    ##   ..$ GRAVCPC     : NULL
    ##   ..$ GRAVCPT     : NULL
    ##   ..$ GRAVPCPT    : NULL
    ##   ..$ NSTRTC      : NULL
    ##   ..$ DISTHW      : NULL
    ##   ..$ DIVIDX      : NULL
    ##   ..$ ACTDENS     : NULL
    ##   ..$ DISTCBD     : NULL
    ##  - attr(*, "codepage")= int 1252
    ##  - attr(*, "variable.labels")= Named chr(0) 
    ##   ..- attr(*, "names")= chr(0)

#### Transform dataset into dataframe

``` r
df <- data.frame(data)
```

#### Check summary statistics of variables

``` r
descriptive_stats <- dfSummary(df)
```

``` r
view(descriptive_stats)
```

<style type="text/css">
 img {   background-color: transparent;   border: 0; }  .st-table td, .st-table th {   padding: 8px; }  .st-table > thead > tr {    background-color: #eeeeee; }  .st-cross-table td {   text-align: center; }  .st-descr-table td {   text-align: right; }  table.st-table th {   text-align: center; }  table.st-table > thead > tr {    background-color: #eeeeee; }  table.st-table td span {   display: block; }  table.st-table > tfoot > tr > td {   border:none; }  .st-container {   width: 100%;   padding-right: 15px;   padding-left: 15px;   margin-right: auto;   margin-left: auto;   margin-top: 15px; }  .st-multiline {   white-space: pre; }  .st-table {     width: auto;     table-layout: auto;     margin-top: 20px;     margin-bottom: 20px;     max-width: 100%;     background-color: transparent;     border-collapse: collapse; }  .st-table > thead > tr > th, .st-table > tbody > tr > th, .st-table > tfoot > tr > th, .st-table > thead > tr > td, .st-table > tbody > tr > td, .st-table > tfoot > tr > td {   vertical-align: middle; }  .st-table-bordered {   border: 1px solid #bbbbbb; }  .st-table-bordered > thead > tr > th, .st-table-bordered > tbody > tr > th, .st-table-bordered > thead > tr > td, .st-table-bordered > tbody > tr > td {   border: 1px solid #cccccc; }  .st-table-bordered > thead > tr > th, .st-table-bordered > thead > tr > td, .st-table thead > tr > th {   border-bottom: none; }  .st-freq-table > thead > tr > th, .st-freq-table > tbody > tr > th, .st-freq-table > tfoot > tr > th, .st-freq-table > thead > tr > td, .st-freq-table > tbody > tr > td, .st-freq-table > tfoot > tr > td, .st-freq-table-nomiss > thead > tr > th, .st-freq-table-nomiss > tbody > tr > th, .st-freq-table-nomiss > tfoot > tr > th, .st-freq-table-nomiss > thead > tr > td, .st-freq-table-nomiss > tbody > tr > td, .st-freq-table-nomiss > tfoot > tr > td, .st-cross-table > thead > tr > th, .st-cross-table > tbody > tr > th, .st-cross-table > tfoot > tr > th, .st-cross-table > thead > tr > td, .st-cross-table > tbody > tr > td, .st-cross-table > tfoot > tr > td {   padding-left: 20px;   padding-right: 20px; }  .st-table-bordered > thead > tr > th, .st-table-bordered > tbody > tr > th, .st-table-bordered > thead > tr > td, .st-table-bordered > tbody > tr > td {   border: 1px solid #cccccc; }  .st-table-striped > tbody > tr:nth-of-type(odd) {   background-color: #ffffff; }  .st-table-striped > tbody > tr:nth-of-type(even) {   background-color: #f9f9f9; }  .st-descr-table > thead > tr > th, .st-descr-table > tbody > tr > th, .st-descr-table > thead > tr > td, .st-descr-table > tbody > tr > td {   padding-left: 24px;   padding-right: 24px;   word-wrap: break-word; }  .st-freq-table, .st-freq-table-nomiss, .st-cross-table {   border: medium none; }  .st-freq-table > thead > tr:nth-child(1) > th:nth-child(1), .st-cross-table > thead > tr:nth-child(1) > th:nth-child(1), .st-cross-table > thead > tr:nth-child(1) > th:nth-child(3) {   border: none;   background-color: #ffffff;   text-align: center; }  .st-protect-top-border {   border-top: 1px solid #cccccc !important; }  .st-ws-char {   display: inline;   color: #999999;   letter-spacing: 0.2em; }  /* Optionnal classes */ .st-small {   font-size: 13px; }  .st-small td, .st-small th {   padding: 8px; }  .st-small > thead > tr > th, .st-small > tbody > tr > th, .st-small > thead > tr > td, .st-small > tbody > tr > td {   padding-left: 12px;   padding-right: 12px; } </style>

### Data Frame Summary

**df**  
**Dimensions:** 470 x 32  
**Duplicates:** 0

<table style="width:100%;">
<colgroup>
<col style="width: 4%" />
<col style="width: 13%" />
<col style="width: 31%" />
<col style="width: 18%" />
<col style="width: 23%" />
<col style="width: 8%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">No</th>
<th style="text-align: left;">Variable</th>
<th style="text-align: left;">Stats / Values</th>
<th style="text-align: left;">Freqs (% of Valid)</th>
<th style="text-align: left;">Graph</th>
<th style="text-align: left;">Missing</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">RespondentID<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 784082877.9 (8627500.7)<br />
min &lt; med &lt; max:<br />
773001005 &lt; 780248282.5 &lt; 808234671<br />
IQR (CV) : 16634784.8 (0)</td>
<td style="text-align: left;">470 distinct values</td>
<td style="text-align: left;"><br />
  :<br />
  :     :<br />
  :     :<br />
: :     :<br />
: : : : : :</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">2</td>
<td style="text-align: left;">DWELCLAS<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 5.1 (1.3)<br />
min &lt; med &lt; max:<br />
1 &lt; 5 &lt; 7<br />
IQR (CV) : 2 (0.2)</td>
<td style="text-align: left;">1 : 5 ( 1.1%)<br />
2 : 14 ( 3.0%)<br />
3 : 31 ( 6.6%)<br />
4 : 75 (16.0%)<br />
5 : 130 (27.7%)<br />
6 : 162 (34.5%)<br />
7 : 53 (11.3%)</td>
<td style="text-align: left;"><br />
<br />
I<br />
III<br />
IIIII<br />
IIIIII<br />
II</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3</td>
<td style="text-align: left;">INCOME<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 4259.6 (3001.8)<br />
min &lt; med &lt; max:<br />
700 &lt; 2750 &lt; 12500<br />
IQR (CV) : 2000 (0.7)</td>
<td style="text-align: left;">700 : 20 ( 4.3%)<br />
1500 : 96 (20.4%)<br />
2750 : 142 (30.2%)<br />
4750 : 106 (22.6%)<br />
7500 : 75 (16.0%)<br />
12500 : 31 ( 6.6%)</td>
<td style="text-align: left;"><br />
IIII<br />
IIIIII<br />
IIII<br />
III<br />
I</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: left;">CHILD13<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 0.4 (0.8)<br />
min &lt; med &lt; max:<br />
0 &lt; 0 &lt; 4<br />
IQR (CV) : 0 (2)</td>
<td style="text-align: left;">0 : 353 (75.1%)<br />
1 : 62 (13.2%)<br />
2 : 41 ( 8.7%)<br />
3 : 13 ( 2.8%)<br />
4 : 1 ( 0.2%)</td>
<td style="text-align: left;">IIIIIIIIIIIIIII<br />
II<br />
I<br />
<br />
</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">5</td>
<td style="text-align: left;">H18<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 2.1 (0.9)<br />
min &lt; med &lt; max:<br />
0 &lt; 2 &lt; 6<br />
IQR (CV) : 0.8 (0.4)</td>
<td style="text-align: left;">0 : 1 ( 0.2%)<br />
1 : 112 (23.8%)<br />
2 : 239 (50.8%)<br />
3 : 77 (16.4%)<br />
4 : 35 ( 7.4%)<br />
5 : 3 ( 0.6%)<br />
6 : 3 ( 0.6%)</td>
<td style="text-align: left;"><br />
IIII<br />
IIIIIIIIII<br />
III<br />
I<br />
<br />
</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">6</td>
<td style="text-align: left;">HEMPLOY<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 1.5 (0.7)<br />
min &lt; med &lt; max:<br />
0 &lt; 2 &lt; 5<br />
IQR (CV) : 1 (0.5)</td>
<td style="text-align: left;">0 : 39 ( 8.3%)<br />
1 : 171 (36.4%)<br />
2 : 237 (50.4%)<br />
3 : 21 ( 4.5%)<br />
4 : 1 ( 0.2%)<br />
5 : 1 ( 0.2%)</td>
<td style="text-align: left;">I<br />
IIIIIII<br />
IIIIIIIIII<br />
<br />
<br />
</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">7</td>
<td style="text-align: left;">HSIZE<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 2.6 (1.3)<br />
min &lt; med &lt; max:<br />
1 &lt; 2 &lt; 7<br />
IQR (CV) : 2 (0.5)</td>
<td style="text-align: left;">1 : 104 (22.1%)<br />
2 : 147 (31.3%)<br />
3 : 96 (20.4%)<br />
4 : 96 (20.4%)<br />
5 : 20 ( 4.3%)<br />
6 : 5 ( 1.1%)<br />
7 : 2 ( 0.4%)</td>
<td style="text-align: left;">IIII<br />
IIIIII<br />
IIII<br />
IIII<br />
<br />
<br />
</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">8</td>
<td style="text-align: left;">AVADUAGE<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 37.8 (9.9)<br />
min &lt; med &lt; max:<br />
0 &lt; 36 &lt; 78<br />
IQR (CV) : 12.7 (0.3)</td>
<td style="text-align: left;">126 distinct values</td>
<td style="text-align: left;"><br />
      :<br />
      :<br />
    : : :<br />
    : : :<br />
    : : : : .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9</td>
<td style="text-align: left;">IAGE<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 36.9 (11.6)<br />
min &lt; med &lt; max:<br />
0 &lt; 34 &lt; 78<br />
IQR (CV) : 15 (0.3)</td>
<td style="text-align: left;">53 distinct values</td>
<td style="text-align: left;"><br />
    : :<br />
    : :<br />
    : : .<br />
    : : : .<br />
    : : : : .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">10</td>
<td style="text-align: left;">ISEX<br />
[numeric]</td>
<td style="text-align: left;">Min : 0<br />
Mean : 0.5<br />
Max : 1</td>
<td style="text-align: left;">0 : 214 (45.5%)<br />
1 : 256 (54.5%)</td>
<td style="text-align: left;">IIIIIIIII<br />
IIIIIIIIII</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">11</td>
<td style="text-align: left;">NCARS<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 1.7 (0.9)<br />
min &lt; med &lt; max:<br />
0 &lt; 2 &lt; 5<br />
IQR (CV) : 1 (0.5)</td>
<td style="text-align: left;">0 : 23 ( 4.9%)<br />
1 : 182 (38.7%)<br />
2 : 193 (41.1%)<br />
3 : 56 (11.9%)<br />
4 : 13 ( 2.8%)<br />
5 : 3 ( 0.6%)</td>
<td style="text-align: left;"><br />
IIIIIII<br />
IIIIIIII<br />
II<br />
<br />
</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">12</td>
<td style="text-align: left;">AREA<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 133 (121.5)<br />
min &lt; med &lt; max:<br />
30 &lt; 110 &lt; 2250<br />
IQR (CV) : 60 (0.9)</td>
<td style="text-align: left;">76 distinct values</td>
<td style="text-align: left;"><br />
:<br />
:<br />
:<br />
:<br />
:</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">13</td>
<td style="text-align: left;">BEDROOM<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 2.9 (1.1)<br />
min &lt; med &lt; max:<br />
0 &lt; 3 &lt; 7<br />
IQR (CV) : 1 (0.4)</td>
<td style="text-align: left;">0 : 1 ( 0.2%)<br />
1 : 28 ( 6.0%)<br />
2 : 153 (32.6%)<br />
3 : 180 (38.3%)<br />
4 : 73 (15.5%)<br />
5 : 26 ( 5.5%)<br />
6 : 7 ( 1.5%)<br />
7 : 2 ( 0.4%)</td>
<td style="text-align: left;"><br />
I<br />
IIIIII<br />
IIIIIII<br />
III<br />
I<br />
<br />
</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">14</td>
<td style="text-align: left;">PARK<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 0.8 (1)<br />
min &lt; med &lt; max:<br />
0 &lt; 1 &lt; 4<br />
IQR (CV) : 1 (1.2)</td>
<td style="text-align: left;">0 : 224 (47.7%)<br />
1 : 136 (28.9%)<br />
2 : 84 (17.9%)<br />
3 : 18 ( 3.8%)<br />
4 : 8 ( 1.7%)</td>
<td style="text-align: left;">IIIIIIIII<br />
IIIII<br />
III<br />
<br />
</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">15</td>
<td style="text-align: left;">BEDSIZE<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 1.4 (0.8)<br />
min &lt; med &lt; max:<br />
0 &lt; 1 &lt; 5<br />
IQR (CV) : 0.7 (0.6)</td>
<td style="text-align: left;">22 distinct values</td>
<td style="text-align: left;"><br />
  :<br />
  :<br />
  :<br />
  : . .<br />
. : : :   .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">16</td>
<td style="text-align: left;">PARKSIZE<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 0.5 (0.6)<br />
min &lt; med &lt; max:<br />
0 &lt; 0.2 &lt; 3<br />
IQR (CV) : 1 (1.2)</td>
<td style="text-align: left;">13 distinct values</td>
<td style="text-align: left;"><br />
:<br />
:<br />
:<br />
: :<br />
: :   .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">17</td>
<td style="text-align: left;">RAGE10<br />
[numeric]</td>
<td style="text-align: left;">Min : 0<br />
Mean : 0.2<br />
Max : 1</td>
<td style="text-align: left;">0 : 356 (75.7%)<br />
1 : 114 (24.3%)</td>
<td style="text-align: left;">IIIIIIIIIIIIIII<br />
IIII</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">18</td>
<td style="text-align: left;">TCBD<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 24.7 (16.2)<br />
min &lt; med &lt; max:<br />
0.8 &lt; 23.8 &lt; 73.3<br />
IQR (CV) : 25.7 (0.7)</td>
<td style="text-align: left;">434 distinct values</td>
<td style="text-align: left;"><br />
:<br />
: : . :<br />
: : : :<br />
: : : : . .<br />
: : : : : : .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">19</td>
<td style="text-align: left;">DISTHTC<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 1347 (1815.8)<br />
min &lt; med &lt; max:<br />
49 &lt; 719 &lt; 17732.7<br />
IQR (CV) : 1125 (1.3)</td>
<td style="text-align: left;">434 distinct values</td>
<td style="text-align: left;"><br />
:<br />
:<br />
:<br />
:<br />
: .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">20</td>
<td style="text-align: left;">TWCBD<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 17 (16.2)<br />
min &lt; med &lt; max:<br />
0.3 &lt; 9.9 &lt; 67.8<br />
IQR (CV) : 20 (1)</td>
<td style="text-align: left;">439 distinct values</td>
<td style="text-align: left;"><br />
:<br />
:<br />
: .<br />
: :   .<br />
: : : : . : .   .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">21</td>
<td style="text-align: left;">TDWWK<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 23.5 (17.1)<br />
min &lt; med &lt; max:<br />
0 &lt; 22.2 &lt; 80.7<br />
IQR (CV) : 23.6 (0.7)</td>
<td style="text-align: left;">414 distinct values</td>
<td style="text-align: left;"><br />
: .<br />
: : : :<br />
: : : :<br />
: : : : .<br />
: : : : : . .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">22</td>
<td style="text-align: left;">HEADH<br />
[numeric]</td>
<td style="text-align: left;">Min : 0<br />
Mean : 0.9<br />
Max : 1</td>
<td style="text-align: left;">0 : 64 (13.6%)<br />
1 : 406 (86.4%)</td>
<td style="text-align: left;">II<br />
IIIIIIIIIIIIIIIII</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">23</td>
<td style="text-align: left;">POPDENS<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 92 (58.2)<br />
min &lt; med &lt; max:<br />
0 &lt; 83.2 &lt; 255.6<br />
IQR (CV) : 89.2 (0.6)</td>
<td style="text-align: left;">431 distinct values</td>
<td style="text-align: left;"><br />
    . :<br />
: : : :<br />
: : : : : . .<br />
: : : : : : : .<br />
: : : : : : : : :</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">24</td>
<td style="text-align: left;">EDUINDEX<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 0.2 (0.1)<br />
min &lt; med &lt; max:<br />
0 &lt; 0.2 &lt; 0.7<br />
IQR (CV) : 0.2 (0.6)</td>
<td style="text-align: left;">434 distinct values</td>
<td style="text-align: left;"><br />
  :<br />
  : .<br />
  : : :<br />
: : : : : .<br />
: : : : : : : . .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">25</td>
<td style="text-align: left;">GRAVCPC<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 0.3 (0.1)<br />
min &lt; med &lt; max:<br />
0.1 &lt; 0.3 &lt; 0.4<br />
IQR (CV) : 0.1 (0.2)</td>
<td style="text-align: left;">433 distinct values</td>
<td style="text-align: left;"><br />
          :<br />
          :<br />
        . : .<br />
      . : : :<br />
  . : : : : :</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">26</td>
<td style="text-align: left;">GRAVCPT<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 0.3 (0.1)<br />
min &lt; med &lt; max:<br />
0 &lt; 0.3 &lt; 0.4<br />
IQR (CV) : 0.1 (0.2)</td>
<td style="text-align: left;">434 distinct values</td>
<td style="text-align: left;"><br />
            :<br />
          : :<br />
        : : :<br />
      : : : :<br />
    . : : : :</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">27</td>
<td style="text-align: left;">GRAVPCPT<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 1.2 (0.3)<br />
min &lt; med &lt; max:<br />
0.5 &lt; 1.1 &lt; 2.9<br />
IQR (CV) : 0.2 (0.3)</td>
<td style="text-align: left;">434 distinct values</td>
<td style="text-align: left;"><br />
    :<br />
    :<br />
    :<br />
    : .<br />
. : : : . .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">28</td>
<td style="text-align: left;">NSTRTC<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 22.2 (12.6)<br />
min &lt; med &lt; max:<br />
0 &lt; 22 &lt; 84<br />
IQR (CV) : 16 (0.6)</td>
<td style="text-align: left;">59 distinct values</td>
<td style="text-align: left;"><br />
  : .<br />
  : :<br />
: : : :<br />
: : : :<br />
: : : : .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">29</td>
<td style="text-align: left;">DISTHW<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 1883.4 (1748.3)<br />
min &lt; med &lt; max:<br />
74.7 &lt; 1338.7 &lt; 16590.1<br />
IQR (CV) : 1820.5 (0.9)</td>
<td style="text-align: left;">434 distinct values</td>
<td style="text-align: left;"><br />
:<br />
:<br />
:<br />
: :<br />
: : . .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">30</td>
<td style="text-align: left;">DIVIDX<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 0.4 (0.1)<br />
min &lt; med &lt; max:<br />
0.3 &lt; 0.4 &lt; 0.6<br />
IQR (CV) : 0.1 (0.2)</td>
<td style="text-align: left;">144 distinct values</td>
<td style="text-align: left;"><br />
  :<br />
  : :<br />
  : :<br />
  : :<br />
: : : : : . . .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">31</td>
<td style="text-align: left;">ACTDENS<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 5.8 (8.5)<br />
min &lt; med &lt; max:<br />
0 &lt; 2.5 &lt; 63.2<br />
IQR (CV) : 3.5 (1.5)</td>
<td style="text-align: left;">144 distinct values</td>
<td style="text-align: left;"><br />
:<br />
:<br />
:<br />
:<br />
: . .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">32</td>
<td style="text-align: left;">DISTCBD<br />
[numeric]</td>
<td style="text-align: left;">Mean (sd) : 7967.4 (7442.9)<br />
min &lt; med &lt; max:<br />
148.9 &lt; 5542.3 &lt; 44004.6<br />
IQR (CV) : 9777.9 (0.9)</td>
<td style="text-align: left;">434 distinct values</td>
<td style="text-align: left;"><br />
:<br />
:<br />
:<br />
: : .<br />
: : : : .</td>
<td style="text-align: left;">0<br />
(0%)</td>
</tr>
</tbody>
</table>

``` r
summary(df)
```

    ##   RespondentID          DWELCLAS         INCOME         CHILD13      
    ##  Min.   :773001005   Min.   :1.000   Min.   :  700   Min.   :0.0000  
    ##  1st Qu.:776363273   1st Qu.:4.000   1st Qu.: 2750   1st Qu.:0.0000  
    ##  Median :780248282   Median :5.000   Median : 2750   Median :0.0000  
    ##  Mean   :784082878   Mean   :5.147   Mean   : 4260   Mean   :0.3979  
    ##  3rd Qu.:792998058   3rd Qu.:6.000   3rd Qu.: 4750   3rd Qu.:0.0000  
    ##  Max.   :808234671   Max.   :7.000   Max.   :12500   Max.   :4.0000  
    ##       H18           HEMPLOY          HSIZE          AVADUAGE    
    ##  Min.   :0.000   Min.   :0.000   Min.   :1.000   Min.   : 0.00  
    ##  1st Qu.:2.000   1st Qu.:1.000   1st Qu.:2.000   1st Qu.:30.33  
    ##  Median :2.000   Median :2.000   Median :2.000   Median :36.00  
    ##  Mean   :2.115   Mean   :1.526   Mean   :2.583   Mean   :37.79  
    ##  3rd Qu.:2.750   3rd Qu.:2.000   3rd Qu.:4.000   3rd Qu.:43.00  
    ##  Max.   :6.000   Max.   :5.000   Max.   :7.000   Max.   :78.00  
    ##       IAGE            ISEX            NCARS            AREA     
    ##  Min.   : 0.00   Min.   :0.0000   Min.   :0.000   Min.   :  30  
    ##  1st Qu.:28.00   1st Qu.:0.0000   1st Qu.:1.000   1st Qu.:  90  
    ##  Median :34.00   Median :1.0000   Median :2.000   Median : 110  
    ##  Mean   :36.89   Mean   :0.5447   Mean   :1.709   Mean   : 133  
    ##  3rd Qu.:43.00   3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.: 150  
    ##  Max.   :78.00   Max.   :1.0000   Max.   :5.000   Max.   :2250  
    ##     BEDROOM           PARK           BEDSIZE        PARKSIZE     
    ##  Min.   :0.000   Min.   :0.0000   Min.   :0.00   Min.   :0.0000  
    ##  1st Qu.:2.000   1st Qu.:0.0000   1st Qu.:0.80   1st Qu.:0.0000  
    ##  Median :3.000   Median :1.0000   Median :1.00   Median :0.2500  
    ##  Mean   :2.877   Mean   :0.8298   Mean   :1.35   Mean   :0.4662  
    ##  3rd Qu.:3.000   3rd Qu.:1.0000   3rd Qu.:1.50   3rd Qu.:1.0000  
    ##  Max.   :7.000   Max.   :4.0000   Max.   :5.00   Max.   :3.0000  
    ##      RAGE10            TCBD            DISTHTC             TWCBD        
    ##  Min.   :0.0000   Min.   : 0.7817   Min.   :   49.05   Min.   : 0.3116  
    ##  1st Qu.:0.0000   1st Qu.: 9.9509   1st Qu.:  400.47   1st Qu.: 5.2482  
    ##  Median :0.0000   Median :23.8093   Median :  719.04   Median : 9.8600  
    ##  Mean   :0.2426   Mean   :24.7046   Mean   : 1346.98   Mean   :16.9986  
    ##  3rd Qu.:0.0000   3rd Qu.:35.6981   3rd Qu.: 1525.45   3rd Qu.:25.2658  
    ##  Max.   :1.0000   Max.   :73.2820   Max.   :17732.68   Max.   :67.8190  
    ##      TDWWK           HEADH           POPDENS          EDUINDEX      
    ##  Min.   : 0.00   Min.   :0.0000   Min.   :  0.00   Min.   :0.00000  
    ##  1st Qu.:10.14   1st Qu.:1.0000   1st Qu.: 45.86   1st Qu.:0.09601  
    ##  Median :22.18   Median :1.0000   Median : 83.17   Median :0.17319  
    ##  Mean   :23.53   Mean   :0.8638   Mean   : 92.00   Mean   :0.19880  
    ##  3rd Qu.:33.72   3rd Qu.:1.0000   3rd Qu.:135.01   3rd Qu.:0.27699  
    ##  Max.   :80.72   Max.   :1.0000   Max.   :255.55   Max.   :0.65966  
    ##     GRAVCPC           GRAVCPT           GRAVPCPT          NSTRTC     
    ##  Min.   :0.06528   Min.   :0.02569   Min.   :0.4534   Min.   : 0.00  
    ##  1st Qu.:0.25301   1st Qu.:0.22117   1st Qu.:0.9985   1st Qu.:14.00  
    ##  Median :0.30924   Median :0.28129   Median :1.0911   Median :22.00  
    ##  Mean   :0.28980   Mean   :0.26186   Mean   :1.1517   Mean   :22.22  
    ##  3rd Qu.:0.34041   3rd Qu.:0.31036   3rd Qu.:1.2224   3rd Qu.:30.00  
    ##  Max.   :0.39395   Max.   :0.36508   Max.   :2.8799   Max.   :84.00  
    ##      DISTHW             DIVIDX          ACTDENS            DISTCBD       
    ##  Min.   :   74.67   Min.   :0.2634   Min.   : 0.02199   Min.   :  148.9  
    ##  1st Qu.:  712.79   1st Qu.:0.3239   1st Qu.: 1.31611   1st Qu.: 2025.3  
    ##  Median : 1338.74   Median :0.3548   Median : 2.49377   Median : 5542.3  
    ##  Mean   : 1883.41   Mean   :0.3777   Mean   : 5.77963   Mean   : 7967.4  
    ##  3rd Qu.: 2533.30   3rd Qu.:0.3970   3rd Qu.: 4.85162   3rd Qu.:11803.2  
    ##  Max.   :16590.06   Max.   :0.6500   Max.   :63.18205   Max.   :44004.6

> **Note:** I used other functions for summary statistics in last
> exercise.

#### Take a look at the dataset

``` r
head(df,10, style="rmarkdown")
```

    ##    RespondentID DWELCLAS INCOME CHILD13 H18 HEMPLOY HSIZE AVADUAGE IAGE ISEX
    ## 1     799161661        5   7500       1   2       2     3 32.00000   32    1
    ## 2     798399409        6   4750       0   1       1     1 31.00000   31    1
    ## 3     798374392        6   4750       2   2       2     4 41.50000   42    0
    ## 4     798275277        5   7500       0   3       2     4 44.66667   52    1
    ## 5     798264250        6   2750       1   1       1     2 33.00000   33    0
    ## 6     798235878        6   1500       0   3       2     3 30.00000   47    1
    ## 7     797907742        4  12500       0   3       0     3 69.33333   62    1
    ## 8     797871767        2   1500       0   4       2     4 48.75000   21    0
    ## 9     797821210        6   1500       0   1       1     1 34.00000   34    0
    ## 10    797552006        5   1500       0   1       1     1 25.00000   25    0
    ##    NCARS AREA BEDROOM PARK   BEDSIZE PARKSIZE RAGE10      TCBD   DISTHTC
    ## 1      2  100       2    1 0.6666667      0.5      1 36.791237  629.1120
    ## 2      1   90       2    1 2.0000000      1.0      0 15.472989  550.5769
    ## 3      2  220       4    2 1.0000000      1.0      1 24.098125  547.8633
    ## 4      3  120       3    0 0.7500000      0.0      0 28.724796 2350.5782
    ## 5      1   90       2    0 1.0000000      0.0      0  7.283384  698.3000
    ## 6      1  100       2    0 0.6666667      0.0      0 20.604348 1838.2472
    ## 7      2  178       5    2 1.6666667      1.0      0 42.613947 6837.6209
    ## 8      3  180       3    0 0.7500000      0.0      0 40.805074 6815.2118
    ## 9      1   80       2    0 2.0000000      0.0      1 46.289862 4308.7500
    ## 10     1   50       1    1 1.0000000      1.0      1  8.701663  512.1624
    ##        TWCBD     TDWWK HEADH    POPDENS   EDUINDEX   GRAVCPC   GRAVCPT
    ## 1  10.003945 31.142824     1  85.701553 0.06406279 0.2492962 0.2492607
    ## 2  15.502989  0.000000     1 146.434938 0.26723192 0.3293831 0.3102800
    ## 3  12.709374 20.384273     1 106.608098 0.09996816 0.2396229 0.2899865
    ## 4   3.168599 32.942463     1  36.783804 0.08671065 0.2734539 0.2487830
    ## 5   5.364160 13.040133     1 181.627196 0.13091674 0.2854017 0.2913676
    ## 6   5.024916 22.424728     1  72.065190 0.31409969 0.3655758 0.2339590
    ## 7  41.749079  9.190614     1  47.797301 0.08100000 0.1559960 0.2087794
    ## 8  45.143804  3.489875     0  40.794500 0.07485605 0.1647730 0.1802961
    ## 9   1.313656 47.399052     1   6.977336 0.04782609 0.1471991 0.2285333
    ## 10 14.502650 15.541200     1 179.615483 0.13547579 0.3353141 0.2940870
    ##     GRAVPCPT NSTRTC    DISTHW    DIVIDX     ACTDENS   DISTCBD
    ## 1  1.0001423     38 2036.4661 0.3225354  0.67224060  9776.142
    ## 2  1.0615674     34  747.7683 0.3484588  2.48603452  3523.994
    ## 3  0.8263245     33 2279.0577 0.3237884  1.62490591 11036.407
    ## 4  1.0991661      6 1196.4665 0.3272149  1.76649226  6257.262
    ## 5  0.9795244     31 3507.2402 0.3545181 11.32493089  1265.239
    ## 6  1.5625632     45  345.3282 0.3545291  4.60901894  6614.929
    ## 7  0.7471812     12 4125.4581 0.3914727  0.05016163 12035.249
    ## 8  0.9139020      6 4346.3370 0.3914727  0.05016163 11759.166
    ## 9  0.6441035      4 2385.6950 0.3534021  0.12445352 11817.881
    ## 10 1.1401870     22 3082.7274 0.3111948  3.84403202  1726.137

``` r
summary(df)
```

    ##   RespondentID          DWELCLAS         INCOME         CHILD13      
    ##  Min.   :773001005   Min.   :1.000   Min.   :  700   Min.   :0.0000  
    ##  1st Qu.:776363273   1st Qu.:4.000   1st Qu.: 2750   1st Qu.:0.0000  
    ##  Median :780248282   Median :5.000   Median : 2750   Median :0.0000  
    ##  Mean   :784082878   Mean   :5.147   Mean   : 4260   Mean   :0.3979  
    ##  3rd Qu.:792998058   3rd Qu.:6.000   3rd Qu.: 4750   3rd Qu.:0.0000  
    ##  Max.   :808234671   Max.   :7.000   Max.   :12500   Max.   :4.0000  
    ##       H18           HEMPLOY          HSIZE          AVADUAGE    
    ##  Min.   :0.000   Min.   :0.000   Min.   :1.000   Min.   : 0.00  
    ##  1st Qu.:2.000   1st Qu.:1.000   1st Qu.:2.000   1st Qu.:30.33  
    ##  Median :2.000   Median :2.000   Median :2.000   Median :36.00  
    ##  Mean   :2.115   Mean   :1.526   Mean   :2.583   Mean   :37.79  
    ##  3rd Qu.:2.750   3rd Qu.:2.000   3rd Qu.:4.000   3rd Qu.:43.00  
    ##  Max.   :6.000   Max.   :5.000   Max.   :7.000   Max.   :78.00  
    ##       IAGE            ISEX            NCARS            AREA     
    ##  Min.   : 0.00   Min.   :0.0000   Min.   :0.000   Min.   :  30  
    ##  1st Qu.:28.00   1st Qu.:0.0000   1st Qu.:1.000   1st Qu.:  90  
    ##  Median :34.00   Median :1.0000   Median :2.000   Median : 110  
    ##  Mean   :36.89   Mean   :0.5447   Mean   :1.709   Mean   : 133  
    ##  3rd Qu.:43.00   3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.: 150  
    ##  Max.   :78.00   Max.   :1.0000   Max.   :5.000   Max.   :2250  
    ##     BEDROOM           PARK           BEDSIZE        PARKSIZE     
    ##  Min.   :0.000   Min.   :0.0000   Min.   :0.00   Min.   :0.0000  
    ##  1st Qu.:2.000   1st Qu.:0.0000   1st Qu.:0.80   1st Qu.:0.0000  
    ##  Median :3.000   Median :1.0000   Median :1.00   Median :0.2500  
    ##  Mean   :2.877   Mean   :0.8298   Mean   :1.35   Mean   :0.4662  
    ##  3rd Qu.:3.000   3rd Qu.:1.0000   3rd Qu.:1.50   3rd Qu.:1.0000  
    ##  Max.   :7.000   Max.   :4.0000   Max.   :5.00   Max.   :3.0000  
    ##      RAGE10            TCBD            DISTHTC             TWCBD        
    ##  Min.   :0.0000   Min.   : 0.7817   Min.   :   49.05   Min.   : 0.3116  
    ##  1st Qu.:0.0000   1st Qu.: 9.9509   1st Qu.:  400.47   1st Qu.: 5.2482  
    ##  Median :0.0000   Median :23.8093   Median :  719.04   Median : 9.8600  
    ##  Mean   :0.2426   Mean   :24.7046   Mean   : 1346.98   Mean   :16.9986  
    ##  3rd Qu.:0.0000   3rd Qu.:35.6981   3rd Qu.: 1525.45   3rd Qu.:25.2658  
    ##  Max.   :1.0000   Max.   :73.2820   Max.   :17732.68   Max.   :67.8190  
    ##      TDWWK           HEADH           POPDENS          EDUINDEX      
    ##  Min.   : 0.00   Min.   :0.0000   Min.   :  0.00   Min.   :0.00000  
    ##  1st Qu.:10.14   1st Qu.:1.0000   1st Qu.: 45.86   1st Qu.:0.09601  
    ##  Median :22.18   Median :1.0000   Median : 83.17   Median :0.17319  
    ##  Mean   :23.53   Mean   :0.8638   Mean   : 92.00   Mean   :0.19880  
    ##  3rd Qu.:33.72   3rd Qu.:1.0000   3rd Qu.:135.01   3rd Qu.:0.27699  
    ##  Max.   :80.72   Max.   :1.0000   Max.   :255.55   Max.   :0.65966  
    ##     GRAVCPC           GRAVCPT           GRAVPCPT          NSTRTC     
    ##  Min.   :0.06528   Min.   :0.02569   Min.   :0.4534   Min.   : 0.00  
    ##  1st Qu.:0.25301   1st Qu.:0.22117   1st Qu.:0.9985   1st Qu.:14.00  
    ##  Median :0.30924   Median :0.28129   Median :1.0911   Median :22.00  
    ##  Mean   :0.28980   Mean   :0.26186   Mean   :1.1517   Mean   :22.22  
    ##  3rd Qu.:0.34041   3rd Qu.:0.31036   3rd Qu.:1.2224   3rd Qu.:30.00  
    ##  Max.   :0.39395   Max.   :0.36508   Max.   :2.8799   Max.   :84.00  
    ##      DISTHW             DIVIDX          ACTDENS            DISTCBD       
    ##  Min.   :   74.67   Min.   :0.2634   Min.   : 0.02199   Min.   :  148.9  
    ##  1st Qu.:  712.79   1st Qu.:0.3239   1st Qu.: 1.31611   1st Qu.: 2025.3  
    ##  Median : 1338.74   Median :0.3548   Median : 2.49377   Median : 5542.3  
    ##  Mean   : 1883.41   Mean   :0.3777   Mean   : 5.77963   Mean   : 7967.4  
    ##  3rd Qu.: 2533.30   3rd Qu.:0.3970   3rd Qu.: 4.85162   3rd Qu.:11803.2  
    ##  Max.   :16590.06   Max.   :0.6500   Max.   :63.18205   Max.   :44004.6

#### Make ID as row names or case number

``` r
df<-data.frame(df, row.names = 1)
```

#### Evaluating the assumptions for factoral analysis:

#### Let’s run a random regression model in order to evaluate some assumptions

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

![](README_files/2-FactorAnalysis/unnamed-chunk-11-1.png)<!-- -->

-   **Linearity**

``` r
qqnorm(standardized)
abline(0,1)
```

![](README_files/2-FactorAnalysis/unnamed-chunk-12-1.png)<!-- -->

-   **Homogeneity**

``` r
plot(fitted, standardized)
abline(0,0)
abline(v=0)
```

![](README_files/2-FactorAnalysis/unnamed-chunk-13-1.png)<!-- -->

#### Examine correlations

``` r
corr_matrix <- cor(df, method = "pearson")
corr_matrix
```

    ##              DWELCLAS       INCOME      CHILD13          H18     HEMPLOY
    ## DWELCLAS  1.000000000  0.147143110  0.024684232 -0.048447283  0.03327254
    ## INCOME    0.147143110  1.000000000  0.129397042  0.360207827  0.30754469
    ## CHILD13   0.024684232  0.129397042  1.000000000 -0.042835437  0.18080310
    ## H18      -0.048447283  0.360207827 -0.042835437  1.000000000  0.47037990
    ## HEMPLOY   0.033272536  0.307544689  0.180803095  0.470379898  1.00000000
    ## HSIZE    -0.014940129  0.382460069  0.603726731  0.737385427  0.46896904
    ## AVADUAGE -0.142406030  0.164481895  0.002346507  0.119867627 -0.13835843
    ## IAGE     -0.059931293  0.201393335  0.073684495  0.041547529 -0.16384886
    ## ISEX     -0.035351143  0.087979201 -0.021167354  0.118234096 -0.03209938
    ## NCARS     0.032124894  0.438639549  0.091049900  0.565437239  0.46269514
    ## AREA      0.096292307  0.113240176  0.085276299  0.205254365  0.07094398
    ## BEDROOM   0.117320837  0.276999727  0.235126992  0.366781885  0.14691902
    ## PARK      0.214866484  0.205576805  0.211851647  0.119591572  0.15260566
    ## BEDSIZE   0.105091675 -0.195384556 -0.318182468 -0.515604318 -0.41529978
    ## PARKSIZE  0.207388396  0.094162658  0.195520356 -0.076718350  0.03298755
    ## RAGE10    0.210598575 -0.037728773  0.087026464 -0.140111551  0.06123081
    ## TCBD     -0.025763972 -0.081436447  0.013164695  0.078022792  0.09738298
    ## DISTHTC   0.008562010 -0.043498313 -0.032138159  0.081147972  0.06389792
    ## TWCBD    -0.003763337 -0.095354321  0.060310380  0.022236335  0.02993614
    ## TDWWK    -0.002630514 -0.004283219  0.073147715  0.076823913  0.17800356
    ## HEADH     0.064973501 -0.061107459  0.163142463 -0.547813174 -0.21355625
    ## POPDENS  -0.111043259 -0.096750069 -0.071362520 -0.011338703 -0.07242689
    ## EDUINDEX  0.158429562  0.205589925  0.048535709  0.006297921 -0.01903140
    ## GRAVCPC   0.049480878  0.112533526 -0.022256298 -0.052225279 -0.07714384
    ## GRAVCPT  -0.059634851  0.057883637 -0.064660339 -0.029811727 -0.13864355
    ## GRAVPCPT  0.135573345  0.042050396  0.063002785 -0.025507126  0.08274901
    ## NSTRTC   -0.034576923 -0.057521843  0.004990959  0.010613944 -0.06185120
    ## DISTHW    0.033337038 -0.094071166 -0.022635955 -0.042087150 -0.04876244
    ## DIVIDX    0.088063254  0.119139507 -0.012047611 -0.069068917 -0.08277455
    ## ACTDENS   0.020595926  0.057346222 -0.014894213 -0.053983685 -0.09722306
    ## DISTCBD  -0.037465061 -0.028461539  0.021915688  0.071160729  0.07370984
    ##                 HSIZE     AVADUAGE         IAGE         ISEX       NCARS
    ## DWELCLAS -0.014940129 -0.142406030 -0.059931293 -0.035351143  0.03212489
    ## INCOME    0.382460069  0.164481895  0.201393335  0.087979201  0.43863955
    ## CHILD13   0.603726731  0.002346507  0.073684495 -0.021167354  0.09104990
    ## H18       0.737385427  0.119867627  0.041547529  0.118234096  0.56543724
    ## HEMPLOY   0.468969037 -0.138358425 -0.163848862 -0.032099376  0.46269514
    ## HSIZE     1.000000000  0.100535868  0.108611117  0.098298370  0.51264719
    ## AVADUAGE  0.100535868  1.000000000  0.628251985  0.104570045  0.09818663
    ## IAGE      0.108611117  0.628251985  1.000000000  0.213558861 -0.02435079
    ## ISEX      0.098298370  0.104570045  0.213558861  1.000000000  0.06085179
    ## NCARS     0.512647186  0.098186631 -0.024350788  0.060851789  1.00000000
    ## AREA      0.222553496  0.002861041 -0.004694367 -0.053687929  0.07935207
    ## BEDROOM   0.459155061  0.140642865  0.187795297  0.094133992  0.20202778
    ## PARK      0.224971806 -0.004845322  0.021492441 -0.006306264  0.25527637
    ## BEDSIZE  -0.611577478 -0.029252915  0.007886531 -0.005405153 -0.36236012
    ## PARKSIZE  0.060153708 -0.061220537  0.001259403  0.018979890  0.01132456
    ## RAGE10   -0.061396409 -0.149649329 -0.126937918 -0.050771483  0.02929568
    ## TCBD      0.057103158  0.060146947 -0.002626011 -0.036246526  0.14826380
    ## DISTHTC   0.031261293  0.043507314 -0.008590903 -0.048818916  0.11464415
    ## TWCBD     0.046329108  0.064153780  0.057536769  0.039913016  0.08991808
    ## TDWWK     0.090432041 -0.089659751 -0.130125353 -0.074318481  0.13220765
    ## HEADH    -0.296255753 -0.171936202  0.357723277  0.048079220 -0.29861879
    ## POPDENS  -0.068274216 -0.055392271 -0.041062608  0.093524107 -0.18634746
    ## EDUINDEX  0.049769170  0.062919851  0.087126736  0.003389406  0.07256928
    ## GRAVCPC  -0.045383407 -0.040822704  0.011802041  0.026098330 -0.08972425
    ## GRAVCPT  -0.057545119 -0.055672548 -0.018682152  0.046923676 -0.13437429
    ## GRAVPCPT  0.020625250  0.020853827  0.006259520 -0.042465999  0.05460544
    ## NSTRTC    0.009172641 -0.036622852 -0.021145073  0.059931515 -0.09195698
    ## DISTHW   -0.050158852 -0.024023159 -0.061570863  0.006991340 -0.06952569
    ## DIVIDX   -0.049812164 -0.083238006 -0.049353016 -0.005478992 -0.09175142
    ## ACTDENS  -0.043518630 -0.068716907 -0.015664249  0.063143995 -0.18075832
    ## DISTCBD   0.063371403  0.121593563  0.063592991 -0.042005935  0.15979842
    ##                  AREA      BEDROOM         PARK      BEDSIZE     PARKSIZE
    ## DWELCLAS  0.096292307  0.117320837  0.214866484  0.105091675  0.207388396
    ## INCOME    0.113240176  0.276999727  0.205576805 -0.195384556  0.094162658
    ## CHILD13   0.085276299  0.235126992  0.211851647 -0.318182468  0.195520356
    ## H18       0.205254365  0.366781885  0.119591572 -0.515604318 -0.076718350
    ## HEMPLOY   0.070943981  0.146919025  0.152605661 -0.415299776  0.032987549
    ## HSIZE     0.222553496  0.459155061  0.224971806 -0.611577478  0.060153708
    ## AVADUAGE  0.002861041  0.140642865 -0.004845322 -0.029252915 -0.061220537
    ## IAGE     -0.004694367  0.187795297  0.021492441  0.007886531  0.001259403
    ## ISEX     -0.053687929  0.094133992 -0.006306264 -0.005405153  0.018979890
    ## NCARS     0.079352074  0.202027778  0.255276375 -0.362360116  0.011324565
    ## AREA      1.000000000  0.443948570  0.344799398  0.084392972  0.115300184
    ## BEDROOM   0.443948570  1.000000000  0.238933964  0.285888024  0.118810327
    ## PARK      0.344799398  0.238933964  1.000000000 -0.059769864  0.828536142
    ## BEDSIZE   0.084392972  0.285888024 -0.059769864  1.000000000  0.044254068
    ## PARKSIZE  0.115300184  0.118810327  0.828536142  0.044254068  1.000000000
    ## RAGE10    0.032038341 -0.078495840  0.372142057 -0.014257339  0.422571709
    ## TCBD      0.168270236  0.078168099  0.166233866 -0.023817286  0.108913221
    ## DISTHTC   0.070937670  0.027109126  0.132639880 -0.040940642  0.113602518
    ## TWCBD     0.060109770  0.029631737  0.051264114 -0.008520460  0.020273034
    ## TDWWK     0.083175273  0.003370623  0.159036813 -0.116707964  0.137573627
    ## HEADH    -0.177592039 -0.132666146 -0.005740114  0.193484622  0.118212775
    ## POPDENS  -0.164154863 -0.097244114 -0.328648622 -0.004293009 -0.289448932
    ## EDUINDEX  0.019545875  0.074701858  0.049632471  0.017704369  0.057911972
    ## GRAVCPC  -0.140882619 -0.085171154 -0.158552391  0.004172174 -0.100459644
    ## GRAVCPT  -0.110912993 -0.038993436 -0.232335349  0.031000190 -0.210109046
    ## GRAVPCPT  0.039855985 -0.015210524  0.134472445 -0.010525967  0.150658324
    ## NSTRTC   -0.085843518 -0.044638533 -0.215132938 -0.003977355 -0.193786458
    ## DISTHW   -0.005235821 -0.009477106 -0.041209913  0.031031548 -0.042558711
    ## DIVIDX   -0.051655460 -0.009323922 -0.125213388  0.079371962 -0.109090585
    ## ACTDENS  -0.085630878 -0.020119290 -0.223248381  0.045677489 -0.213333732
    ## DISTCBD   0.196469248  0.092532883  0.199626784 -0.028647234  0.127325546
    ##               RAGE10         TCBD      DISTHTC        TWCBD        TDWWK
    ## DWELCLAS  0.21059857 -0.025763972  0.008562010 -0.003763337 -0.002630514
    ## INCOME   -0.03772877 -0.081436447 -0.043498313 -0.095354321 -0.004283219
    ## CHILD13   0.08702646  0.013164695 -0.032138159  0.060310380  0.073147715
    ## H18      -0.14011155  0.078022792  0.081147972  0.022236335  0.076823913
    ## HEMPLOY   0.06123081  0.097382975  0.063897917  0.029936143  0.178003556
    ## HSIZE    -0.06139641  0.057103158  0.031261293  0.046329108  0.090432041
    ## AVADUAGE -0.14964933  0.060146947  0.043507314  0.064153780 -0.089659751
    ## IAGE     -0.12693792 -0.002626011 -0.008590903  0.057536769 -0.130125353
    ## ISEX     -0.05077148 -0.036246526 -0.048818916  0.039913016 -0.074318481
    ## NCARS     0.02929568  0.148263799  0.114644154  0.089918084  0.132207654
    ## AREA      0.03203834  0.168270236  0.070937670  0.060109770  0.083175273
    ## BEDROOM  -0.07849584  0.078168099  0.027109126  0.029631737  0.003370623
    ## PARK      0.37214206  0.166233866  0.132639880  0.051264114  0.159036813
    ## BEDSIZE  -0.01425734 -0.023817286 -0.040940642 -0.008520460 -0.116707964
    ## PARKSIZE  0.42257171  0.108913221  0.113602518  0.020273034  0.137573627
    ## RAGE10    1.00000000  0.170627527  0.241766551  0.121882052  0.108996972
    ## TCBD      0.17062753  1.000000000  0.530724338  0.432501738  0.454807965
    ## DISTHTC   0.24176655  0.530724338  1.000000000  0.163386609  0.334123510
    ## TWCBD     0.12188205  0.432501738  0.163386609  1.000000000 -0.039051519
    ## TDWWK     0.10899697  0.454807965  0.334123510 -0.039051519  1.000000000
    ## HEADH     0.13783504 -0.082659691 -0.045848685  0.012995658 -0.057340739
    ## POPDENS  -0.29277651 -0.386413815 -0.442270662 -0.145105217 -0.238077126
    ## EDUINDEX -0.18641419 -0.427796501 -0.287839234 -0.222189961 -0.259277889
    ## GRAVCPC  -0.19073768 -0.887289680 -0.571600365 -0.421392702 -0.473362298
    ## GRAVCPT  -0.23368131 -0.724328225 -0.641722497 -0.240765582 -0.438093260
    ## GRAVPCPT  0.11894245 -0.006885557  0.228517739 -0.162958869  0.089406239
    ## NSTRTC   -0.24404047 -0.340400923 -0.290243939 -0.174407740 -0.159533078
    ## DISTHW    0.08961613  0.440443894  0.405245673  0.252368854  0.190964770
    ## DIVIDX   -0.06113048 -0.420705579 -0.103355338 -0.159112374 -0.216637836
    ## ACTDENS  -0.16974988 -0.475645321 -0.208874552 -0.180253050 -0.248317272
    ## DISTCBD   0.15917217  0.849257124  0.578355455  0.319922514  0.410541329
    ##                 HEADH      POPDENS     EDUINDEX      GRAVCPC     GRAVCPT
    ## DWELCLAS  0.064973501 -0.111043259  0.158429562  0.049480878 -0.05963485
    ## INCOME   -0.061107459 -0.096750069  0.205589925  0.112533526  0.05788364
    ## CHILD13   0.163142463 -0.071362520  0.048535709 -0.022256298 -0.06466034
    ## H18      -0.547813174 -0.011338703  0.006297921 -0.052225279 -0.02981173
    ## HEMPLOY  -0.213556247 -0.072426887 -0.019031399 -0.077143839 -0.13864355
    ## HSIZE    -0.296255753 -0.068274216  0.049769170 -0.045383407 -0.05754512
    ## AVADUAGE -0.171936202 -0.055392271  0.062919851 -0.040822704 -0.05567255
    ## IAGE      0.357723277 -0.041062608  0.087126736  0.011802041 -0.01868215
    ## ISEX      0.048079220  0.093524107  0.003389406  0.026098330  0.04692368
    ## NCARS    -0.298618787 -0.186347455  0.072569285 -0.089724253 -0.13437429
    ## AREA     -0.177592039 -0.164154863  0.019545875 -0.140882619 -0.11091299
    ## BEDROOM  -0.132666146 -0.097244114  0.074701858 -0.085171154 -0.03899344
    ## PARK     -0.005740114 -0.328648622  0.049632471 -0.158552391 -0.23233535
    ## BEDSIZE   0.193484622 -0.004293009  0.017704369  0.004172174  0.03100019
    ## PARKSIZE  0.118212775 -0.289448932  0.057911972 -0.100459644 -0.21010905
    ## RAGE10    0.137835044 -0.292776513 -0.186414192 -0.190737675 -0.23368131
    ## TCBD     -0.082659691 -0.386413815 -0.427796501 -0.887289680 -0.72432823
    ## DISTHTC  -0.045848685 -0.442270662 -0.287839234 -0.571600365 -0.64172250
    ## TWCBD     0.012995658 -0.145105217 -0.222189961 -0.421392702 -0.24076558
    ## TDWWK    -0.057340739 -0.238077126 -0.259277889 -0.473362298 -0.43809326
    ## HEADH     1.000000000 -0.024609810  0.019387113  0.062658099  0.02598758
    ## POPDENS  -0.024609810  1.000000000  0.041494488  0.350347551  0.44583283
    ## EDUINDEX  0.019387113  0.041494488  1.000000000  0.550865441  0.33853111
    ## GRAVCPC   0.062658099  0.350347551  0.550865441  1.000000000  0.64615269
    ## GRAVCPT   0.025987580  0.445832828  0.338531105  0.646152694  1.00000000
    ## GRAVPCPT  0.027673512 -0.189631134  0.115360588  0.192108769 -0.57295326
    ## NSTRTC   -0.034914728  0.507599069  0.105330284  0.327592960  0.35187170
    ## DISTHW   -0.011422642 -0.099246320 -0.363710306 -0.583222568 -0.30822450
    ## DIVIDX    0.045759317  0.005538559  0.279580273  0.362278145  0.29169880
    ## ACTDENS   0.042393215  0.296121488  0.156467398  0.357409393  0.39564644
    ## DISTCBD  -0.065185785 -0.458868344 -0.315956164 -0.759293373 -0.69749368
    ##              GRAVPCPT       NSTRTC       DISTHW       DIVIDX     ACTDENS
    ## DWELCLAS  0.135573345 -0.034576923  0.033337038  0.088063254  0.02059593
    ## INCOME    0.042050396 -0.057521843 -0.094071166  0.119139507  0.05734622
    ## CHILD13   0.063002785  0.004990959 -0.022635955 -0.012047611 -0.01489421
    ## H18      -0.025507126  0.010613944 -0.042087150 -0.069068917 -0.05398368
    ## HEMPLOY   0.082749005 -0.061851205 -0.048762441 -0.082774554 -0.09722306
    ## HSIZE     0.020625250  0.009172641 -0.050158852 -0.049812164 -0.04351863
    ## AVADUAGE  0.020853827 -0.036622852 -0.024023159 -0.083238006 -0.06871691
    ## IAGE      0.006259520 -0.021145073 -0.061570863 -0.049353016 -0.01566425
    ## ISEX     -0.042465999  0.059931515  0.006991340 -0.005478992  0.06314399
    ## NCARS     0.054605439 -0.091956976 -0.069525687 -0.091751418 -0.18075832
    ## AREA      0.039855985 -0.085843518 -0.005235821 -0.051655460 -0.08563088
    ## BEDROOM  -0.015210524 -0.044638533 -0.009477106 -0.009323922 -0.02011929
    ## PARK      0.134472445 -0.215132938 -0.041209913 -0.125213388 -0.22324838
    ## BEDSIZE  -0.010525967 -0.003977355  0.031031548  0.079371962  0.04567749
    ## PARKSIZE  0.150658324 -0.193786458 -0.042558711 -0.109090585 -0.21333373
    ## RAGE10    0.118942455 -0.244040474  0.089616128 -0.061130482 -0.16974988
    ## TCBD     -0.006885557 -0.340400923  0.440443894 -0.420705579 -0.47564532
    ## DISTHTC   0.228517739 -0.290243939  0.405245673 -0.103355338 -0.20887455
    ## TWCBD    -0.162958869 -0.174407740  0.252368854 -0.159112374 -0.18025305
    ## TDWWK     0.089406239 -0.159533078  0.190964770 -0.216637836 -0.24831727
    ## HEADH     0.027673512 -0.034914728 -0.011422642  0.045759317  0.04239322
    ## POPDENS  -0.189631134  0.507599069 -0.099246320  0.005538559  0.29612149
    ## EDUINDEX  0.115360588  0.105330284 -0.363710306  0.279580273  0.15646740
    ## GRAVCPC   0.192108769  0.327592960 -0.583222568  0.362278145  0.35740939
    ## GRAVCPT  -0.572953257  0.351871701 -0.308224503  0.291698798  0.39564644
    ## GRAVPCPT  1.000000000 -0.101771699 -0.212827899  0.067696290 -0.08440319
    ## NSTRTC   -0.101771699  1.000000000 -0.268272304  0.191562182  0.34547838
    ## DISTHW   -0.212827899 -0.268272304  1.000000000 -0.174115874 -0.01361610
    ## DIVIDX    0.067696290  0.191562182 -0.174115874  1.000000000  0.75926409
    ## ACTDENS  -0.084403192  0.345478384 -0.013616098  0.759264086  1.00000000
    ## DISTCBD   0.132486010 -0.297768090  0.263133855 -0.287605002 -0.40562597
    ##              DISTCBD
    ## DWELCLAS -0.03746506
    ## INCOME   -0.02846154
    ## CHILD13   0.02191569
    ## H18       0.07116073
    ## HEMPLOY   0.07370984
    ## HSIZE     0.06337140
    ## AVADUAGE  0.12159356
    ## IAGE      0.06359299
    ## ISEX     -0.04200593
    ## NCARS     0.15979842
    ## AREA      0.19646925
    ## BEDROOM   0.09253288
    ## PARK      0.19962678
    ## BEDSIZE  -0.02864723
    ## PARKSIZE  0.12732555
    ## RAGE10    0.15917217
    ## TCBD      0.84925712
    ## DISTHTC   0.57835546
    ## TWCBD     0.31992251
    ## TDWWK     0.41054133
    ## HEADH    -0.06518578
    ## POPDENS  -0.45886834
    ## EDUINDEX -0.31595616
    ## GRAVCPC  -0.75929337
    ## GRAVCPT  -0.69749368
    ## GRAVPCPT  0.13248601
    ## NSTRTC   -0.29776809
    ## DISTHW    0.26313386
    ## DIVIDX   -0.28760500
    ## ACTDENS  -0.40562597
    ## DISTCBD   1.00000000

#### Check for correlation adequacy (pattern between variables) - Bartlett’s Test

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

#### Check for sampling adequacy - KMO test

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
> (MSA)

#### Determine the number of factors to extract

**1.\_Parallel Analysis\_**

``` r
num_factors = fa.parallel(df, fm = "ml", fa = "fa")
```

![](README_files/2-FactorAnalysis/unnamed-chunk-17-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  8  and the number of components =  NA

> **Note:** fm = factor math; ml = maximum likelihood; fa = factor
> analysis

**2. *Kaiser Criterion***

``` r
sum(num_factors$fa.values > 1)
```

    ## [1] 4

> **Note:** Determines the number of factors with eigenvalue &gt; 1

**3. *Principal Component Analysis* (PCA)**

``` r
df_pca <- princomp(df, cor=TRUE)
```

> **Note:** cor = TRUE, standardizes your dataset

\# print variance that explains the components

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

``` r
loadings(df_pca) 
```

    ## 
    ## Loadings:
    ##          Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7 Comp.8 Comp.9 Comp.10
    ## DWELCLAS                0.279         0.169  0.102         0.147         0.462 
    ## INCOME           0.308        -0.122                       0.237         0.270 
    ## CHILD13          0.186  0.129               -0.336  0.362 -0.428 -0.235        
    ## H18              0.396 -0.218                0.107         0.116  0.125        
    ## HEMPLOY   0.101  0.292         0.256                                     0.245 
    ## HSIZE            0.436                      -0.131  0.177 -0.182               
    ## AVADUAGE                      -0.504 -0.212                             -0.303 
    ## IAGE                          -0.584 -0.229         0.113                      
    ## ISEX                          -0.226                       0.158  0.636  0.175 
    ## NCARS     0.124  0.333                                     0.253         0.142 
    ## AREA      0.104  0.131  0.113         0.402  0.141 -0.157 -0.263 -0.108 -0.115 
    ## BEDROOM          0.227        -0.280  0.410  0.120        -0.248         0.173 
    ## PARK      0.167  0.150  0.398         0.138 -0.163                0.178 -0.274 
    ## BEDSIZE         -0.277  0.171 -0.211  0.328  0.219 -0.182                0.194 
    ## PARKSIZE  0.129         0.446               -0.223                0.240 -0.270 
    ## RAGE10    0.139         0.288  0.179        -0.159  0.107  0.235  0.101 -0.137 
    ## TCBD      0.356 -0.102 -0.129                                                  
    ## DISTHTC   0.274                              0.266  0.186                      
    ## TWCBD     0.159        -0.111 -0.121  0.152 -0.183  0.130  0.209 -0.208        
    ## TDWWK     0.219                0.169                      -0.234  0.182        
    ## HEADH           -0.199  0.236 -0.157 -0.164 -0.255  0.298                0.350 
    ## POPDENS  -0.234        -0.216               -0.183        -0.193  0.303        
    ## EDUINDEX -0.178  0.157  0.219                0.138 -0.110        -0.229        
    ## GRAVCPC  -0.343  0.128  0.166        -0.156        -0.120                      
    ## GRAVCPT  -0.331                       0.210 -0.214         0.176 -0.111        
    ## GRAVPCPT                0.249        -0.407  0.366        -0.270  0.122        
    ## NSTRTC   -0.205        -0.121                             -0.312  0.358        
    ## DISTHW    0.176 -0.152 -0.176         0.222         0.292  0.155               
    ## DIVIDX   -0.189         0.116                0.381  0.449               -0.214 
    ## ACTDENS  -0.233                       0.165  0.253  0.486         0.118 -0.193 
    ## DISTCBD   0.334                              0.127                             
    ##          Comp.11 Comp.12 Comp.13 Comp.14 Comp.15 Comp.16 Comp.17 Comp.18
    ## DWELCLAS  0.152   0.556   0.203   0.206   0.246   0.216           0.173 
    ## INCOME   -0.296   0.101  -0.127  -0.181          -0.188   0.164  -0.516 
    ## CHILD13                   0.121   0.164  -0.171   0.200   0.112  -0.232 
    ## H18                                       0.130          -0.185   0.277 
    ## HEMPLOY                  -0.122  -0.278  -0.110  -0.181   0.115   0.359 
    ## HSIZE                     0.151                   0.108                 
    ## AVADUAGE          0.298   0.134          -0.101   0.162   0.262         
    ## IAGE              0.132          -0.172   0.201                   0.269 
    ## ISEX      0.173  -0.476           0.335   0.143   0.118   0.119  -0.114 
    ## NCARS                    -0.233          -0.175   0.122  -0.105  -0.242 
    ## AREA                                      0.600  -0.280          -0.247 
    ## BEDROOM          -0.160   0.113  -0.166  -0.254   0.201  -0.116   0.179 
    ## PARK                                             -0.247                 
    ## BEDSIZE          -0.117          -0.148  -0.365   0.143                 
    ## PARKSIZE                                 -0.242  -0.176           0.156 
    ## RAGE10    0.143           0.113  -0.413   0.178   0.405          -0.189 
    ## TCBD                     -0.192   0.103                                 
    ## DISTHTC                   0.119                          -0.532         
    ## TWCBD     0.589          -0.281          -0.101  -0.136   0.230   0.121 
    ## TDWWK    -0.498          -0.176   0.158           0.187   0.367   0.204 
    ## HEADH    -0.128  -0.123  -0.208  -0.179   0.174  -0.188  -0.240         
    ## POPDENS           0.253          -0.119  -0.124  -0.300          -0.100 
    ## EDUINDEX                 -0.134   0.550          -0.105  -0.245         
    ## GRAVCPC                   0.102                                         
    ## GRAVCPT  -0.178                           0.103   0.144                 
    ## GRAVPCPT  0.314           0.182                  -0.184   0.151         
    ## NSTRTC    0.143   0.403  -0.368                   0.123  -0.322  -0.139 
    ## DISTHW   -0.151   0.109   0.459   0.132  -0.157  -0.307          -0.124 
    ## DIVIDX                   -0.222                           0.113         
    ## ACTDENS                                          -0.124   0.137         
    ## DISTCBD                  -0.307                                         
    ##          Comp.19 Comp.20 Comp.21 Comp.22 Comp.23 Comp.24 Comp.25 Comp.26
    ## DWELCLAS  0.160           0.121           0.152                         
    ## INCOME    0.172   0.170  -0.298  -0.314                                 
    ## CHILD13                   0.117           0.126                  -0.154 
    ## H18       0.159  -0.113  -0.264          -0.128                         
    ## HEMPLOY  -0.461   0.438   0.256                                         
    ## HSIZE     0.127          -0.119                                         
    ## AVADUAGE -0.241           0.187                           0.229   0.403 
    ## IAGE                                             -0.130  -0.257  -0.521 
    ## ISEX              0.118                                                 
    ## NCARS            -0.465   0.433   0.359                                 
    ## AREA     -0.193           0.195                                         
    ## BEDROOM                  -0.160                   0.116   0.141   0.215 
    ## PARK                      0.178                                         
    ## BEDSIZE                                                  -0.119  -0.242 
    ## PARKSIZE  0.105                                                         
    ## RAGE10   -0.399          -0.311   0.119  -0.155                         
    ## TCBD              0.157  -0.146   0.245          -0.129  -0.122   0.152 
    ## DISTHTC  -0.125                  -0.394   0.541                         
    ## TWCBD            -0.305  -0.117  -0.371                                 
    ## TDWWK    -0.189  -0.431  -0.118  -0.226                                 
    ## HEADH            -0.172                                   0.235   0.412 
    ## POPDENS  -0.192  -0.138  -0.316   0.296   0.476  -0.149                 
    ## EDUINDEX -0.513          -0.271          -0.107          -0.136         
    ## GRAVCPC          -0.118          -0.139           0.223   0.293  -0.214 
    ## GRAVCPT                                   0.141   0.171   0.323  -0.173 
    ## GRAVPCPT         -0.138                  -0.148           0.147         
    ## NSTRTC                    0.161  -0.261  -0.375                         
    ## DISTHW   -0.192  -0.130   0.135          -0.397           0.297  -0.158 
    ## DIVIDX                            0.103          -0.581   0.300         
    ## ACTDENS                           0.187           0.570  -0.362         
    ## DISTCBD           0.276  -0.162   0.281           0.382   0.447  -0.330 
    ##          Comp.27 Comp.28 Comp.29 Comp.30 Comp.31
    ## DWELCLAS                                        
    ## INCOME                                          
    ## CHILD13           0.132                   0.438 
    ## H18               0.420                   0.522 
    ## HEMPLOY                                         
    ## HSIZE             0.275                  -0.723 
    ## AVADUAGE          0.166                         
    ## IAGE             -0.162                         
    ## ISEX                                            
    ## NCARS     0.197  -0.110                         
    ## AREA      0.195                                 
    ## BEDROOM  -0.116  -0.480                         
    ## PARK     -0.666   0.142  -0.100                 
    ## BEDSIZE           0.558                         
    ## PARKSIZE  0.633  -0.127                         
    ## RAGE10                                          
    ## TCBD      0.114   0.137  -0.757                 
    ## DISTHTC                  -0.121                 
    ## TWCBD                                           
    ## TDWWK                                           
    ## HEADH             0.214                         
    ## POPDENS                                         
    ## EDUINDEX                                        
    ## GRAVCPC                  -0.470   0.566         
    ## GRAVCPT           0.103  -0.242  -0.638         
    ## GRAVPCPT                         -0.493         
    ## NSTRTC                                          
    ## DISTHW                                          
    ## DIVIDX                                          
    ## ACTDENS   0.109                                 
    ## DISTCBD                   0.244   0.109         
    ## 
    ##                Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7 Comp.8 Comp.9
    ## SS loadings     1.000  1.000  1.000  1.000  1.000  1.000  1.000  1.000  1.000
    ## Proportion Var  0.032  0.032  0.032  0.032  0.032  0.032  0.032  0.032  0.032
    ## Cumulative Var  0.032  0.065  0.097  0.129  0.161  0.194  0.226  0.258  0.290
    ##                Comp.10 Comp.11 Comp.12 Comp.13 Comp.14 Comp.15 Comp.16 Comp.17
    ## SS loadings      1.000   1.000   1.000   1.000   1.000   1.000   1.000   1.000
    ## Proportion Var   0.032   0.032   0.032   0.032   0.032   0.032   0.032   0.032
    ## Cumulative Var   0.323   0.355   0.387   0.419   0.452   0.484   0.516   0.548
    ##                Comp.18 Comp.19 Comp.20 Comp.21 Comp.22 Comp.23 Comp.24 Comp.25
    ## SS loadings      1.000   1.000   1.000   1.000   1.000   1.000   1.000   1.000
    ## Proportion Var   0.032   0.032   0.032   0.032   0.032   0.032   0.032   0.032
    ## Cumulative Var   0.581   0.613   0.645   0.677   0.710   0.742   0.774   0.806
    ##                Comp.26 Comp.27 Comp.28 Comp.29 Comp.30 Comp.31
    ## SS loadings      1.000   1.000   1.000   1.000   1.000   1.000
    ## Proportion Var   0.032   0.032   0.032   0.032   0.032   0.032
    ## Cumulative Var   0.839   0.871   0.903   0.935   0.968   1.000

Scree Plot

plot(df\_pca,type=“lines”) fit$scores \#biplot(df\_pca)

# EXPLORATORY FACTOR ANALYSIS

## Model 1: No rotation

df\_factor &lt;- factanal(df, factors = 4, rotation = “none”,
scores=c(“regression”), fm = “ml”)

## Model 2: Rotation Varimax

df\_factor\_var &lt;- factanal(df, factors = 4, rotation = “varimax”,
scores=c(“regression”), fm = “ml”)

## Model 3: Rotation Oblimin

df\_factor\_obl &lt;- factanal(df, factors = 4, rotation = “oblimin”,
scores=c(“regression”), fm = “ml”)

# Let’s print out the results of df\_factor\_obl, and take a look.

print(df\_factor, digits=2, cutoff=0.3, sort=TRUE)

## Note: We used a cutoff of 0.3 due to the sample size is higher than 350 obs.

## Note: Variability contained in the factors = Communality + Uniqueness

## Note: Varimax assigns orthogonal rotation,

## and oblimin assigns oblique rotation.

\#Plot factor 1 against factor 2, and compare the results of different
rotations

## No Rotation

plot(df\_factor*l**o**a**d**i**n**g**s*\[,1\], *d**f*<sub>*f*</sub>*a**c**t**o**r*loadings\[,2\],
xlab = “Factor 1”, ylab = “Factor 2”, ylim = c(-1,1), xlim = c(-1,1),
main = “No rotation”) abline(h = 0, v = 0) load &lt;-
df\_factor$loadings\[,1:2\] text(text(load,labels=names(df),cex=.7,
col=“blue”)) abline(h = 0, v = 0)

## Varimax rotation

plot(df\_factor\_var*l**o**a**d**i**n**g**s*\[,1\], *d**f*<sub>*f*</sub>*a**c**t**o**r*<sub>*v*</sub>*a**r*loadings\[,2\],
xlab = “Factor 1”, ylab = “Factor 2”, ylim = c(-1,1), xlim = c(-1,1),
main = “Varimax rotation”) abline(h = 0, v = 0) \#Note: If you also want
to put the variable label here, just take out the \#. \#load &lt;-
df\_factor\_var$loadings\[,1:2\]
\#text(text(load,labels=names(df),cex=.7, col=“red”))

# Oblimin Rotation

plot(df\_factor\_obl*l**o**a**d**i**n**g**s*\[,1\], *d**f*<sub>*f*</sub>*a**c**t**o**r*<sub>*o*</sub>*b**l*loadings\[,2\],
xlab = “Factor 1”, ylab = “Factor 2”, ylim = c(-1,1), xlim = c(-1,1),
main = “Oblimin rotation”) abline(h = 0, v = 0) \#load &lt;-
df\_factor\_obl$loadings\[,1:2\]
\#text(text(load,labels=names(df),cex=.7, col=“yellow”)) \#abline(h = 0,
v = 0)

knitr::spin(hair = “Factor\_Analysis\_TDMLecture.R”)
