R basics
================

In this chapter we will introduce to the R basics and some exercises to
get familiar to how R works.

## Math operations

### Sum

``` r
1+1
```

    ## [1] 2

### Subtraction

``` r
5-2
```

    ## [1] 3

### Multiplication

``` r
2*2
```

    ## [1] 4

### Division

``` r
8/2
```

    ## [1] 4

### Round the number

``` r
round(3.14)
```

    ## [1] 3

``` r
round(3.14, 1) # The "1" indicates to round it up to 1 decimal digit.
```

    ## [1] 3.1

You can use help `?round` in the console to see the description of the
function, and the default arguments.

## Basic shortpaths

### Perform Combinations

``` r
c(1, 2, 3)
```

    ## [1] 1 2 3

``` r
c(1:3) # The ":" indicates a range between the first and second numbers. 
```

    ## [1] 1 2 3

### Create a comment with `ctrl + shift + m`

``` r
# Comments help you organize your code. The software will not run the comment. 
```

### Create a table

A simple table with the number of trips by car, PT, walking, and cycling
in a hypothetical street segment at a certain period.

**Define variables**

``` r
modes <- c("car", "PT", "walking", "cycling") # you can use "=" or "<-"
Trips = c(200, 50, 300, 150) # uppercase letters modify
```

**Join the variables to create a table**

``` r
table_example = data.frame(modes, Trips)
```

**Take a look at the table**

Visualize the table by clicking on the “Data” in the “Environment” page
or use :

``` r
View(table_example)
```

**Look at the first row**

``` r
table_example[1,] #rows and columns start from 1 in R, differently from Python which starts from 0.
```

    ##   modes Trips
    ## 1   car   200

**Look at first row and column**

``` r
table_example[1,1]
```

    ## [1] "car"

## Practical exercise

**Dataset:** the number of trips between all municipalities in the
Lisbon Metropolitan Area, Portugal \[@IMOB\].

### Import dataset

You can click directly in the file under the “Files” pan, or:

``` r
data = readRDS("Data/TRIPSmode.Rds")
```

After you type `"` you can use `tab` to navigate between folders and
files and `enter` to autocomplete.

### Take a first look at the data

**Summary statistics**

``` r
summary(data)
```

    ##     Origin          Destination            Total             Walk       
    ##  Length:315         Length:315         Min.   :     7   Min.   :     0  
    ##  Class :character   Class :character   1st Qu.:   330   1st Qu.:     0  
    ##  Mode  :character   Mode  :character   Median :  1090   Median :     0  
    ##                                        Mean   : 16825   Mean   :  4033  
    ##                                        3rd Qu.:  5374   3rd Qu.:     0  
    ##                                        Max.   :875144   Max.   :306289  
    ##       Bike              Car            PTransit            Other        
    ##  Min.   :   0.00   Min.   :     0   Min.   :     0.0   Min.   :    0.0  
    ##  1st Qu.:   0.00   1st Qu.:   263   1st Qu.:     5.0   1st Qu.:    0.0  
    ##  Median :   0.00   Median :   913   Median :   134.0   Median :    0.0  
    ##  Mean   :  80.19   Mean   :  9956   Mean   :  2602.6   Mean   :  152.4  
    ##  3rd Qu.:   0.00   3rd Qu.:  4408   3rd Qu.:   975.5   3rd Qu.:   62.5  
    ##  Max.   :5362.00   Max.   :349815   Max.   :202428.0   Max.   :11647.0

**Check the structure of the data**

``` r
str(data)
```

    ## 'data.frame':    315 obs. of  8 variables:
    ##  $ Origin     : chr  "Alcochete" "Alcochete" "Alcochete" "Alcochete" ...
    ##  $ Destination: chr  "Alcochete" "Almada" "Amadora" "Barreiro" ...
    ##  $ Total      : num  20478 567 188 867 114 ...
    ##  $ Walk       : num  6833 0 0 0 0 ...
    ##  $ Bike       : num  320 0 0 0 0 0 0 0 91 0 ...
    ##  $ Car        : num  12484 353 107 861 114 ...
    ##  $ PTransit   : num  833 0 81 5 0 ...
    ##  $ Other      : num  7 214 0 0 0 0 0 0 0 0 ...

**Check the first values of each variable**

``` r
data
```

``` r
head(data, 3) # first 3 values
```

    ##      Origin Destination Total Walk Bike   Car PTransit Other
    ## 1 Alcochete   Alcochete 20478 6833  320 12484      833     7
    ## 2 Alcochete      Almada   567    0    0   353        0   214
    ## 3 Alcochete     Amadora   188    0    0   107       81     0

**Check the number of rows (observations) and columns (variables)**

``` r
nrow(data)
```

    ## [1] 315

``` r
ncol(data)
```

    ## [1] 8

**Open the dataset**

``` r
View(data)
```

### Explore the data

**Check the total number of trips**

Use `$` to select a variable of the data

``` r
sum(data$Total)
```

    ## [1] 5299853

**Percentage of car trips related to the total**

``` r
sum(data$Car)/sum(data$Total) * 100
```

    ## [1] 59.17638

**Percentage of active trips related to the total**

``` r
(sum(data$Walk) + sum(data$Bike)) / sum(data$Total) * 100
```

    ## [1] 24.44883

### Modify original data

**Create a column with the sum of the number of trips for active modes**

``` r
data$Active = data$Walk + data$Bike
```

**Filter by condition (create new tables)**

Filter trips only with origin from Lisbon

``` r
data_Lisbon = data[data$Origin == "Lisboa",]
```

Filter trips with origin **different** from Lisbon

``` r
data_out_Lisbon = data[data$Origin != "Lisboa",]
```

Filter trips with origin **and** destination in Lisbon

``` r
data_in_Out_Lisbon = data[data$Origin == "Lisboa" & data$Destination == "Lisboa",]
```

**Remove the first column**

``` r
data = data[ ,-1] #first column
```

**Create a table only with origin, destination and walking trips**

There are many ways to do the same operation.

``` r
names(data)
```

    ## [1] "Destination" "Total"       "Walk"        "Bike"        "Car"        
    ## [6] "PTransit"    "Other"       "Active"

``` r
data_walk2 = data[ ,c(1,2,4)]
```

``` r
data_walk3 = data[ ,-c(3,5:9)]
```

### Export data

Save data in **.csv** and **.Rds**

``` r
# write.csv(data, 'Data/dataset_basic.csv', row.names = FALSE)
# saveRDS(data, 'Data/dataset_basic_2.Rds') #Choose a different file. 
```

### Import data

``` r
# csv_file = read.csv("data/dataset_basic.csv")
# rds_file = readRDS("data/dataset_basic_2.Rds")
```
