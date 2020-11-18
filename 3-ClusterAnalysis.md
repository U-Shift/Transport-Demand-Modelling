Cluster Analysis
================

## Example Airports

### Variables:

`Code` `Airport` `Ordem` `Passengers` `Movements` `Number of airlines`
`Main air line flights percentage` `Maximum percentage of traffic per
country` \# Number of Low Cost Airlines \# Low Cost Airlines percentage
\# Destinations \# Average\_route\_Distance \# Distance to closest
Similar Airport \# Airport Regional Relevance \# Distance to city km \#
Inhabitants corrected \# Number of visitors corrected \# GDP corrected
\# Cargoton

#### Import Libraries

``` r
library(readxl) # Library used for reading excel files
library(skimr) # Library used for summary statistics
library(tidyverse) # Library used in data science to perform exploratory data analysis
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.4     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(mclust) # Library used for model based clustering
```

    ## Package 'mclust' version 5.4.6
    ## Type 'citation("mclust")' for citing this R package in publications.

    ## 
    ## Attaching package: 'mclust'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

``` r
library(cluster) # Library used for cluster analysis
library(factoextra) # Library used for visualizing distances
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

#### Set working directory

``` r
setwd("G:/O meu disco/TDM - Lecture R/TDM github/Transport-Demand-Modelling/")
```

#### Import dataset

``` r
dataset <- read_excel("Data/Data_Aeroports_Clustersv1.xlsX")
```

#### Transform dataset into dataframe

``` r
df <- data.frame(dataset)
```

#### Summary statistics

``` r
skim(df)
```

|                                                  |      |
| :----------------------------------------------- | :--- |
| Name                                             | df   |
| Number of rows                                   | 32   |
| Number of columns                                | 21   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| character                                        | 2    |
| numeric                                          | 19   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| Code           |          0 |              1 |   3 |   3 |     0 |        32 |          0 |
| Airport        |          0 |              1 |   4 |  35 |     0 |        32 |          0 |

**Variable type: numeric**

| skim\_variable                       | n\_missing | complete\_rate |        mean |          sd |        p0 |        p25 |         p50 |         p75 |        p100 | hist  |
| :----------------------------------- | ---------: | -------------: | ----------: | ----------: | --------: | ---------: | ----------: | ----------: | ----------: | :---- |
| Ordem                                |          0 |              1 |       16.50 |        9.38 |      1.00 |       8.75 |       16.50 |       24.25 |       32.00 | ▇▇▇▇▇ |
| Passengers                           |          0 |              1 | 20750710.88 | 17601931.34 | 456698.00 | 8927021.50 | 17275317.50 | 28666511.50 | 67054745.00 | ▇▅▂▂▁ |
| Movements                            |          0 |              1 |   205111.16 |   143564.45 |   5698.00 |   82765.75 |   191742.50 |   258654.50 |   518018.00 | ▇▅▇▂▃ |
| Numberofairlines                     |          0 |              1 |       57.81 |       40.42 |      1.00 |      22.50 |       55.50 |       90.25 |      136.00 | ▇▆▅▆▃ |
| Mainairlineflightspercentage         |          0 |              1 |       33.78 |       22.08 |     12.00 |      22.00 |       28.50 |       33.00 |       95.00 | ▇▆▁▁▂ |
| Maximumpercentageoftrafficpercountry |          0 |              1 |       17.47 |        7.31 |      9.00 |      12.00 |       15.00 |       22.25 |       35.00 | ▇▂▃▂▂ |
| NumberofLCCflightsweekly             |          0 |              1 |      397.59 |      221.56 |     37.00 |     226.25 |      366.50 |      546.75 |      776.00 | ▅▇▅▅▆ |
| NumberofLowCostAirlines              |          0 |              1 |       11.59 |        5.60 |      1.00 |       7.75 |       12.00 |       16.00 |       23.00 | ▃▇▇▆▃ |
| LowCostAirlinespercentage            |          0 |              1 |       36.44 |       30.10 |      6.25 |      16.29 |       19.59 |       50.36 |      100.00 | ▇▂▁▁▂ |
| Destinations                         |          0 |              1 |      167.62 |       80.13 |     20.00 |     109.25 |      168.50 |      222.50 |      301.00 | ▃▇▆▇▆ |
| Average\_Route\_Distance             |          0 |              1 |     2275.19 |      930.28 |   1225.00 |    1599.50 |     2152.00 |     2765.00 |     5635.00 | ▇▆▂▁▁ |
| DistancetoclosestAirport             |          0 |              1 |       90.19 |       64.56 |     13.84 |      45.83 |       66.50 |      111.61 |      244.50 | ▇▇▃▁▂ |
| DistancetoclosestSimilarAirport      |          0 |              1 |      248.64 |      183.60 |     38.16 |      97.74 |      206.12 |      376.15 |      635.05 | ▇▅▃▁▃ |
| AirportRegionalrelevance             |          0 |              1 |        0.73 |        0.23 |      0.19 |       0.58 |        0.80 |        0.91 |        0.99 | ▁▃▁▆▇ |
| Distancetocitykm                     |          0 |              1 |       25.81 |       25.44 |      3.00 |       9.75 |       14.50 |       35.00 |      100.00 | ▇▂▁▁▁ |
| Inhanbitantscorrected                |          0 |              1 |  4528561.95 |  2590542.88 | 329240.50 | 2856960.30 |  4532760.00 |  6733158.88 |  9870818.00 | ▆▆▇▇▁ |
| numberofvisitorscorrected            |          0 |              1 |  2766002.58 |  2549773.72 |  80232.50 | 1018390.89 |  1896295.60 |  3450491.78 |  9732062.00 | ▇▃▁▂▁ |
| GDPcorrected                         |          0 |              1 |    30160.75 |    10510.93 |   8500.00 |   25000.00 |    31150.00 |    35550.00 |    56600.00 | ▃▅▇▃▁ |
| Cargoton                             |          0 |              1 |   236531.76 |   478310.12 |      0.00 |   10325.00 |    72749.85 |   153372.85 |  1819000.00 | ▇▁▁▁▁ |

#### Now let us plot an example and take a look

``` r
plot(Numberofairlines ~ Destinations, df)
with(df, text(Numberofairlines ~ Destinations, label = Airport, pos = 4, cex = 0.6))
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-6-1.png)<!-- -->

> **Note:** You can already guess the number of clusters by visualizing
> the two variables. However, this is not clear and it does not consider
> the other variables.

#### Treat the data before performing a cluster analysis

  - In this example we do not have missing values. In case you do have
    in the future, you can take out the missing values with listwise
    deletion.

<!-- end list -->

``` r
 df <- na.omit(df)
```

  - Leave only continuous variables, and take out “Ordem”

<!-- end list -->

``` r
  drop <- c("Code","Airport", "Ordem")
  df_reduced = df[,!(names(df) %in% drop)]
```

Analyze how the scale of the values of the variables are different

``` r
head(df_reduced)
```

    ##   Passengers Movements Numberofairlines Mainairlineflightspercentage
    ## 1    9830987    119322               64                           18
    ## 2    9742300    132200               29                           33
    ## 3    9155665    101557               47                           17
    ## 4    9139479     74281               35                           29
    ## 5    9129053     83013               11                           37
    ## 6    8320927    115934               36                           31
    ##   Maximumpercentageoftrafficpercountry NumberofLCCflightsweekly
    ## 1                                   20                      256
    ## 2                                   13                      351
    ## 3                                   26                      259
    ## 4                                   23                      300
    ## 5                                   22                      227
    ## 6                                   14                      341
    ##   NumberofLowCostAirlines LowCostAirlinespercentage Destinations
    ## 1                      18                  28.12500          104
    ## 2                      12                  41.37931          189
    ## 3                      19                  40.42553          116
    ## 4                      18                  51.42857          160
    ## 5                       8                  72.72727           87
    ## 6                       7                  19.44445          111
    ##   Average_Route_Distance DistancetoclosestAirport
    ## 1                   1253                 23.66681
    ## 2                   1721                 63.45766
    ## 3                   3143                122.58936
    ## 4                   1701                 63.09924
    ## 5                   1582                 45.13247
    ## 6                   1460                244.49577
    ##   DistancetoclosestSimilarAirport AirportRegionalrelevance Distancetocitykm
    ## 1                       223.83824                0.8698581                6
    ## 2                        63.45766                0.5127419               15
    ## 3                       132.45082                0.7840877               19
    ## 4                       134.50558                0.8098081                9
    ## 5                        45.13247                0.1947903               55
    ## 6                       559.31000                0.9810450               10
    ##   Inhanbitantscorrected numberofvisitorscorrected GDPcorrected Cargoton
    ## 1             3551805.0                 2152829.8        26300 11223.39
    ## 2             4180133.5                 1151381.6        30100   562.00
    ## 3              705807.8                 1678968.6        20700 25994.00
    ## 4             1508358.6                 1944196.8        25000  3199.73
    ## 5             1562709.8                  181063.5        32000 28698.00
    ## 6             6626197.0                  770720.5        11200 82756.54

  - Standardization (Z-score) - (xi - xmean / standard deviation)

<!-- end list -->

``` r
  mean <- apply(df_reduced, 2, mean) # The "2" in the function is used to select the columns. MARGIN: c(1,2)
  sd <- apply(df_reduced, 2, sd)
  df_scaled <- scale(df_reduced, mean, sd)
```

## HIERARCHICAL CLUSTERING

#### Measuring Similarity through Euclidean distances

``` r
  distance <- dist(df_scaled, method = "euclidean")
  print(distance, digits = 3)
```

    ##        1     2     3     4     5     6     7     8     9    10    11    12
    ## 2   3.03                                                                  
    ## 3   3.12  3.73                                                            
    ## 4   1.93  2.62  2.19                                                      
    ## 5   4.84  3.34  4.70  4.14                                                
    ## 6   4.98  5.11  5.27  5.17  6.77                                          
    ## 7   4.08  4.49  2.76  2.96  5.41  4.38                                    
    ## 8   3.76  4.03  3.40  3.05  4.14  4.62  3.08                              
    ## 9   4.06  1.74  4.47  3.51  2.92  5.70  5.25  4.54                        
    ## 10  3.22  3.29  4.47  3.73  5.65  4.65  5.27  5.19  3.80                  
    ## 11  3.48  3.36  4.20  3.88  5.75  5.00  5.34  5.65  3.94  2.33            
    ## 12  3.84  3.42  4.34  3.88  5.95  5.09  5.47  5.56  3.78  2.22  2.69      
    ## 13  4.01  4.11  4.68  4.23  5.43  3.64  4.93  4.47  4.23  2.80  4.32  3.40
    ## 14  3.89  2.45  4.27  3.76  5.23  5.53  5.51  5.37  2.98  2.77  2.55  2.18
    ## 15  3.64  3.55  4.77  4.47  5.70  5.23  5.96  5.92  3.96  3.00  2.71  3.09
    ## 16  3.26  2.60  4.38  3.74  5.32  5.29  5.67  5.41  3.28  2.28  1.93  2.05
    ## 17  4.50  5.04  4.26  4.56  6.58  1.92  3.66  4.22  5.68  4.40  4.72  4.59
    ## 18  8.79  8.19  8.63  8.89  9.42  9.97 10.12  9.98  8.07  7.09  7.14  6.76
    ## 19  8.32  8.22  8.83  8.74  9.97  9.42 10.00 10.29  8.21  6.68  6.77  6.29
    ## 20  7.55  7.19  7.76  7.79  9.15  7.34  8.56  8.82  7.19  5.26  5.50  5.20
    ## 21  4.63  5.52  5.71  5.32  7.21  5.81  6.55  6.28  5.59  3.65  4.33  3.88
    ## 22  5.81  5.96  6.50  6.23  8.34  7.06  7.52  8.14  6.20  4.28  4.05  3.61
    ## 23  4.98  5.79  6.13  5.84  7.80  6.27  7.08  7.48  6.05  4.29  4.10  3.92
    ## 24  5.70  5.35  6.38  6.10  7.86  5.75  6.99  7.62  5.50  3.74  3.63  3.39
    ## 25  4.89  3.66  5.41  5.07  5.56  6.68  6.76  6.85  3.55  3.88  2.86  3.80
    ## 26  3.45  4.60  4.09  3.75  6.67  5.78  4.87  5.72  5.29  4.02  3.82  3.88
    ## 27  6.91  5.76  6.74  6.21  4.03  7.55  6.84  5.65  5.03  7.08  7.83  7.24
    ## 28  6.65  5.38  6.40  5.82  3.64  7.24  6.36  5.26  4.74  7.21  7.59  7.20
    ## 29  4.35  3.24  4.73  4.43  4.43  6.83  6.34  5.50  3.57  4.03  4.47  3.92
    ## 30  3.42  3.20  3.45  2.95  3.24  5.28  3.88  2.18  3.85  4.75  5.00  5.35
    ## 31  7.29  7.03  6.46  6.34  6.31  6.28  5.05  4.17  7.22  8.16  8.68  8.43
    ## 32  6.70  5.34  6.88  6.27  3.70  7.30  6.82  5.61  4.82  7.38  7.56  7.55
    ##       13    14    15    16    17    18    19    20    21    22    23    24
    ## 2                                                                         
    ## 3                                                                         
    ## 4                                                                         
    ## 5                                                                         
    ## 6                                                                         
    ## 7                                                                         
    ## 8                                                                         
    ## 9                                                                         
    ## 10                                                                        
    ## 11                                                                        
    ## 12                                                                        
    ## 13                                                                        
    ## 14  4.39                                                                  
    ## 15  4.25  3.03                                                            
    ## 16  4.09  1.49  2.13                                                      
    ## 17  3.40  5.25  4.92  5.01                                                
    ## 18  8.35  6.88  6.75  6.74  9.05                                          
    ## 19  8.06  6.99  6.06  6.56  8.66  4.12                                    
    ## 20  6.56  5.69  5.49  5.65  6.95  5.36  3.94                              
    ## 21  4.28  4.83  4.34  4.22  4.99  6.85  5.89  5.25                        
    ## 22  5.84  4.54  4.25  4.03  6.29  5.77  4.54  4.90  3.70                  
    ## 23  5.28  5.06  3.30  4.12  5.53  6.73  4.74  5.29  3.36  2.78            
    ## 24  5.04  4.01  3.38  3.63  5.41  6.30  5.20  4.41  4.15  2.45  3.22      
    ## 25  5.55  2.81  2.73  2.64  6.40  6.29  6.27  5.81  5.23  4.32  4.63  3.74
    ## 26  5.09  4.32  3.74  3.84  4.73  7.45  6.50  6.35  4.10  3.95  3.50  4.24
    ## 27  5.80  7.18  7.35  7.26  7.59 10.68 11.21 10.22  8.62  9.92  9.25  9.08
    ## 28  6.10  6.97  7.23  7.06  7.30 10.83 11.17 10.19  8.52  9.77  9.06  8.96
    ## 29  4.89  3.38  3.95  3.32  6.22  6.32  7.10  6.95  5.38  5.65  5.72  5.75
    ## 30  4.67  4.71  5.23  4.68  4.75  9.10  9.65  8.55  6.04  7.58  7.06  7.17
    ## 31  6.73  8.32  8.87  8.50  6.09 12.20 12.68 10.96  9.01 10.86 10.27 10.06
    ## 32  6.57  7.08  6.75  6.92  7.34 10.36 10.82 10.31  8.41  9.53  8.65  8.60
    ##       25    26    27    28    29    30    31
    ## 2                                           
    ## 3                                           
    ## 4                                           
    ## 5                                           
    ## 6                                           
    ## 7                                           
    ## 8                                           
    ## 9                                           
    ## 10                                          
    ## 11                                          
    ## 12                                          
    ## 13                                          
    ## 14                                          
    ## 15                                          
    ## 16                                          
    ## 17                                          
    ## 18                                          
    ## 19                                          
    ## 20                                          
    ## 21                                          
    ## 22                                          
    ## 23                                          
    ## 24                                          
    ## 25                                          
    ## 26  4.56                                    
    ## 27  7.73  8.62                              
    ## 28  7.45  8.20  1.97                        
    ## 29  4.07  5.16  6.81  6.92                  
    ## 30  5.74  5.27  5.86  5.48  4.32            
    ## 31  9.63  8.59  5.85  5.20  8.66  5.41      
    ## 32  7.01  8.02  3.75  3.04  6.50  5.15  6.13

#### Visualize distances in heatmap

``` r
  fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), order = FALSE)
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-12-1.png)<!-- -->

> **Note:** There are other forms distance measures that can be used
> such as: i) Minkowski distance; ii) Manhattan distance; iii)
> Mahanalobis distance.

**1. Complete linkage (Farthest neighbor) clustering algorithm**

Based on the maximum distance between observations in each cluster.

``` r
modelc <- hclust(distance, "complete")
plot(modelc, labels = df$Airport, xlab = "Distance - Complete linkage", hang = -1)

# Visualize the cut on the tree 
rect.hclust(modelc, 4, border = "blue") 
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-13-1.png)<!-- -->

**2. Average linkage between groups** \#\# The distance between clusters
is the average of the distances between \#\# observations in one cluster
to all the members in the other cluster.

modela \<- hclust(distance, “average”) plot(modela, labels = df$Airport,
xlab = “Distance - Average linkage”, hang = -1) rect.hclust(modelc, 4,
border = “red”)

# 3\. Ward\`s method

\#\# The measures of similarity are the sum of squares within the
cluster summed \#\# over all variables.

modelw \<- hclust(distance, “ward.D2”) plot(modelw, labels = df$Airport,
xlab = “Distance - Ward method”, hang = -1) \# Visualize where to cut on
the tree (choose number of clusters) rect.hclust(modelw, 4, border =
“orange”)

# 4\. Centroid method

\#\# The similarity between two clusters is the distance between its
centroids.

modelcen \<- hclust(distance, “centroid”) plot(modelcen, labels =
df$Airport, xlab = “Distance - Centroid method”, hang = -1)
rect.hclust(modelcen, 4, border = “green”)

# Now lets evaluate the membership of each observation with the cutree function for each method.

member\_com \<- cutree(modelc, 4) member\_av \<- cutree(modela, 4)
member\_ward \<- cutree(modelw, 4) member\_cen \<- cutree(modelcen, 4)

# Plot table to compare the how common each method is to each other.

# Lets compare the complete linkage with the average linkage

table(member\_com, member\_av)

# Silhouette Plot

## Analyzes how similiar an observation is to its own cluster compared to other clusters.

## The clustering configuration is appropriate when most objects have high values.

## Low or negative values indicate that the clustering does not have an appropriate number of clusters.

plot(silhouette(member\_com, distance)) plot(silhouette(member\_av,
distance)) plot(silhouette(member\_ward, distance))
plot(silhouette(member\_cen, distance))

# NON-HiERARCHICAL CLUSTERING

# K-means clustering

km\_clust \<- kmeans(df\_scaled, 3)

\# Print out the results km\_clust

str(km\_clust)

# Choosing K

# This algorithm will detect how many clusters from 1 to 10 explains more variance

k \<- list() for(i in 1:10){ k\[\[i\]\] \<- kmeans(df\_scaled, i) }

\#Print the k value and take a look at the ratio (between\_SS /
total\_SS) k

# Now lets try to plot (between\_SS / total\_SS) in to a scree plot

betSS\_totSS \<- list() for(i in 1:10){ betSS\_totSS\[\[i\]\] \<-
k\[\[i\]\]\(betweenss/k[[i]]\)totss }

plot(1:10, betSS\_totSS, type = “b”, ylab = “Between SS / Total SS”,
xlab = “Number of clusters”)

# Finally, try plotting each variable with each other and analyze if the clusters make sense.

# Let us go back to first example and take a look.

plot(Numberofairlines \~ Destinations, df, col = km\_clust$cluster)
with(df, text(Numberofairlines \~ Destinations, label = Airport, pos =
1, cex = 0.6))
