Cluster Analysis
================

## Example Airports

> Your task: Create and evaluate the many types of clustering methods.

### Variables:

`Code`: Code of the airport;

`Airport`: Name of the airport;

`Ordem`: ID of the observations;

`Passengers`: Number of passengers;

`Movements`: Number of flights;

`Numberofairlines`: Number of airlines in each airport;

`Mainairlineflightspercentage`: Percentage of flights of the main
airline of each airport;

`Maximumpercentageoftrafficpercountry`: Maximum percentage of flights
per country;

`NumberofLCCflightsweekly`: Number of weekly low cost flights\`;

`NumberofLowCostAirlines`: Number of low cost airlines of each airport;

`LowCostAirlinespercentage`: Percentage of the number of low cost
airlines in each airport;

`Destinations`: Number of flights arriving at each airport;

`Average_route_Distance`: Average route distance in km;

`DistancetoclosestAirport`: Distance to closest airport in km

`DistancetoclosestSimilarAirport`: Distance to closest similar airport
in km;

`AirportRegionalRelevance`: Relevance of the airport in a regional scale
(0 - 1);

`Distancetocitykm`: Distance between the airport and the city in km;

`Inhabitantscorrected`: Population of the city;

`numberofvisitorscorrected`: Number of vistors that arrived in the
airport;

`GDP corrected`: Corrected value of the Gross Domestic Product;

`Cargoton`: Cargo ton. The total number of cargo transported in a
certain period multiplied by the number o flights.

#### Import Libraries

``` r
library(readxl) # Library used for reading excel files
library(skimr) # Library used for summary statistics
library(tidyverse) # Library used in data science to perform exploratory data analysis
library(mclust) # Library used for model based clustering
library(cluster) # Library used for cluster analysis
library(factoextra) # Library used for visualizing distances
```

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
> the other variables in the analysis.

#### Treat the data before performing a cluster analysis

  - In this example we do not have missing values. In case you do have
    in the future, you can take out the missing values with listwise
    deletion.

<!-- end list -->

``` r
 df <- na.omit(df)
```

> **Note**: The listwise deletion removes the whole observation that has
> a missing value from the analysis. This may be appropriate only in
> some cases. There are many other forms of treating missing values.

  - Leave only continuous variables, and take out “Ordem”

<!-- end list -->

``` r
  drop <- c("Code","Airport", "Ordem")
  df_reduced = df[,!(names(df) %in% drop)]
```

> **Note:** We took out “Ordem” because it is an ID variable, and
> therefore it does not give value to the analysis.

#### Take a look at the scale of the variables. See how they are different\!

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

#### Z-score standardization - (xi - xmean / standard deviation)

``` r
  mean <- apply(df_reduced, 2, mean) # The "2" in the function is used to select the columns. MARGIN: c(1,2)
  sd <- apply(df_reduced, 2, sd)
  df_scaled <- scale(df_reduced, mean, sd)
```

### HIERARCHICAL CLUSTERING

#### Measuring Similarity through Euclidean distances

``` r
  distance <- dist(df_scaled, method = "euclidean")
```

> **Note:** There are other forms distance measures that can be used
> such as: i) Minkowski distance; ii) Manhattan distance; iii)
> Mahanalobis distance.

#### Visualize distances in heatmap

``` r
  fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), order = FALSE)
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-12-1.png)<!-- -->

#### Let us execute the many types of hierarchical clustering

**1. Single linkage (nearest neighbor) clustering algorithm**

Used a bottom-up approach, by linking two clusters that have the closest
distance between each other.

``` r
models <- hclust(distance, "single")
plot(models, labels = df$Airport, xlab = "Distance - Single linkage", hang = -1)

# Visualize the cut on the tree 
rect.hclust(models, 4, border = "purple") 
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-13-1.png)<!-- -->

**2. Complete linkage (Farthest neighbor) clustering algorithm**

Based on the maximum distance between observations in each cluster.

``` r
modelc <- hclust(distance, "complete")
plot(modelc, labels = df$Airport, xlab = "Distance - Complete linkage", hang = -1)

# Visualize the cut on the tree 
rect.hclust(modelc, 4, border = "blue") 
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-14-1.png)<!-- -->

**3. Average linkage between groups**

The distance between clusters is the average of the distances between
observations in one cluster to all the members in the other cluster.

``` r
modela <- hclust(distance, "average")
plot(modela, labels = df$Airport, xlab = "Distance - Average linkage", hang = -1)
rect.hclust(modelc, 4, border = "red")
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-15-1.png)<!-- -->

**4. Ward\`s method**

The measures of similarity are the sum of squares within the cluster
summed over all variables.

``` r
modelw <- hclust(distance, "ward.D2")
plot(modelw, labels = df$Airport, xlab = "Distance - Ward method", hang = -1)
# Visualize where to cut on the tree (choose number of clusters)
rect.hclust(modelw, 4, border = "orange")
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-16-1.png)<!-- -->

**5. Centroid method**

The similarity between two clusters is the distance between its
centroids.

``` r
modelcen <- hclust(distance, "centroid")
plot(modelcen, labels = df$Airport, xlab = "Distance - Centroid method", hang = -1)
rect.hclust(modelcen, 4, border = "green")
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-17-1.png)<!-- -->

#### Now lets evaluate the membership of each observation with the cutree function for each method.

``` r
member_single <- cutree(models, 4)
member_com <- cutree(modelc, 4)
member_av <- cutree(modela, 4)
member_ward <- cutree(modelw, 4)
member_cen <- cutree(modelcen, 4)
```

#### Plot table to compare how common each method is to each other.

Let us compare the complete linkage with the average linkage

``` r
table(member_com, member_av)
```

    ##           member_av
    ## member_com  1  2  3  4
    ##          1 14  0  0  0
    ##          2 11  0  0  0
    ##          3  0  3  0  0
    ##          4  0  0  3  1

> **Note:** Try comparing other methods, and evaluate how common they
> are.

#### Silhouette Plot

``` r
plot(silhouette(member_single, distance))
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-20-1.png)<!-- -->

``` r
plot(silhouette(member_com, distance))
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-20-2.png)<!-- -->

``` r
plot(silhouette(member_av, distance))
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-20-3.png)<!-- -->

``` r
plot(silhouette(member_ward, distance))
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-20-4.png)<!-- -->

``` r
plot(silhouette(member_cen, distance))
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-20-5.png)<!-- -->

> **Note:** The silhouette plot evaluates how similiar an observation is
> to its own cluster compared to other clusters. The clustering
> configuration is appropriate when most objects have high values. Low
> or negative values indicate that the clustering method is not
> appropriate or the number of clusters is not ideal.

### NON-HiERARCHICAL CLUSTERING

#### K-means clustering

``` r
km_clust <- kmeans(df_scaled, 3)
```

Print out the results

``` r
km_clust 
```

    ## K-means clustering with 3 clusters of sizes 7, 18, 7
    ## 
    ## Cluster means:
    ##   Passengers  Movements Numberofairlines Mainairlineflightspercentage
    ## 1  1.5805700  1.5672811      1.252280245                   -0.4493699
    ## 2 -0.2469314 -0.2011272     -0.004982515                   -0.3699491
    ## 3 -0.9456035 -1.0500969     -1.239468064                    1.4006677
    ##   Maximumpercentageoftrafficpercountry NumberofLCCflightsweekly
    ## 1                           -0.5723298               1.09794424
    ## 2                           -0.1629508               0.04370786
    ## 3                            0.9913460              -1.21033588
    ##   NumberofLowCostAirlines LowCostAirlinespercentage Destinations
    ## 1               0.3787531                -0.7808392   1.12432250
    ## 2               0.3305562                -0.2689855   0.07609389
    ## 3              -1.2287547                 1.4725164  -1.31999250
    ##   Average_Route_Distance DistancetoclosestAirport
    ## 1              1.1638144              -0.37683045
    ## 2             -0.1062034               0.06631895
    ## 3             -0.8907198               0.20629602
    ##   DistancetoclosestSimilarAirport AirportRegionalrelevance Distancetocitykm
    ## 1                      0.87493166                0.5998030       -0.2060176
    ## 2                     -0.07737412                0.1407640       -0.3136475
    ## 3                     -0.67596963               -0.9617675        1.0125397
    ##   Inhanbitantscorrected numberofvisitorscorrected GDPcorrected   Cargoton
    ## 1            1.09082987                 1.3272381    0.8042615  1.2446104
    ## 2           -0.06258292                -0.2290652   -0.1117858 -0.3186842
    ## 3           -0.92990235                -0.7382133   -0.5168124 -0.4251367
    ## 
    ## Clustering vector:
    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 
    ##  2  2  2  2  3  2  2  3  2  2  2  2  2  2  2  2  2  1  1  1  1  1  1  1  2  2 
    ## 27 28 29 30 31 32 
    ##  3  3  2  3  3  3 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1]  70.90186 150.32291  67.16167
    ##  (between_SS / total_SS =  48.3 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
str(km_clust)
```

    ## List of 9
    ##  $ cluster     : Named int [1:32] 2 2 2 2 3 2 2 3 2 2 ...
    ##   ..- attr(*, "names")= chr [1:32] "1" "2" "3" "4" ...
    ##  $ centers     : num [1:3, 1:18] 1.581 -0.247 -0.946 1.567 -0.201 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:3] "1" "2" "3"
    ##   .. ..$ : chr [1:18] "Passengers" "Movements" "Numberofairlines" "Mainairlineflightspercentage" ...
    ##  $ totss       : num 558
    ##  $ withinss    : num [1:3] 70.9 150.3 67.2
    ##  $ tot.withinss: num 288
    ##  $ betweenss   : num 270
    ##  $ size        : int [1:3] 7 18 7
    ##  $ iter        : int 2
    ##  $ ifault      : int 0
    ##  - attr(*, "class")= chr "kmeans"

#### Choosing K

#### This algorithm will detect how many clusters from 1 to 10 explains more variance

``` r
  k <- list()
  for(i in 1:10){
    k[[i]] <- kmeans(df_scaled, i)
  }
```

> **Note**: Try printing the k value and take a look at the ratio
> “between\_SS / total\_SS”. Evaluate how it varies when you add
> clusters.

#### Now lets try to plot (between\_SS / total\_SS) in to a scree plot

``` r
betSS_totSS <- list()
for(i in 1:10){
betSS_totSS[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}
plot(1:10, betSS_totSS, type = "b", ylab = "Between SS / Total SS", xlab = "Number of clusters")
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-24-1.png)<!-- -->

#### Let us try to take out the outliers and see the diference in the k-means clustering

  - Examine the boxplots

<!-- end list -->

``` r
par(mar=c(15,2,1,1)) # Make labels fit in the boxplot
boxplot(df_scaled, las = 2)
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-25-1.png)<!-- -->

  - **Detect the outliers**

<!-- end list -->

``` r
outliers <- boxplot.stats(df_scaled)$out

outliers
```

    ##  [1] 2.630622 2.772024 2.772024 3.611626 2.916185 2.523102 2.732030 2.515406
    ##  [9] 3.308457 3.285459

  - **Remove rows with outliers**

<!-- end list -->

``` r
df_no_outliers <- which(df_scaled %in% outliers,) 
```

> **Note:** There are many methods to treat outliers. This is just one
> of them. Not an ideal one, since it removes many observations that are
> relevant for the analysis, since the dataset is small.

#### Execute a k-means clustering with the dataset without the outliers and see the diference.

``` r
km_no_outliers <- kmeans(df_no_outliers, 3)

km_no_outliers
```

    ## K-means clustering with 3 clusters of sizes 4, 3, 3
    ## 
    ## Cluster means:
    ##        [,1]
    ## 1 539.00000
    ## 2  89.66667
    ## 3 397.66667
    ## 
    ## Clustering vector:
    ##  [1] 2 2 2 3 3 3 1 1 1 1
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1]  2882.000  7708.667 12604.667
    ##  (between_SS / total_SS =  93.8 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

#### Finally, try plotting each variable with each other and analyze if the clusters make sense.

Let us go back to first example and take a look.

  - **K-means with outliers**

<!-- end list -->

``` r
plot(Numberofairlines ~ Destinations, df, col = km_clust$cluster)
with(df, text(Numberofairlines ~ Destinations, label = Airport, pos = 1, cex = 0.6))
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-29-1.png)<!-- -->

  - **K-means without outliers**

<!-- end list -->

``` r
plot(Numberofairlines ~ Destinations, df, col = km_no_outliers$cluster)
with(df, text(Numberofairlines ~ Destinations, label = Airport, pos = 1, cex = 0.6))
```

![](README_files/3-ClusterAnalysis/unnamed-chunk-30-1.png)<!-- -->
