Panel and Spatial Regression Models
================

## Panel Regression Models

## Spatial Regression Models

### Moran’s I spatial autocorrelation

Install and load ape package

``` r
# install.packages("ape")
library(ape)
```

It does not deal with ordered factors, zeros, or infinite distances.  
So we need to clean data first.

``` r
str(TABLE)
TABLE$classfactor<-as.numeric(TABLE$CLASS) #make ordered factors as numeric
TABLE$classfactor<-factor(TABLE$classfactor)
TABLEmoran<-TABLE
TABLEmoran$geometry<-NULL #drop geometry
TABLEmoran<-na.omit(TABLEmoran) #remove cases with NA
TABLEmoran<-TABLEmoran[TABLEmoran$Orig_Lat!=0,] #Remove cases with Lat/Lon equals to zero
```

##### Distances matrix, from coordinates (Lat Long)

``` r
ozone.dists <- as.matrix(dist(cbind(TABLEmoran$Orig_Long, TABLEmoran$Orig_Lat)))
ozone.dists.inv <- 1/ozone.dists
diag(ozone.dists.inv) <- 0
ozone.dists.inv[is.infinite(ozone.dists.inv)] <- 0 #remove infinite distances
```

##### Result:

``` r
#First attempt
Moran.I(TABLEmoran$classfactor, ozone.dists.inv)
#Remove distances over 15 km
ozone.dists.bin <- (ozone.dists > 0 & ozone.dists <= 15000)

#Second attempt
Moran.I(TABLEmoran$classfactor, ozone.dists.bin) #Moran’s I =0.012, p = .001
```

> The result (observed) is the Moran’s I value, and if it is enough
> close to zero, we can affirm (with p=…) that ther is not a spatial
> pattern, suggesting an aleatory distribution in space. Tf the result
> was close to 1 or -1, it would suggest a pattern in distribuition in
> space.

**See more
[here](https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/)**
