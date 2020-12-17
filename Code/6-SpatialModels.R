library(sf)
WIfinal = st_read("Data/Spatial/wi_final_census2_random4.shp")
class(WIfinal) #it is a spatial feature dataset
## View(WIfinal)


st_crs(WIfinal) = 4326 #set the crs to WGS84
WIfinal = st_transform(WIfinal, 3857) #project the data

summary(WIfinal$HISP_)
library(tmap) #tmap package
tm_shape(WIfinal) + 
  tm_polygons(style = "quantile", col = "HISP_", title= "Hispanic people", legend.hist = TRUE) +
  tm_legend(outside = TRUE, text.size = .8) 

tmap_mode("view") #you can choose "plot" (as above) or "view"
tm_shape(WIfinal) + 
  tm_polygons(style = "quantile", col = "HISP_", title= "Hispanic people")

library(spdep)
neighbors <- poly2nb(WIfinal, queen=TRUE) #queen
neighbors
neighbors_rook <- poly2nb(WIfinal, queen=F) #rook
neighbors_rook

weightsW = nb2listw(neighbors, style="W")
weightsW

moran.test(WIfinal$HISP_, weightsW)

#for a Monte Carlo simulation with 600 rounds
moran.mc(WIfinal$HISP_, weightsW, nsim=599)

plot(moran.mc(WIfinal$HISP_, weightsW, nsim=599), main="", las=1) #density plot

moran.plot(WIfinal$HISP_, listw = weightsW)

localmoranstats <- localmoran(WIfinal$HISP_, weightsW)
summary(localmoranstats)

moranmap <- cbind(WIfinal, localmoranstats) #first, bind the statistics to the original data
names(moranmap)[39] <- "Pvalue"	#change the name of this variable to make it easier to call it

tm_shape(moranmap) +
  tm_polygons(col = "Ii", style = "pretty", title = "local Moran's statistic") 
tm_shape(moranmap) +
  tm_polygons(
    col = "Pvalue",
    breaks = c(-Inf, 0.01, 0.05, 0.1, 0.15, Inf),
    palette = "-Blues",
    title = "local Moran's I p-values") 

#scale the variable of interest and save it to a new column
moranmap$HISP_scale <- as.vector(scale(moranmap$HISP_))
summary(moranmap$HISP_scale)
#create a spatial lag variable and save it to a new column
moranmap$HISP_lag <- lag.listw(weightsW, moranmap$HISP_scale)
summary(moranmap$HISP_lag)

library(tidyverse)
siglevel = 0.15 #we can change to different levels
moranmap <- moranmap %>%  mutate(quad_sig = ifelse(moranmap$HISP_scale > 0 & 
                              moranmap$HISP_lag > 0 & 
                              moranmap$Pvalue <= siglevel, 
                     "high-high",
                     ifelse(moranmap$HISP_scale <= 0 & 
                              moranmap$HISP_lag <= 0 & 
                              moranmap$Pvalue <= siglevel, 
                     "low-low", 
                     ifelse(moranmap$HISP_scale > 0 & 
                              moranmap$HISP_lag <= 0 & 
                              moranmap$Pvalue <= siglevel, 
                     "high-low",
                     ifelse(moranmap$HISP_scale <= 0 & 
                              moranmap$HISP_lag > 0 & 
                              moranmap$Pvalue <= siglevel,
                     "low-high", 
                     "non-significant")))))
moranmap$quad_sig = factor(moranmap$quad_sig)
table(moranmap$quad_sig)

# palcolor = c("red",rgb(1,0,0,alpha=0.4),rgb(0,0,1,alpha=0.4),"blue", "white") #define the color palette for the 5 categories (HH, HL, LH, LL, NS)
palcolor = c("red", "white") #the color palette for only 2 categories
tmap_mode("plot")
tm_shape(moranmap)+
  tm_polygons(col = "quad_sig", palette = palcolor, title = "local moran statistic")+
  tm_legend(outside = TRUE) 

# install.packages("ape")
library(ape)

## str(TABLE)
## TABLE$classfactor<-as.numeric(TABLE$CLASS) #make ordered factors as numeric
## TABLE$classfactor<-factor(TABLE$classfactor)
## TABLEmoran<-TABLE
## TABLEmoran$geometry<-NULL #drop geometry
## TABLEmoran<-na.omit(TABLEmoran) #remove cases with NA
## TABLEmoran<-TABLEmoran[TABLEmoran$Orig_Lat!=0,] #Remove cases with Lat/Lon equals to zero

## DISTANCES <- as.matrix(dist(cbind(TABLEmoran$Orig_Long, TABLEmoran$Orig_Lat)))
## DISTANCESinv <- 1/DISTANCES
## diag(DISTANCESinv) <- 0 #diagonal as zero
## DISTANCESinv[is.infinite(DISTANCESinv)] <- 0 #remove infinite distances

## #First attempt
## Moran.I(TABLEmoran$classfactor, DISTANCESinv)
## #Remove distances over 15 km
## DISTANCESbin <- (DISTANCES > 0 & DISTANCES <= 15000)
## 
## #Second attempt
## Moran.I(TABLEmoran$classfactor, DISTANCESbin) #Moran’s I =0.012, p = .001
