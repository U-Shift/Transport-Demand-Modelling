# ============================================================================ #
# LOAD LIBRARY                  
# ============================================================================ #


### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

# ============================================================================ #
# LOAD DATA
# ============================================================================ #

# Loading data from package
database = apollo_modeChoiceData
# for data dictionary, use ?apollo_modeChoiceData

# Use only RP data
database = subset(database,database$RP==1)


# ============================================================================ #
# EXPLORE DATA
# ============================================================================ #
# Mode choice
counts <- table(database$choice)
barplot(counts, 
        main="Mode choice",
        xlab="Choice", 
        names.arg = c("car", "bus", "air", "rail"))


# Travel times
hist(database$time_car, xlab = "Minutes", main = "Travel Time - Car")
hist(database$time_bus, xlab = "Minutes", main = "Travel Time - Bus")
hist(database$time_air, xlab = "Minutes", main = "Travel Time - Air")
hist(database$time_rail, xlab = "Minutes", main = "Travel Time - Rail")

# Travel costs
hist(database$cost_car, xlab = "GBP", main = "Travel Cost - Car")
hist(database$cost_bus, xlab = "GBP", main = "Travel Cost - Bus")
hist(database$cost_air, xlab = "GBP", main = "Travel Cost - Air")
hist(database$cost_rail, xlab = "GBP", main = "Travel Cost - Rail")

# Income
hist(database$income, xlab = "GBP per annum", main = "Individual Income")

