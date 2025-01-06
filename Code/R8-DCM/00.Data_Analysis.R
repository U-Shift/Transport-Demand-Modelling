# This code has been adapted from the MNL_RP example of the documentation of the "apollo" library, 
# found at: https://www.apollochoicemodelling.com/examples.html

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(skimr)

# We are going to use the example dataset from Apollo. 
# The dataset is to be used for discrete choice modelling. 
# Data comes from 500 individuals, each with two revealed preferences (RP) observation, 
# and 14 stated stated (SC) observations. There are 8,000 choices in total. Data is simulated. 
# Each observation contains attributes for the alternatives, 
# availability of alternatives, and characteristics of the individuals.

# Variables

# ID: Identification number of the individual.
# RP: 1 if the row corresponds to a revealed preference (RP) observation. 0 otherwise.
# RP_journey: Consecutive ID of RP observations. 0 if SP observation.
# SP: 1 if the row corresponds to a stated preference (SP) observation. 0 otherwise.
# SP_task: Consecutive ID of SP choice tasks. 0 if RP observation.
# access_air: Access time (in minutes) of mode air.
# access_bus: Access time (in minutes) of mode bus.
# access_rail: Access time (in minutes) of mode rail.
# av_air: 1 if the mode air (plane) is available. 0 otherwise.
# av_bus: 1 if the mode bus is available. 0 otherwise.
# av_car: 1 if the mode car is available. 0 otherwise.
# av_rail: 1 if the mode rail (train) is available. 0 otherwise.
# business: Purpose of the trip. 1 for business, 0 for other.
# choice: Choice indicator, 1=car, 2=bus, 3=air, 4=rail.
# cost_air: Cost (in GBP) of mode air.
# cost_bus: Cost (in GBP) of mode bus.
# cost_car: Cost (in GBP) of mode car.
# cost_rail: Cost (in GBP) of mode rail.
# female: Sex of individual. 1 for female, 0 for male.
# income: Income (in GBP per annum) of the individual.
# service_air: Additional services for the air alternative. 1 for no-frills, 2 for wifi, 3 for food. This is not used in the RP data, where it is set to 0.
# service_rail: Additional services for the rail alternative. 1 for no-frills, 2 for wifi, 3 for food. This is not used in the RP data, where it is set to 0.
# time_air: Travel time (in minutes) of mode air.
# time_bus: Travel time (in minutes) of mode bus.
# time_car: Travel time (in minutes) of mode car.
# time_rail: Travel time (in minutes) of mode rail.

# Loading data from package
database = apollo_modeChoiceData
# for data dictionary, use ?apollo_modeChoiceData

# Summary statistics
skim(database)

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
        names.arg = c("car", "bus", "air", "rail"),
        col = rainbow(4))


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

