# This code has been adapted from the MNL_RP example of the documentation of the "apollo" library, 
# found at: https://www.apollochoicemodelling.com/examples.html

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

### Clear memory
rm(list = ls()) #optional but recommended 

### Load Apollo library
library(apollo)
library(skimr)

# Initialise code
apollo_initialise()


# Loading data from package
database = apollo_modeChoiceData
# for data dictionary, use ?apollo_modeChoiceData

# Summary statistics
skim(database)

# Use only RP data
database = subset(database,database$RP==1)

# Exploratory Data Analysis

# Mode choice
counts <- table(database$choice)

barplot(counts, 
        main="Revealed preferences of mode choices",
        xlab="Mode choice",
        ylab="Nº respondents",
        names.arg = c("car", "bus", "air", "rail"),
        col = rainbow(4),
        ylim = c(0,350),
        las = 1)


# Travel times
hist(database$time_car, xlab = "Minutes", main = "Travel Time - Car", las = 1, ylim = c(0,350))
hist(database$time_bus, xlab = "Minutes", main = "Travel Time - Bus", las = 1, ylim = c(0,500), xlim = c(0,500))
hist(database$time_air, xlab = "Minutes", main = "Travel Time - Air", las = 1, xlim = c(0,100))
hist(database$time_rail, xlab = "Minutes", main = "Travel Time - Rail", las = 1, ylim = c(0,400), xlim = c(0,200))

# Travel costs
hist(database$cost_car, xlab = "GBP", main = "Travel Cost - Car", las = 1, ylim = c(0,250))
hist(database$cost_bus, xlab = "GBP", main = "Travel Cost - Bus", las = 1, ylim = c(0,200))
hist(database$cost_air, xlab = "GBP", main = "Travel Cost - Air", las = 1, xlim = c(0,120))
hist(database$cost_rail, xlab = "GBP", main = "Travel Cost - Rail", las = 1, ylim = c(0,200), xlim = c(0,80))

# Income
hist(database$income, xlab = "GBP per annum", main = "Individual Income", las = 1, ylim = c(0,120), xlim = c(0, 80000))

# ============================================================================ #

# MULTINOMIAL LOGIT MODEL


# Set core controls
apollo_control = list(
  modelName       = "MNL_SimpleExample",
  modelDescr      = "Multinomial Logit Model",
  indivID         = "ID", 
  outputDirectory = "output"
)

# Define model parameters                               
# Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_car   = 0,
              asc_bus   = 0,
              asc_air   = 0,
              asc_rail  = 0,
              b_tt_car  = 0,
              b_tt_bus  = 0,
              b_tt_air  = 0,
              b_tt_rail = 0,
              b_cost_car = 0, 
              b_cost_bus = 0,
              b_cost_air = 0,
              b_cost_rail = 0,
              b_income = 0)

# Vector with names (in quotes) of parameters to be kept fixed at their starting 
# value in apollo_beta, use apollo_beta_fixed = c() if none

apollo_fixed = c("asc_car")

# Group and validate inputs

apollo_inputs = apollo_validateInputs() #The function runs a number of checks and produces a consolidated list of model inputs

# "apollo_inputs" checks if all inputs required for the model are defined, it will fail otherwise.  
# (apollo_control, apollo_beta, apollo_fixed, and the database)


# Define model and likelihood function

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){

# obs:  While the other functions we have encountered are part of the package, 
# "apollo_probabilities" needs to be defined by the user as
# it is specific to the model to be estimated. 
    
  # Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)   # call individual elements by name (e.g. using female instead of database$female)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  # Create list of probabilities P
  P = list()
  
  # List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["car"]]  = asc_car  + b_tt_car  * time_car + b_cost_car * cost_car
  V[["bus"]]  = asc_bus  + b_tt_bus  * time_bus + b_cost_bus * cost_bus + b_income * income
  V[["air"]]  = asc_air  + b_tt_air  * time_air + b_cost_air * cost_air + b_income * income 
  V[["rail"]] = asc_rail + b_tt_rail * time_rail + b_cost_rail * cost_rail + b_income * income
  
  # Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(car=1, bus=2, air=3, rail=4), 
    avail         = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail), #Availability of alternative for every observation
    choiceVar     = choice,
    utilities     = V
  )
  
  # Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  #  Multiplies the probabilities across individual choice observations for the same individual
  
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  # Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# Estimate the model

model = apollo_estimate(apollo_beta,
                        apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs)


# Print Outputs
# To the screen
apollo_modelOutput(model, modelOutput_settings = list(printDataReport =T,
                                                      printFixed =T,
                                                      printPVal =1) )

# To a file
apollo_saveOutput(model)

# ============================================================================ #

# NESTED LOGIT MODEL

### Clear memory
rm(list = ls())

### Initialise code
# apollo_initialise()

### Set core controls
apollo_control_N = list(
  modelName       = "NL_SimpleModel",
  modelDescr      = "Simple NL model",
  indivID         = "ID", 
  outputDirectory = "output"
)

apollo_beta=c(asc_car   = 0,
              asc_bus   = 0,
              asc_air   = 0,
              asc_rail  = 0,
              b_tt_car  = 0,
              b_tt_bus  = 0,
              b_tt_air  = 0,
              b_tt_rail = 0,
              b_cost_car  = 0,
              b_cost_bus  = 0,
              b_cost_air  = 0,
              b_cost_rail = 0,
              b_income = 0,
              lambda_PT = 1)

# DEFINE MODEL AND LIKELIHOOD FUNCTION
# ============================================================================ #
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  # Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  # Create list of probabilities P
  P = list()
  
  # List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V = list()
  V[["car"]]  = asc_car  + b_tt_car  * time_car  + b_cost_car * cost_car
  V[["bus"]]  = asc_bus  + b_tt_bus  * time_bus  + b_cost_bus * cost_bus 
  V[["air"]]  = asc_air  + b_tt_air  * time_air  + b_cost_air * cost_air   
  V[["rail"]] = asc_rail + b_tt_rail * time_rail + b_cost_rail * cost_rail
  
  ### Specify nests for NL model
  nlNests      = list(root=1, 
                      PT=lambda_PT)
  
  # Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]]  = c("car", "PT")
  nlStructure[["PT"]] = c("bus","air", "rail")
  
  # Define settings for NL model
  nl_settings <- list(
    alternatives = c(car=1, bus=2, air=3, rail=4),
    avail        = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail),
    choiceVar    = choice,
    utilities    = V,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  # Compute probabilities using NL model
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  # Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  # Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ============================================================================ #
# ESTIMATE THE MODEL
# ============================================================================ #
model = apollo_estimate(apollo_beta,
                        apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs)

# ============================================================================ #
# OUTPUTS
# ============================================================================ #
# To the screen
apollo_modelOutput(model, modelOutput_settings = list(printDataReport =T,
                                                      printFixed =T,
                                                      printPVal =1) )

# To a file
apollo_saveOutput(model)
