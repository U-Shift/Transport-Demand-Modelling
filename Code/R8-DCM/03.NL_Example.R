# ============================================================================ #
# LOAD LIBRARY                  
# ============================================================================ #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "NL_SimpleModel",
  modelDescr      = "Simple NL model",
  indivID         = "ID", 
  outputDirectory = "output"
)

# ============================================================================ #
# LOAD DATA
# ============================================================================ #
# Loading data from package
# if data is to be loaded from a file (e.g. called data.csv), 
database = apollo_modeChoiceData
# for data dictionary, use ?apollo_modeChoiceData

# Use only RP data
database = subset(database,database$RP==1)


# ============================================================================ #
# DEFINE MODEL PARAMETERS
# ============================================================================ #
# Vector of parameters, including any that are kept fixed in estimation
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
              lambda_PT = 1)

# Vector with names (in quotes) of parameters to be kept fixed at their starting 
#value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car")

# ============================================================================ #
# GROUP AND VALIDATE INPUTS
# ============================================================================ #
apollo_inputs = apollo_validateInputs()

# ============================================================================ #
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

