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
  modelName       = "MNL_RP",
  modelDescr      = "Simple MNL model on mode choice RP data",
  indivID         = "ID", 
  outputDirectory = "output"
)

# ============================================================================ #
# LOAD DATA
# ============================================================================ #
# Loading data from package
# if data is to be loaded from a file (e.g. called data.csv), 
database = read.csv("Data/8-DiscreteChoiceModels/swissmetro.dat",header=TRUE,sep = '\t')

# Use SP data and Choice != 0
database = subset(database, database$SP==1 & database$CHOICE!=0)
rio::export(database, "database.xlsx")


# ============================================================================ #
# DEFINE MODEL PARAMETERS
# ============================================================================ #
# Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_train  = 0,
              asc_sm     = 0,
              asc_car    = 0,
              b_tt_train = 0,
              b_tt_sm    = 0,
              b_tt_car   = 0)

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
  V[["train"]] = asc_train + b_tt_train  * TRAIN_TT
  V[["sm"]]    = asc_sm    + b_tt_sm     * SM_TT
  V[["car"]]   = asc_car   + b_tt_car    * CAR_TT
  
  # Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(train=1, sm=2, car=3 ), 
    avail         = list(train=TRAIN_AV, sm=SM_AV, car=CAR_AV), 
    choiceVar     = CHOICE,
    utilities     = V
  )
  
  # Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
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

