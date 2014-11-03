# Import functions
source('./code/helperFunctions.R')
source('./code/likelihoodFunctions.R')

# Check for all the model inputs and set defaults if missing
printTitle('CHECKING MODEL INPUTS')
source('./code/checkModelInputs.R')
cat('\n...ALL MODEL INPUTS ARE GOOD!\n')

# Create the data list object for storing all variables and data
source('./code/createDataObject.R')

# Set the log likelihood and gradient functions
source(file='./code/setLikelihoodFunctions.R')

# Setup all variables (scales them too if chosen)
source(file='./code/setupVariables.R')

# Create starting points for the optimization loop
source('./code/makeStartPoints.R')

# Run the optimizor
if (runOptimizor == TRUE) {source('./code/optimizor.R')}

# Save all the run info
if (saveResults == TRUE) {source('./code/saveResults.R')}
