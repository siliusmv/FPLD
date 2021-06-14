### Initial stuff
# Start by loading the library
library(FPLD)

# ==============================================================================
# Download distance to sea data
# ==============================================================================

source(file.path(script_dir(), "download_data.R"))

# ==============================================================================
# Simulation studies
# ==============================================================================

# We want to test our method of quantiles in a simulation study
source(file.path(script_dir(), "univariate_simulation_study.R"))

# ==============================================================================
# Load, tidy and explore the actual data
# ==============================================================================

# Load all eklima-data
source(file.path(script_dir(), "prepare_data.R"))

# Examine some of the eklima-data
source(file.path(script_dir(), "explore_data.R"))

# ==============================================================================
# Perform inference
# ==============================================================================

# Do local estimation with MQ
source(file.path(script_dir(), "local_estimation.R"))
source(file.path(script_dir(), "display_local_estimation_results.R"))

# Do regional estimation with QR
source(file.path(script_dir(), "regional_estimation.R"))
source(file.path(script_dir(), "display_regional_estimation_results.R"))
