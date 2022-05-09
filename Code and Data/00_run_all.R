# This script runs the Data cleaning, summarizing, visualization, and estimation scripts.

# Setting up working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Defining globals
input_dir = "data"
code_dir = "code"
output_dir = "output"

source("code/01_setup.R")
source("code/02_subsetting_to_req.R")
source("code/03_assign_muncipality_canton.R")
source("code/04_data_creation.R")
source("code/05_mlogit_data_creation.R")
source("code/06_visualization.R")