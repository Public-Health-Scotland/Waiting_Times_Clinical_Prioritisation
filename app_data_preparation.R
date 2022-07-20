####################### App data preparation #######################

# Data preparation for app

# This script loads data from Clinical_Prioritisation/data/processed_data
# and saves out .rds files needed for running the dashboard to the shiny_app/data folder

rm(list = ls())
gc()

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.

# Getting project directory to choose files from
project_directory <- rstudioapi::getActiveProject()
if (!is.null(project_directory)){ setwd(project_directory) }

processed_data_folder <- "data/processed data/"
shiny_data_folder <- "shiny_app/data/"


