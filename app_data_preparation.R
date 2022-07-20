####################### App data preparation #######################

# Data preparation for app

# This script loads data from Clinical_Prioritisation/data/processed_data
# and saves out .rds files needed for running the dashboard to the shiny_app/data folder

rm(list = ls())
gc()

# Getting project directory to choose files from
project_directory <- rstudioapi::getActiveProject()
if (!is.null(project_directory)){ setwd(project_directory) }

processed_data_folder <- "data/processed data/"
shiny_data_folder <- "shiny_app/data/"

# Load all processed data files and save out as rds

copy_to_shiny_data <- function(csv){
  # Given a .csv file name in processed_data_folder
  # copies it across to an .rds file in shiny_data_folder
  readfile <- read_csv(paste0(processed_data_folder, csv))
  saveRDS(readfile, paste0(shiny_data_folder, gsub(".csv", ".rds", csv)))
}

# Find all csv files in processed_data
processed_data_csvs <- list.files(path=processed_data_folder, pattern="*.csv")

for(csv in processed_data_csvs){
  copy_to_shiny_data(csv)
}



