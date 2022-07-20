####################### Setup #######################

# Shiny packages
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)

# Data wrangling packages
library(dplyr)
library(magrittr)
library(lubridate)

# Plotting packages
library(ggplot2)
library(plotly)

# PHS styling packages
library(phsstyles)


# Load packages and functions, and data wrangling
source("functions/core_functions.R")

# Initialise list to store all app data
app_data <- list()

# Load in data to app_data
# Find all rds files in shiny_app/data
rds_files <- list.files(path="data/", pattern="*.rds")

for (rds in rds_files){
  load_rds_file(rds)
}

