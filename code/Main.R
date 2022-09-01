########################################################################
# Name of file - Main.R
# Data release - Stage of Treatment
# Original Author - Caroline Thomson
# Orginal Date - June 2022
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Main file for data wrangling and graph making for
#               CP publication
#
# Approximate run time - 3 minutes
#########################################################################

gc()
rm(list=ls())

#### Packages and functions ----
# Load packages ----
message("LOADING PACKAGES")
source("packages/packages.R")
message("LOADING FUNCTIONS")
source("functions/CP-functions.R")

Sys.umask(002) #Used to ensure directory permissions are correct

# Dates ----
max_date <- as.Date("2022-06-30")
max_date2 <- as.Date("2022-03-31")

#### Publication preparation ----
message("DATA WRANGLING")
source("code/scripts/CP-publication-prep.R")

#### Graphs for report ----
message("MAKING GRAPHS FOR REPORT")
source("code/scripts/make_report_graphs.R")

#### App data preparation ----
message("PREPARING DATA FOR SHINY APP")
source("code/scripts/app_data_preparation.R")

#### END OF FILE ----

