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
# Approximate run time - xx minutes
#########################################################################

#### Packages and functions ----
# Load packages ----
source("packages/packages.R")
source("functions/CP-functions.R")

Sys.umask(002) #Used to ensure directory permissions are correct

# Dates ----
max_date <- as.Date("2022-06-30")
max_date2 <- as.Date("2022-03-31")

#### Publication preparation ----
source("code/scripts/CP-publication-prep.R")

#### Graphs for report ----
source("code/scripts/make_report_graphs.R")


#### END OF FILE ----

