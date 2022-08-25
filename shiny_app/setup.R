####################### Setup #######################

# Shiny packages
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(shinymanager)

# Data wrangling packages
library(tidyr)
library(dplyr)
library(magrittr)
library(lubridate)
library(glue)

# Plotting packages
library(ggplot2)
library(plotly)
library(sparkline)

# PHS styling packages
library(phsstyles)

#html dpendencies
library(htmlwidgets)


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

## Plot Parameters ---------------------------------------------------------

# Style of x and y axis
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14),
                    showline = TRUE, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
                    tickfont = list(size=14), titlefont = list(size=14))

waiting_times_palette <- phs_colours(c("phs-green","phs-purple", "phs-blue", "phs-magenta", "phs-graphite"))

# Buttons to remove from plotly plots
bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                     'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                     'hoverClosestCartesian')

# Filter options ---------------------------------------------------------

hb_ordered <- c("NHS Scotland",
                "NHS Ayrshire & Arran",
                "NHS Borders",
                "NHS Dumfries & Galloway",
                "NHS Fife",
                "NHS Forth Valley",
                "NHS Grampian",
                "NHS Greater Glasgow & Clyde",
                "NHS Highland",
                "NHS Lanarkshire",
                "NHS Lothian",
                "NHS Orkney",
                "NHS Shetland",
                "NHS Tayside",
                "NHS Western Isles",
                "Golden Jubilee National Hospital")

hb_most_waiting <- c("NHS Greater Glasgow & Clyde",
                     "NHS Lothian",
                     "NHS Grampian",
                     "NHS Lanarkshire",
                     "NHS Tayside",
                     "NHS Ayrshire & Arran")

# Creating list of reactiveValues to store the plots and numbers in so that they can be accessed when needed.
# Note that these CANNOT be stored as output$ instead, because they are used in a uiOutput in ui.R
# And it is bad practice to call output$ objects from within another output$. In this case it leads
# to mysterious rendering errors due to breaking the reactive dependency tree.
plots <- reactiveValues()
numbers <- reactiveValues()



