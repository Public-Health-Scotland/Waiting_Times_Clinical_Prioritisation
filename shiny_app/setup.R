####################### Setup #######################
if(!require('pacman')){
  install.packages('pacman')
} #loads pacman or installs and then loads it if necessary


p_load(shiny, shinycssloaders, shinyjs, shinyWidgets, shinyBS, shinymanager,
       tidyr, dplyr, magrittr, lubridate, glue, ggplot2, plotly, sparkline,
       phsstyles, htmlwidgets)

# # Shiny packages
# library(shiny)
# library(shinycssloaders)
# library(shinyjs)
# library(shinyWidgets)
# library(shinyBS)
# library(shinymanager)
# 
# # Data wrangling packages
# library(tidyr)
# library(dplyr)
# library(magrittr)
# library(lubridate)
# library(glue)
# 
# # Plotting packages
# library(ggplot2)
# library(plotly)
# library(sparkline)
# 
# # PHS styling packages
# library(phsstyles)
# 
# #html dpendencies
# library(htmlwidgets)


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

hb_ordered <- c("NHSScotland",
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

quarter_end_dates <- c("June 2022", "March 2022", "December 2021", "September 2021")




