########################################################################
# Name of file - admin_functions.R
# Data release - Stage of Treatment
# Original Authors - Maiana Sanjuan
# Orginal Date - July 2022
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Functions used by developpers to manage data and admin
#
# Approximate run time - NA
#########################################################################


# 1 Get a copy of data from a colleague's project folder -----------------------------------------------------------------------
#If a data folder has not been created, it will create one for you at the root of your project directory.
#source_project: name of the project folder to copy from (Chr)
#replace_file: should existing files with the same name be overwritten (TRUE/FALSE)
#eg. get_data("R Code", replace_file = TRUE)

get_data <- function(source_project, replace_file = FALSE){
  
  source <- str_c(str_remove(here::here(), basename(here::here())), source_project, "/data/")
  
  list_of_files <- list.files(source) 
  
  if(file.exists("data")==FALSE){
    dir.create(file.path(here::here(), "data"))
  }
  
  destination <-  here::here("data")
  
  file.copy(file.path(source, list_of_files), destination, overwrite = replace_file)
  
}


