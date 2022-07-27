####################### Core Functions #######################

# Load data from shiny_app/data
load_rds_file <- function(rds){
  # Given a .rds file name in shiny_app/data
  # this function loads it in to app_data list object
  # NB you must create this object first using app_data <- list()
  app_data[[gsub(".rds", "", rds)]] <<- readRDS(paste0("data/", rds))
}

# Remove warnings from icons ----
icon_no_warning_fn = function(icon_name) {
  icon(icon_name, verify_fa=FALSE)
}

# Get date string for quarter given long form
get_quarter_date <- function(longform_date){
  datestring <- case_when(longform_date == "September 2021" ~ "2021-09-30",
                          longform_date == "December 2021" ~ "2021-12-31",
                          longform_date == "March 2022" ~ "2022-03-31")
  return(datestring)
}
