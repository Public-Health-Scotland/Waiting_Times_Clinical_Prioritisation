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

# Transforms e.g. 2021-09-30 to September 2021
get_month <- function(short_date){
  long_date <- format(as.Date(short_date, format="%Y-%d-%m"), "%B %Y")
  return(long_date)
}

# Transforms e.g. September 2021 to 2021-09-30
get_short_date <- function(monthyear){
  short_date <- rollforward(as.Date(paste0("01 ", monthyear), "%d %B %Y"))
  return(short_date)
}

# Transform weeks waiting labels from e.g. 000-004 to "0-4"
get_pretty_weeks <- function(ugly_weeks){
  pretty_weeks <- gsub("\\b0+\\B", "", ugly_weeks)
  return(pretty_weeks)
}