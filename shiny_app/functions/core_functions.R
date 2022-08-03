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

# Move x and y axis labels so they don't overlap plot
# (Function from https://stackoverflow.com/questions/42763280/r-ggplot-and-plotly-axis-margin-wont-change)
stop_axis_title_overlap <- function(gg, x.y = -0.05, y.x = -0.1) {
  wip <- gg[['x']][['layout']][['annotations']] %>%
    tibble::enframe() %>%
    mutate(value = purrr::map(value, as_tibble)) %>%
    tidyr::unnest(cols = c(value)) %>%
    filter(!is.na(annotationType)) %>%
    mutate(
      x = case_when(x == 0.5 ~ x, TRUE ~ x.y),
      y = case_when(y == 0.5 ~ y, TRUE ~ y.x)
    ) %>%
    select(name, text, x, y) %>%
    unique()

  if (nrow(wip) == 2) {
    for (i in 1:nrow(wip)) {
      if (wip$x[i] == 0.50) {
        gg[['x']][['layout']][['annotations']][[1]][['y']] <- wip$y[i]
      } else {
        gg[['x']][['layout']][['annotations']][[2]][['x']] <- wip$x[i]
      }
    }
  } else if (wip$y == 0.5) {
    gg[['x']][['layout']][['annotations']][[1]][['x']] <- wip$x
  } else {
    gg[['x']][['layout']][['annotations']][[1]][['y']] <- wip$y
  }

  gg
}