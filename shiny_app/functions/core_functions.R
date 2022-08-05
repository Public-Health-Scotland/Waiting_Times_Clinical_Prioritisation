####################### Core Functions #######################

# Load data from shiny_app/data
load_rds_file <- function(rds){
  # Given a .rds file name in shiny_app/data
  # this function loads it in to app_data list object
  # NB you must create this object first using app_data <- list()
  app_data[[gsub(".rds", "", rds)]] <<- readRDS(paste0("data/", rds))
}

# Add n linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}

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

# Move x and y axis labels of ggplotly so they don't overlap plot
# (Function from https://stackoverflow.com/questions/42763280/r-ggplot-and-plotly-axis-margin-wont-change)
stop_axis_title_overlap <- function(gg, x.y = -0.05, y.x = -0.09) {
  ann <- gg[['x']][['layout']][['annotations']] %>%
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

  if (nrow(ann) == 2) {
    for (i in 1:nrow(ann)) {
      if (ann$x[i] == 0.50) {
        gg[['x']][['layout']][['annotations']][[1]][['y']] <- ann$y[i]
      } else {
        gg[['x']][['layout']][['annotations']][[2]][['x']] <- ann$x[i]
      }
    }
  } else if (ann$y == 0.5) {
    gg[['x']][['layout']][['annotations']][[1]][['x']] <- ann$x
  } else {
    gg[['x']][['layout']][['annotations']][[1]][['y']] <- ann$y
  }

  gg
}


# Function to format a given entry in a table
format_entry <- function(x, dp=0, perc=F){
  # x (numeric, char): entry
  # dp (int): number of decimal places
  # perc (bool): whether to add % afterwards

  # First strip any existing commas and whitespace out
  x <- gsub(",", "", x)
  x <- gsub(" ", "", x)

  # Try to convert entry to numeric, if failed return NULL
  numx <- tryCatch(as.numeric(x),
                   warning = function(w) NULL)

  # Format entry if numeric
  if (!is.null(numx)){
    numx <- formatC(numx, format="f", big.mark = ",", digits=dp)
    if (perc){
      numx <- paste0(numx, "%")
    }
    return (numx)
  } else {
    # If entry cannot be converted to numeric, return original entry i.e. "*"
    return(x)
  }
}

# Generic data table
make_table <- function(input_data_table,
                       add_separator_cols = NULL,
                       rows_to_display = 20,
                       scrollX = FALSE,
                       scrollY = FALSE
){

  # Add column formatting
  for (i in add_separator_cols){
    input_data_table[i] <- apply(input_data_table[i], MARGIN=1, FUN=format_entry)
  }

  dt <- DT::datatable(input_data_table, style = 'bootstrap',
                      class = 'table-bordered table-condensed',
                      rownames = FALSE,
                      filter="top",
                      options = list(pageLength = rows_to_display,
                                     scrollX = scrollX,
                                     scrollY = scrollY,
                                     dom = 'tip',
                                     autoWidth = TRUE,
                                     # style header
                                     initComplete = htmlwidgets::JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "$(this.api().table().row().index()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "}")))


  return(dt)


}

# Mini data table for summary stats
info_table <- function(input_data_table,
                       add_separator_cols = NULL
){

  # Add column formatting
  for (i in add_separator_cols){
    input_data_table[i] <- apply(input_data_table[i], MARGIN=1, FUN=format_entry)
  }

  dt <- DT::datatable(input_data_table, style = 'bootstrap',
                      class = 'table-bordered table-condensed',
                      rownames = FALSE,
                      options = list(paging=FALSE,
                                     searching=FALSE,
                                     info=FALSE,
                                     dom = 't',
                                     autoWidth = TRUE,
                                     # style header
                                     initComplete = htmlwidgets::JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "$(this.api().table().row().index()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "}")))


  return(dt)


}