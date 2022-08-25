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
get_month <- function(short_date, format = "%B %Y"){
  long_date <- format(as.Date(short_date, format="%Y-%d-%m"), format)
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

replace_colnames <- function(old_colnames){

  # Convert old colname to new colname
  # Use like names(df) <- replace_colnames(names(df))

  # Named list, LHS is existing col names, RHS is what we want to rename
  possible_colnames <- list("urgency" = "Clinical_Prioritisation",
                            "date" = "Date",
                            "indicator" = "Waiting_status",
                            "nhs_board_of_treatment" = "Health_Board_of_Treatment",
                            "specialty" = "Specialty",
                            "patient_type" = "Patient_type",
                            "number" = "Count",
                            "waited_waiting_over_26_weeks" = "Waited_or_waiting_over_26_weeks",
                            "waited_waiting_over_52_weeks" = "Waited_or_waiting_over_52_weeks",
                            "waited_waiting_over_104_weeks" = "Waited_or_waiting_over_104_weeks",
                            "monthly_avg" = "2019_monthly_average",
                            "y_max" = "y_max",
                            "y_max2" = "y_max2",
                            "ongoing_completed" = "Waiting_status",
                            "weeks" = "Weeks_waiting",
                            "number_seen/on_list" = "Count",
                            "total" = "Total",
                            "p2_proportion" = "Proportion_which_is_P2",
                            "proportion" = "Proportion",
                            "median" = "Median_waiting_time",
                            "90th_percentile" = "90th_percentile_waiting_time"
                            )

  # If the column is in the list names, replace it with the list value
  replace_fn <- function(x){
    if(x %in% names(possible_colnames)){
      return(possible_colnames[[x]])
    } else {
      return(x)
    }

  }

  new_colnames <- purrr::map_chr(old_colnames, ~replace_fn(x=.x))

  return(new_colnames)

}

cols_to_not_display <- c("y_max", "y_max2", "waited_waiting_over_26_weeks",
                            "waited_waiting_over_52_weeks", "waited_waiting_over_104_weeks",
                            "monthly_avg", "proportion"
)

recode_indicator <- function(ind){
  new_ind <- case_when(ind == "Ongoing" ~ "Waiting",
                       ind == "Completed" ~ "Admitted",
                       ind == "additions_to_list" ~ "Additions")

  return(new_ind)
}

make_cols_factors <- function(df, cols){

  df %<>% mutate_at(cols, as.factor)

  return(df)
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

# Wrap labels for plotly graphs
wrap_label <- function(unwrapped_label, width=16){
  wrapped_label <- paste(strwrap(unwrapped_label, width=width), collapse = "<br>")
  return (wrapped_label)
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

  # Take out underscores in column names for display purposes
  table_colnames  <-  gsub("_", " ", colnames(input_data_table))

  # Add column formatting
  for (i in add_separator_cols){
    input_data_table[i] <- apply(input_data_table[i], MARGIN=1, FUN=format_entry)
  }

  dt <- DT::datatable(input_data_table, style = 'bootstrap',
                      class = 'table-bordered table-condensed',
                      rownames = FALSE,
                      filter="top",
                      colnames = table_colnames,
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
                                     scrollX=T, #seems to make table resize to window better
                                     autoWidth = TRUE,
                                     # style header
                                     initComplete = htmlwidgets::JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C5C3DA', 'color': '#3F3685', 'justify-content': 'left', 'align-items': 'left'});",
                                       "$(this.api().table().row().index()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "}"),
                                     columnDefs = list(list(className = 'dt-center', targets = '_all'))
                                     )
                      )#datable


  return(dt)


}

#mini data table for summary stats with sparkline

sparkline_table <- function(input_data_table,
                       add_separator_cols = NULL
){

  # Add column formatting
  for (i in add_separator_cols){
    input_data_table[i] <- apply(input_data_table[i], MARGIN=1, FUN=format_entry)
  }

  dt <- DT::datatable(input_data_table, style = 'bootstrap',
                      class = 'table-bordered table-condensed',
                      rownames = FALSE,
                      escape = FALSE,
                      options = list(paging=FALSE,
                                     searching=FALSE,
                                     info=FALSE,
                                     dom = 't',
                                     autoWidth = TRUE,
                                     fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}'),                                     # style header
                                     initComplete = htmlwidgets::JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#C5C3DA', 'color': '#3F3685', 'justify-content': 'left', 'align-items': 'left'});",
                                       "$(this.api().table().row().index()).css({'background-color': '#C5C3DA', 'color': '#3F3685'});",
                                       "}"),
                                     columnDefs = list(list(className = 'dt-center', targets = '_all'))
                      )
  ) %>% #datatable
    spk_add_deps()


  return(dt)


}