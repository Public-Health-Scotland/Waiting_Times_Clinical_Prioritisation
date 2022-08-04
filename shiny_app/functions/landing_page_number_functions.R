####################### Landing Page Number Functions #######################

## Activity

activity_table <- function(input_data, hbt="NHS Scotland", timescale="monthly") {


    if (timescale == "monthly"){
      dataset <- input_data$monthly %>%
        dplyr::rename("Month ending" = "date",
                      "Waiting status" = "indicator",
                      "Clinical Prioritisation" = "urgency",
                      "Count" = "number",
                      "Monthly average" = "monthly_avg")
      cols_to_keep <- c("Month ending","Waiting status", "Clinical Prioritisation",
                        "Count", "Monthly average")
    } else {
      dataset <- input_data$quarterly %>%
        dplyr::rename("Quarter ending" = "date",
                      "Waiting status" = "indicator",
                      "Clinical Prioritisation" = "urgency",
                      "Count" = "number",
                      "Quarterly average" = "quarterly_avg")
      cols_to_keep <- c("Quarter ending","Waiting status", "Clinical Prioritisation",
                        "Count", "Quarterly average")

    }

    dataset %<>%
      filter(nhs_board_of_treatment == hbt) %>%
      mutate(urgency = factor(`Clinical Prioritisation`, levels=c("P1A-1B", "P2", "P3", "P4", "Other")),
             indicator = case_when(`Waiting status` == "Ongoing" ~ "Waiting",
                                   `Waiting status` == "Completed" ~ "Admitted",
                                   `Waiting status` == "additions_to_list" ~ "Additions to list")) %>%
      select(cols_to_keep) %>%
      unique()


    return(dataset)

}


# ----------------------------------------------------------------------
## Waits

waits_table <- function(input_data,
                        timescale="monthly",
                        time_chunk_end="March 2022",
                        chosen_specialty="All Specialties",
                        hbt="NHS Scotland") {

  dataset <- input_data[[timescale]] %>%
    filter(date == get_short_date(time_chunk_end),
           specialty == chosen_specialty,
           nhs_board_of_treatment == hbt) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")),
           weeks = get_pretty_weeks(weeks)) %>%
    mutate(weeks=factor(weeks, levels=get_pretty_weeks(unique(input_data[[timescale]]$weeks)))) %>%
    select(date, ongoing_completed, specialty, nhs_board_of_treatment, urgency, weeks, `number_seen/on_list`) %>%
    unique()

  if (timescale == "monthly"){
    dataset %<>% dplyr::rename("Month ending" = "date",
                               "Waiting status" = "ongoing_completed",
                               "Specialty" = "specialty",
                               "Health Board of Treatment" = "nhs_board_of_treatment",
                               "Clinical Prioritisation" = "urgency",
                               "Weeks waiting" = "weeks",
                               "Count" = "number_seen/on_list")
  } else if (timescale == "quarterly") {
    dataset %<>% dplyr::rename("Quarter ending" = "date",
                               "Waiting status" = "ongoing_completed",
                               "Specialty" = "specialty",
                               "Health Board of Treatment" = "nhs_board_of_treatment",
                               "Clinical Prioritisation" = "urgency",
                               "Weeks waiting" = "weeks",
                               "Count" = "number_seen/on_list")
  }

  return(dataset)

}

median_byurgency_table <- function(input_data,
                             timescale="monthly",
                             time_chunk_end="March 2022",
                             chosen_specialty="All Specialties",
                             hbt="NHS Scotland"){

  dataset <- input_data[[timescale]] %>%
    # if status is ongoing cannot yet compute stats
    filter(ongoing_completed == "Completed",
           date == get_short_date(time_chunk_end),
           specialty == chosen_specialty,
           nhs_board_of_treatment == hbt) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other"))) %>%
    select(urgency, median, `90th_percentile`) %>%
    unique() %>%
    dplyr::rename("Urgency" = "urgency",
                  "Median" = "median",
                  "90th percentile" = "90th_percentile")

  return(dataset)



}


