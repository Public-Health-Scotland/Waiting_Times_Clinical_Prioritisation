####################### Landing Page Number Functions #######################

## Activity

activity_table <- function(input_data, hbt="NHS Scotland", timescale="monthly") {


    if (timescale == "monthly"){
      dataset <- input_data$monthly
      cols_to_keep <- c("Date","Waiting status", "Clinical Prioritisation",
                        "Count", "Monthly average")
    } else {
      dataset <- input_data$quarterly
      cols_to_keep <- c("Date","Waiting status", "Clinical Prioritisation",
                        "Count", "Quarterly average")

    }


    dataset %<>%
      filter(nhs_board_of_treatment == hbt) %>%
      mutate(urgency = factor(`Clinical Prioritisation`, levels=c("P1A-1B", "P2", "P3", "P4", "Other")),
             indicator = case_when(indicator == "Ongoing" ~ "Waiting",
                                   indicator == "Completed" ~ "Admitted",
                                   indicator == "additions_to_list" ~ "Additions to list")) %>%
      select(cols_to_keep) %>%
      unique()

    names(dataset) <- replace_colnames(names(dataset))


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

  names(dataset) <- replace_colnames(names(dataset))

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
    unique()

  names(dataset) <- replace_colnames(names(dataset))

  return(dataset)



}


