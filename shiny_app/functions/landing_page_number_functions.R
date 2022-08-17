####################### Landing Page Number Functions #######################

## Get BANs
ban <- function(input_data, cp,
                waiting_status="waiting",
                hbt="NHS Scotland", timescale="monthly",
                chosen_specialty="All Specialties"){

  if (timescale == "monthly"){
    dataset <- input_data$monthly
  } else {
    dataset <- input_data$quarterly
  }

  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                                waiting_status == "admitted" ~ "Completed",
                                waiting_status == "additions" ~ "additions_to_list",
                                TRUE ~ "")

  ban <- dataset %>%
    filter(nhs_board_of_treatment == hbt,
           indicator == indicator_string,
           specialty == chosen_specialty,
           urgency == cp,
           date == max(date)) %>%
    .$number %>% unique() %>% .[[1]] %>%
    format(big.mark=",")

  return(ban)

}


## Activity

activity_table <- function(input_data, hbt="NHS Scotland", timescale="monthly",
                           chosen_specialty="All Specialties") {


    if (timescale == "monthly"){
      dataset <- input_data$monthly
      cols_to_keep <- c("date", "indicator", "urgency", "number", "monthly_avg")
    } else {
      dataset <- input_data$quarterly
      cols_to_keep <- c("date", "indicator", "urgency", "number", "quarterly_avg")

    }


    dataset %<>%
      filter(nhs_board_of_treatment == hbt) %>%
      mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other", "Total")),
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
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other", "Total")),
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
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other", "Total")),
           `90th_percentile` = round(`90th_percentile`, 0)) %>%
    select(urgency, median, `90th_percentile`) %>%
    unique() %>%
    arrange(urgency) %>%
    dplyr::rename("CP" = "urgency",
                  "Median (days)" = "median",
                  "90th percentile (days)" = "90th_percentile")

  return(dataset)



}



