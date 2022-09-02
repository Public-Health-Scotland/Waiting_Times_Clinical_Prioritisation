####################### Landing Page Number Functions #######################

## Get BANs ----
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
    unique()

  # Displaying 0 if no entries
  if (nrow(ban)==0){
    ban <- "0"
  } else {
    ban %<>% .$number %>%
      format(big.mark=",")
  }

  return(ban)

}


## Activity ----

activity_table <- function(input_data,
                           hbt="NHS Scotland",
                           timescale="monthly",
                           chosen_specialty="All Specialties")  {

  if (timescale == "monthly"){
    dataset <- input_data$monthly
  } else {
    dataset <- input_data$quarterly
  }

  # Removing unneeded columns
  remove_cols <- intersect(names(dataset), cols_to_not_display)

  dataset %<>%
    select(-remove_cols) %>%
    filter(nhs_board_of_treatment == hbt,
           specialty == chosen_specialty,
           urgency != "Total") %>%
    distinct() %>% # removes duplicate rows
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")),
           indicator = case_when(indicator == "Ongoing" ~ "Waiting",
                                 indicator == "Completed" ~ "Admitted",
                                 indicator == "additions_to_list" ~ "Additions to list")) %>%
    make_cols_factors(c("indicator", "nhs_board_of_treatment", "specialty", "patient_type")) %>%
    group_by(across(c(-urgency, -number))) %>%
    mutate(total = sum(number)) %>%
    ungroup() %>%
    unique()

  names(dataset) <- replace_colnames(names(dataset))


  return(dataset)

}


## Waits ----

waits_table <- function(input_data,
                        timescale="monthly",
                        time_chunk_end="March 2022",
                        chosen_specialty="All Specialties",
                        hbt="NHS Scotland") {

  dataset <- input_data[[timescale]]

  # Removing unneeded columns
  remove_cols <- intersect(names(dataset), cols_to_not_display)

  dataset %<>%
    select(-remove_cols) %>%
    filter(date == get_short_date(time_chunk_end),
           specialty == chosen_specialty,
           nhs_board_of_treatment == hbt) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other", "Total")),
           weeks = get_pretty_weeks(weeks)) %>%
    mutate(ongoing_completed = recode_indicator(ongoing_completed),
           weeks=factor(weeks, levels=get_pretty_weeks(unique(input_data[[timescale]]$weeks)))) %>%
    make_cols_factors(c("ongoing_completed")) %>%
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
           `90th_percentile` = round(`90th_percentile`, 0), `number_seen/on_list` = formatC(`number_seen/on_list`, big.mark = ",", format = "fg" )) %>%
    select(urgency, `number_seen/on_list`, median, `90th_percentile`) %>%
    unique() %>%
    arrange(urgency) %>%
    dplyr::rename("Final priority" = "urgency",
                  "Median (days)" = "median",
                  "90th percentile (days)" = "90th_percentile",
                  "Patients admitted" = "number_seen/on_list")

  return(dataset)



}



