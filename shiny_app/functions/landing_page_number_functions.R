####################### Landing Page Number Functions #######################

activity_table <- function(input_data, hbt="NHS Scotland", timescale="monthly") {


    if (timescale == "monthly"){
      dataset <- input_data$monthly %>% mutate(date_plot = floor_date(date, "month"))
      cols_to_keep <- c("date", "indicator", "urgency", "number", "monthly_avg")
    } else {
      dataset <- input_data$quarterly %>% mutate(date_plot = date)
      cols_to_keep <- c("date", "indicator", "urgency", "number", "quarterly_avg")

    }

    dataset %<>%
      filter(nhs_board_of_treatment == hbt) %>%
      mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other"))) %>%
      mutate(indicator = case_when(indicator == "Ongoing" ~ "Waiting",
                                   indicator == "Completed" ~ "Admitted",
                                   indicator == "additions_to_list" ~ "Additions to list")) %>%
      select(cols_to_keep) %>%
      unique() %>%
      dplyr::rename("Waiting status" = "indicator",
                    "Clinical Prioritisation" = "urgency",
                    "Number" = "number")

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


