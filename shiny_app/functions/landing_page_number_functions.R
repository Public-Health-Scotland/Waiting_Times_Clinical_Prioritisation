####################### Landing Page Number Functions #######################

activity_table <- function(input_data, hbt="NHS Scotland", timescale="monthly") {


    if (timescale == "monthly"){
      dataset <- input_data$monthly %>%
        dplyr::rename("Month ending" = "date",
                      "Waiting status" = "indicator",
                      "Clinical Prioritisation" = "urgency",
                      "Number" = "number",
                      "Monthly average" = "monthly_avg")
      cols_to_keep <- c("Month ending","Waiting status", "Clinical Prioritisation",
                        "Number", "Monthly average")
    } else {
      dataset <- input_data$quarterly %>%
        dplyr::rename("Quarter ending" = "date",
                      "Waiting status" = "indicator",
                      "Clinical Prioritisation" = "urgency",
                      "Number" = "number",
                      "Quarterly average" = "quarterly_avg")
      cols_to_keep <- c("Quarter ending","Waiting status", "Clinical Prioritisation",
                        "Number", "Quarterly average")

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


