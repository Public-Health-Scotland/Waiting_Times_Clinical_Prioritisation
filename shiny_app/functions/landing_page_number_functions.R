####################### Landing Page Number Functions #######################

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

    #dataset <- setNames(data.frame(t(dataset[,-1])), dataset$urgency)

  return(dataset)



}