####################### Specialties Number Functions #######################


spec_activity_table <-  function(input_data,
                                 qend="March 2022",
                                 hbt="NHS Scotland",
                                 specialties=c("All Specialties")) {


  dataset <- input_data %>%
    filter(nhs_board_of_treatment == hbt,
           date == get_short_date(qend),
           specialty %in% input$specialty_filter) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other", "Total")),
           indicator = factor(recode_indicator(indicator), levels=c("Additions", "Admitted", "Waiting"))) %>%
    select(date, indicator, nhs_board_of_treatment, specialty, urgency, number)

  names(dataset) <- replace_colnames(names(dataset))


  return(dataset)


}

spec_waits_table <- function(input_data,
                             qend="March 2022",
                             hbt="NHS Scotland",
                             specialties=c("All Specialties")) {



  dataset <- input_data %>%
    filter(nhs_board_of_treatment == hbt,
           date == get_short_date(qend),
           specialty %in% specialties) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other", "Total")),
           weeks = get_pretty_weeks(weeks),
           seen_or_on_list = case_when(ongoing_completed == "Ongoing" ~ "Number on list",
                                       ongoing_completed == "Completed" ~ "Number seen")) %>%
    mutate(weeks = factor(weeks, levels=get_pretty_weeks(unique(input_data$weeks)))
    ) %>%
    select(date, ongoing_completed, nhs_board_of_treatment, specialty, urgency, `number_seen/on_list`)

  names(dataset) <- replace_colnames(names(dataset))

  return(dataset)

}




