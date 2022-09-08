####################### Specialties Number Functions #######################


spec_activity_table <-  function(input_data,
                                 qend="March 2022",
                                 hbt="NHS Scotland",
                                 specialties=c("All Specialties")) {


  dataset <- input_data %>%
    filter(nhs_board_of_treatment == hbt,
           date == get_short_date(qend),
           specialty %in% input$specialty_filter) %>%
    mutate(indicator = recode_indicator(indicator)) %>%
    make_cols_factors(c("urgency", "indicator", "specialty")) %>%
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
    mutate(weeks = get_pretty_weeks(weeks),
           seen_or_on_list = case_when(ongoing_completed == "Ongoing" ~ "Number on list",
                                       ongoing_completed == "Completed" ~ "Number seen"),
           ongoing_completed = recode_indicator(ongoing_completed)) %>%
    mutate(weeks = factor(weeks, levels=get_pretty_weeks(unique(input_data$weeks)))
    ) %>%
    make_cols_factors(c("ongoing_completed", "specialty", "urgency")) %>%
    select(date, ongoing_completed, nhs_board_of_treatment, specialty, urgency, weeks, `number_seen/on_list`)

  names(dataset) <- replace_colnames(names(dataset))

  return(dataset)

}




