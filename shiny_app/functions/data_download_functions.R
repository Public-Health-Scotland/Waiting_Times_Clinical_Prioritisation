####################### Data Download Functions #######################

## Download display table

data_download_table <-  function(input_data,
                                 hbts=c("NHS Scotland"),
                                 chosen_specialties=c("")) {

  remove_cols <- intersect(names(input_data), cols_to_not_display)

  dataset <- input_data %>%
    filter(nhs_board_of_treatment %in% hbts,
           specialty %in% chosen_specialties) %>%
    select(-remove_cols) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )

  # Replacing column names with more readable versions
  names(dataset) <- replace_colnames(names(dataset))

  return(dataset)


}

data_download_table_head <-  function(input_data,
                                 hbts=c("NHS Scotland"),
                                 chosen_specialties=c("")) {

  remove_cols <- intersect(names(input_data), cols_to_not_display)

  dataset <- input_data %>%
    head(10) %>%
    filter(nhs_board_of_treatment %in% hbts,
           specialty %in% chosen_specialties) %>%
    select(-remove_cols) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )

  # Replacing column names with more readable versions
  names(dataset) <- replace_colnames(names(dataset))

  return(dataset)


}

data_download_table_summary <-  function(input_data,
                                      hbts=c("NHS Scotland"),
                                      chosen_specialties=c("")) {

  remove_cols <- intersect(names(input_data), cols_to_not_display)

  dataset <- input_data %>%
    filter(nhs_board_of_treatment %in% hbts,
           specialty %in% chosen_specialties) %>%
    select(-remove_cols) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )

  # Replacing column names with more readable versions
  names(dataset) <- replace_colnames(names(dataset))

  summary <- summary(dataset)

  return(summary)


}



