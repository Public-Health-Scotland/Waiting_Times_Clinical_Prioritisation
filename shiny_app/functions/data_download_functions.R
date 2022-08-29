####################### Data Download Functions #######################

process_dataset_for_download <- function(df){

  # Removing unneeded columns
  remove_cols <- intersect(names(df), cols_to_not_display)
  df %<>% select(-remove_cols) %>% unique()

  # Replacing column names with more readable versions
  names(df) <- replace_colnames(names(df))
  
  df$Waiting_status <- recode_indicator(df$Waiting_status)

  # Making certain columns factors
  factorcols <- c("Clinical_Prioritisation",
                  "Waiting_status",
                  "Health_Board_of_Treatment",
                  "Specialty",
                  "Patient_type",
                  "Weeks_waiting")
  
  df %<>% make_cols_factors(intersect(names(df), factorcols))

  return(df)

}

## Download display table

data_download_table <-  function(input_data,
                                 hbts=c("NHS Scotland"),
                                 chosen_specialties=c("")) {

  dataset <- input_data %>%
    filter(nhs_board_of_treatment %in% hbts,
           specialty %in% chosen_specialties) %>%
    process_dataset_for_download

  return(dataset)


}

data_download_table_head <-  function(input_data,
                                 hbts=c("NHS Scotland"),
                                 chosen_specialties=c("")) {

  remove_cols <- intersect(names(input_data), cols_to_not_display)

  dataset <- input_data %>%

    filter(nhs_board_of_treatment %in% hbts,
           specialty %in% chosen_specialties) %>%
    head(10) %>%
    process_dataset_for_download

  return(dataset)


}

data_download_table_summary <-  function(input_data,
                                      hbts=c("NHS Scotland"),
                                      chosen_specialties=c("")) {

  remove_cols <- intersect(names(input_data), cols_to_not_display)

  dataset <- input_data %>%
    filter(nhs_board_of_treatment %in% hbts,
           specialty %in% chosen_specialties) %>%
    process_dataset_for_download

  summary <- summary(dataset)

  return(summary)


}



