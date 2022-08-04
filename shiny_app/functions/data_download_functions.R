####################### Data Download Functions #######################

data_download_table <-  function(input_data,
                                 hbts=c("NHS Scotland"),
                                 chosen_specialties=c("All Specialties")) {

  dataset <- input_data %>%
    filter(nhs_board_of_treatment %in% hbts,
           specialties %in% chosen_specialties) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )


  return(dataset)


}