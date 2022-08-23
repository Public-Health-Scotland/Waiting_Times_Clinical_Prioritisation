####################### Intro Page Functions #######################

## Functions from ScotPHO dashboard https://github.com/Public-Health-Scotland/scotpho-profiles-tool/
# Creating big boxes for main tabs in the landing page (see ui for formatting css)
intro_main_box <- function(title_box, button_name, description) {
  div(class="intro-page-box",
      div(title_box, class = "intro-page-box-title"),
      div(description, class = "intro-page-box-description"),
      actionButton(button_name, NULL, class="intro-page-button")
  )
}

#make hb completeness table

dq_table <- function(dq_data, hbt, month_year){
  
  
  dataset <- dq_data %>% 
    filter(nhs_board_of_treatment==hbt, date == get_short_date(month_year)) %>%
    mutate(completeness = paste0(round(completeness, 1), "%"),
           indicator = case_when(indicator=="additions_to_list"~"Additions to the list",
                                 indicator=="Completed" ~ "Admitted", 
                                 indicator=="Ongoing" ~ "Waiting")) %>% 
    pivot_wider(names_from = indicator, values_from = completeness) %>% 
    select(`Additions to the list`, Admitted, Waiting)
    
    
  return(dataset)
}


