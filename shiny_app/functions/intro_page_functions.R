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
    filter(nhs_board_of_treatment==hbt ) %>%
    mutate(completeness = round(completeness, 1),
           comp_format = paste0(round(completeness, 1), "%"),
           indicator = case_when(indicator=="additions_to_list"~"Additions to the list",
                                 indicator=="Completed" ~ "Admitted", 
                                 indicator=="Ongoing" ~ "Waiting")
          ) %>% 
    group_by(indicator) %>% 
    summarise(comp_format = comp_format, date = date, 
              # mini_plot = spk_chr(completeness, tooltipFormat = '{{x}}: {{y}}', height = 30, width = 100)) %>% 
              mini_plot = spk_chr(
                completeness,
                type="bar",
                tooltipFormatter = htmlwidgets::JS(
                  sprintf(
                    "function(sparkline, options, field){
                    debugger;
                    return %s[field[0].offset] + '<br/>' + field[0].value;}",
      jsonlite::toJSON(
        format(
          date
        )
      )
    )
  )
)
               ) %>% 
    filter(date == get_short_date(month_year)) %>% 
    # pivot_wider(names_from = indicator, values_from = completeness) %>% 
    select(indicator, comp_format, mini_plot) %>% 
    rename("Waiting Status" = "indicator", "Completeness (%)"="comp_format", "Trend"="mini_plot")
    
    
  return(dataset)
}


