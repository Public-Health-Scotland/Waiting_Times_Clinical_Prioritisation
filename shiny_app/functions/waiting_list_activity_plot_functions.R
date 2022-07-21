####################### Waiting List Activity Plot Functions #######################


activity_trendplot <- function(input_data, waiting_status) {

  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                         waiting_status == "admitted" ~ "Completed",
                         waiting_status == "additions" ~ "additions_to_list",
                         TRUE ~ "")

  dataset <- input_data %>% mutate(month_ending = floor_date(date, "month")) %>%
    filter(indicator == indicator_string) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )

  yaxis_title <- case_when(waiting_status == "waiting" ~ "Patients waiting",
                           waiting_status == "admitted" ~ "Patients admitted",
                           waiting_status == "additions" ~ "Additions to list",
                           TRUE ~ "")

  yaxis_plots[["title"]] <- yaxis_title
  xaxis_plots[["title"]] <- "Month ending"

  tooltip_trend <- glue("Month ending: {format(dataset$month_ending, '%b %Y')}<br>",
                        "Clinical prioritisation : {dataset$urgency}<br>",
                        "Number of patients: {format(dataset$number, big.mark=',')}<br>")

  p <- dataset %>%
      plot_ly(x = ~month_ending) %>%
      add_bars(y = ~number,
             color = ~urgency,
             colors = waiting_times_palette,
             text = tooltip_trend,
             stroke = I("black"),
             hoverinfo = "text",
             name = ~urgency) %>%
      #Layout
      layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           paper_bgcolor = "#F0EFF3",
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(p)


}

additions_trendplot_byboard <- function(input_data,
                                       hbt="NHS Scotland", hbr="NHS SCOTLAND",
                                       chosen_specialty="All Specialties") {

  dataset <- input_data %>% mutate(month_ending = floor_date(date, "month")) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )

  # Filtering for chosen HBR and HBT
  # Filtering out non-Scotland HBRs if NHS SCOTLAND selected as HBR
  if (hbr == "NHS SCOTLAND"){
    dataset %<>% filter(!(health_board_of_residence %in% c("ENGLAND/WALES/NORTHERN IRELAND",
                                                           "NOT KNOWN",
                                                           "OUTSIDE U.K.", NA)))
  } else {
    dataset %<>% filter(health_board_of_residence == hbr)
  }

  dataset %<>% filter(nhs_board_of_treatment == hbt)

  # Filtering for specialty
  dataset %<>% filter(specialty == chosen_specialty)

  # Grouping what remains
  dataset %<>% group_by(month_ending, urgency) %>%
    summarise(tot_additions = sum(additions_to_list))

  yaxis_plots[["title"]] <- "Additions to list"
  xaxis_plots[["title"]] <- "Month ending"

  tooltip_trend <- glue("Month ending: {format(dataset$month_ending, '%b %Y')}<br>",
                        "Clinical prioritisation : {dataset$urgency}<br>",
                        "HBR : {hbr}<br>",
                        "HBT : {hbt}<br>",
                        "Specialty : {chosen_specialty}<br>",
                        "Additions: {format(dataset$tot_additions, big.mark=',')}<br>")

  p <- dataset %>%
    plot_ly(x = ~month_ending) %>%
    add_bars(y = ~tot_additions,
             color = ~urgency,
             colors = waiting_times_palette,
             text = tooltip_trend,
             stroke = I("black"),
             hoverinfo = "text",
             name = ~urgency) %>%
    #Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           paper_bgcolor = "#F0EFF3",
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(p)


}

