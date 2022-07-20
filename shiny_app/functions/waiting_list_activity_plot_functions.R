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
                        "Number of patients: {dataset$number}<br>")

  p <- dataset %>%
      plot_ly(x = ~month_ending) %>%
      add_bars(y = ~number,
             color = ~urgency,
             colors = phs_colours(c("phs-green","phs-purple", "phs-blue", "phs-magenta", "phs-liberty")),
             text = tooltip_trend,
             stroke = I("black"),
             hoverinfo = "text",
             name = ~urgency) %>%
      #Layout
      layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(p)


}

