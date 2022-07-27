####################### Waiting List Activity Plot Functions #######################

activity_trendplot <- function(input_data, waiting_status,
                               hbt="NHS Scotland",
                               timescale="monthly") {

  # Waiting status
  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                         waiting_status == "admitted" ~ "Completed",
                         waiting_status == "additions" ~ "additions_to_list",
                         TRUE ~ "")

  if (timescale == "monthly"){
    dataset <- input_data %>% mutate(date_plot = floor_date(date, "month"))
  } else {
    dataset <- input_data %>% mutate(date_plot = date)
  }

 dataset %<>%
    filter(indicator == indicator_string,
           nhs_board_of_treatment == hbt) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )

  yaxis_title <- case_when(waiting_status == "waiting" ~ "Patients waiting",
                           waiting_status == "admitted" ~ "Patients admitted",
                           waiting_status == "additions" ~ "Additions to list",
                           TRUE ~ "")

  xaxis_title <- case_when(timescale == "monthly" ~ "Month ending",
                           timescale == "quarterly" ~ "Quarter ending",
                           TRUE ~ "")

  yaxis_plots[["title"]] <- yaxis_title
  xaxis_plots[["title"]] <- xaxis_title

  tooltip_trend <- glue("{xaxis_title}: {format(dataset$date_plot, '%b %Y')}<br>",
                        "Clinical prioritisation : {dataset$urgency}<br>",
                        "Number of patients: {format(dataset$number, big.mark=',')}<br>",
                        "2019 monthly average: {format(dataset$monthly_avg, big.mark=',')}")

  p <- dataset %>%
      plot_ly(x = ~date_plot) %>%
      add_bars(y = ~number,
             color = ~urgency,
             colors = waiting_times_palette,
             text = tooltip_trend,
             stroke = I("black"),
             hoverinfo = "text",
             name = ~urgency) %>%
      add_lines(y = ~monthly_avg, line = list(color = "black", dash="dash"),
              text = tooltip_trend, hoverinfo = "text",
              name = "2019 monthly average") %>%
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


