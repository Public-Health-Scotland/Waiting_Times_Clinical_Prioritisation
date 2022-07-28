####################### Landing Page Plot Functions #######################

## Activity
activity_trendplot <- function(input_data, waiting_status,
                               hbt="NHS Scotland",
                               timescale="monthly") {

  # Waiting status
  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                         waiting_status == "admitted" ~ "Completed",
                         waiting_status == "additions" ~ "additions_to_list",
                         TRUE ~ "")

  if (timescale == "monthly"){
    dataset <- input_data$monthly %>% mutate(date_plot = floor_date(date, "month"))
    cols_to_keep <- c("date_plot", "urgency", "number", "monthly_avg")
  } else {
    dataset <- input_data$quarterly %>% mutate(date_plot = date)
    cols_to_keep <- c("date_plot", "urgency", "number", "quarterly_avg")
  }

 dataset %<>%
    filter(indicator == indicator_string,
           nhs_board_of_treatment == hbt) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) ) %>%
   select(cols_to_keep) %>%
   unique()

  yaxis_title <- case_when(waiting_status == "waiting" ~ "Patients waiting",
                           waiting_status == "admitted" ~ "Patients admitted",
                           waiting_status == "additions" ~ "Additions to list",
                           TRUE ~ "")

  xaxis_title <- case_when(timescale == "monthly" ~ "Month ending",
                           timescale == "quarterly" ~ "Quarter ending",
                           TRUE ~ "")

  yaxis_plots[["title"]] <- yaxis_title
  xaxis_plots[["title"]] <- xaxis_title

  if(timescale == "monthly"){

    tooltip_trend <- glue("{xaxis_title}: {format(dataset$date_plot, '%b %Y')}<br>",
                          "Clinical prioritisation : {dataset$urgency}<br>",
                          "Number of patients: {format(dataset$number, big.mark=',')}<br>",
                          "2019 monthly average: {format(dataset$monthly_avg, big.mark=',')}")

  } else {

    tooltip_trend <- glue("{xaxis_title}: {format(dataset$date_plot, '%b %Y')}<br>",
                          "Clinical prioritisation : {dataset$urgency}<br>",
                          "Number of patients: {format(dataset$number, big.mark=',')}<br>")
  }



  p <- dataset %>%
      plot_ly(x = ~date_plot) %>%
      add_bars(y = ~number,
             color = ~urgency,
             colors = waiting_times_palette,
             text = tooltip_trend,
             stroke = I("black"),
             hoverinfo = "text",
             name = ~urgency)

    if (timescale == "monthly"){
      p %<>% add_lines(y = ~monthly_avg, line = list(color = "black", dash="dash"),
                       text = tooltip_trend, hoverinfo = "text",
                       name = "2019 monthly average")
    } else {
      p %<>% add_lines(y = ~quarterly_avg, line = list(color = "black", dash="dash"),
                       text = tooltip_trend, hoverinfo = "text",
                       name = "2019 quarterly average")
    }
      #Layout
     p %<>%  layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           paper_bgcolor = "#F0EFF3",
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(p)


}

# ----------------------------------------------------------------------------
## Distribution of waits
waits_distribution_plot <- function(input_data, waiting_status,
                                    timescale="monthly",
                                    time_chunk_end="March 2022",
                                    chosen_specialty="All Specialties",
                                    hbt="NHS Scotland") {

  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                                waiting_status == "admitted" ~ "Completed",
                                TRUE ~ "")

  dataset <- input_data[[timescale]] %>%
    filter(ongoing_completed == indicator_string,
           date == get_short_date(time_chunk_end),
           specialty == chosen_specialty,
           nhs_board_of_treatment == hbt) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) ) %>%
    select(date, weeks, `number_seen/on_list`, specialty, nhs_board_of_treatment, urgency) %>%
    unique()

  yaxis_title <- case_when(waiting_status == "waiting" ~ "Patients waiting",
                           waiting_status == "admitted" ~ "Patients admitted",
                           TRUE ~ "")

  yaxis_plots[["title"]] <- yaxis_title
  xaxis_plots[["title"]] <- "Weeks waiting"

  time_name = case_when(timescale == "monthly" ~ "Month", timescale == "quarterly" ~ "Quarter")

  tooltip_trend <- glue("{time_name} ending: {time_chunk_end}<br>",
                        "Weeks waiting: {dataset$weeks}<br>",
                        "HBT: {hbt}<br>",
                        "Clinical prioritisation: {dataset$urgency}<br>",
                        "Specialty: {chosen_specialty}<br>",
                        "Number of patients: {format(dataset$`number_seen/on_list`, big.mark=',')}<br>")


  p <- dataset %>%
    plot_ly(x = ~weeks) %>%
    add_bars(y = ~`number_seen/on_list`,
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


