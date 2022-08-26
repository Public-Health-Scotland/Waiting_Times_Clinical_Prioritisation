####################### Landing Page Plot Functions #######################

## Activity ----
activity_trendplot <- function(input_data, waiting_status,
                               hbt="NHS Scotland",
                               timescale="monthly",
                               chosen_specialty="All Specialties") {

  # Waiting status
  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                         waiting_status == "admitted" ~ "Completed",
                         waiting_status == "additions" ~ "additions_to_list",
                         TRUE ~ "")

  if (timescale == "monthly"){
    dataset <- input_data$monthly %>% mutate(date_plot = floor_date(date, "month"))
    cols_to_keep <- c("date_plot", "urgency", "number", "monthly_avg", "total")
  } else {
    dataset <- input_data$quarterly %>% mutate(date_plot = date)
    cols_to_keep <- c("date_plot", "urgency", "number", "quarterly_avg", "total")
  }

 dataset %<>%
    filter(indicator == indicator_string,
           nhs_board_of_treatment == hbt,
           specialty == chosen_specialty,
           urgency != "Total") %>%
   distinct() %>% #removes duplicate rows
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other"))) %>%
   group_by(across(c(-urgency, -number))) %>%
   mutate(total = sum(number)) %>%
   select(cols_to_keep) %>%
   ungroup() %>%
   unique()

  yaxis_title <- case_when(waiting_status == "waiting" ~ "Patients waiting",
                           waiting_status == "admitted" ~ "Patients admitted",
                           waiting_status == "additions" ~ "Additions to list",
                           TRUE ~ "")

  plot_title <- case_when(waiting_status == "waiting" ~ "Patients waiting for treatment",
                           waiting_status == "admitted" ~ "Patients admitted for treatment",
                           waiting_status == "additions" ~ "Patients added to the list",
                           TRUE ~ "")

  xaxis_title <- case_when(timescale == "monthly" ~ "Month ending",
                           timescale == "quarterly" ~ "Quarter ending",
                           TRUE ~ "")

  yaxis_plots[["title"]] <- yaxis_title
  xaxis_plots[["title"]] <- xaxis_title
  xaxis_plots[["type"]] <- "category" # display only quarters on quaterly plot axis



  if(timescale == "monthly"){

    tooltip_trend <- glue("2019 monthly average: {format(dataset$monthly_avg, big.mark=',')}")

  } else {

    tooltip_trend <- glue("2019 quaterly average: {format(dataset$quarterly_avg, big.mark=',')}")

  }

  time_name = case_when(timescale == "monthly" ~ "Month", timescale == "quarterly" ~ "Quarter")

  p <- dataset %>%
    arrange(date_plot) %>%
      plot_ly(x = ~factor(get_month(date_plot, format="%b<br>%Y"),
                          levels = format(unique(date_plot), "%b<br>%Y")),
              height = 320, legendgroup=~urgency) %>%
      add_bars(y = ~number,
             color = ~urgency,
             colors = waiting_times_palette,
             stroke = I("black"),
             text= ~paste0("<b>HBT</b>: ", nhs_board_of_treatment, "\n",
                          "<b>Specialty</b>: ", specialty, "\n",
                          "<b>", time_name, " ending</b>: ", get_month(date_plot, format="%b %Y"), "\n",
                          "<b>CP</b>: ", urgency, "\n",
                          "<b>Number of patients</b>: ", format(number, big.mark=','), "\n",
                          "<b>Total</b>: ", format(total, big.mark=',')),
             hovertemplate = "%{text}"
             )

    if (timescale == "monthly"){
      p %<>% add_lines(y = ~monthly_avg, line = list(color = "black", dash="dash"),
                       text = tooltip_trend, hoverinfo = "text",
                       name = "2019 monthly average",
                       legendgroup = "average")
    } else {
      p %<>% add_lines(y = ~quarterly_avg, line = list(color = "black", dash="dash"),
                       text = tooltip_trend, hoverinfo = "text",
                       name = "2019 quarterly average",
                       legendgroup = "average")
    }
      #Layout
     p %<>%  layout(margin = list(b = 30, t = 10), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           paper_bgcolor = phs_colours("phs-liberty-10"),
           plot_bgcolor = phs_colours("phs-liberty-10"),
           legend = list(x = 100, y = 0.5, title=list(text='Clinical Prioritisation')), #position of legend
           barmode = "stack") %>% #split by group
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(p)


}

## Distribution of waits ----
waits_distribution_plot <- function(input_data, waiting_status,
                                    timescale="monthly",
                                    time_chunk_end="June 2022",
                                    chosen_specialty="All Specialties",
                                    hbt="NHS Scotland") {

  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                                waiting_status == "admitted" ~ "Completed",
                                TRUE ~ "")

  dataset <- input_data[[timescale]] %>%
    filter(ongoing_completed == indicator_string,
           date == get_short_date(time_chunk_end),
           specialty == chosen_specialty,
           nhs_board_of_treatment == hbt,
           urgency!="Total") %>%
    distinct() %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")),
           weeks = get_pretty_weeks(weeks)) %>%
    mutate(weeks=factor(weeks, levels=get_pretty_weeks(unique(input_data[[timescale]]$weeks)))) %>%
    group_by(across(c(-urgency, -`number_seen/on_list`))) %>%
    mutate(total = sum(`number_seen/on_list`)) %>%
    select(date, weeks, `number_seen/on_list`, specialty, nhs_board_of_treatment, urgency,total) %>%
    unique()


  yaxis_title <- case_when(waiting_status == "waiting" ~ "Patients waiting",
                           waiting_status == "admitted" ~ "Patients admitted",
                           TRUE ~ "")

  yaxis_plots[["title"]] <- yaxis_title
  xaxis_plots[["title"]] <- "Weeks waiting"

  time_name = case_when(timescale == "monthly" ~ "Month", timescale == "quarterly" ~ "Quarter")

  tooltip_trend <- glue("<b>HBT</b>: {dataset$nhs_board_of_treatment}<br>",
                        "<b>Specialty</b>: {dataset$specialty}<br>",
                        "<b>{time_name} ending</b>: {time_chunk_end}<br>",
                        "<b>CP</b>: {dataset$urgency}<br>",
                        "<b>Weeks waiting</b>: {dataset$weeks}<br>",
                        "<b>Number of patients</b>: {format(dataset$`number_seen/on_list`, big.mark=',')}<br>",
                        "<b>Total</b>: {format(dataset$total, big.mark=',')}")

  p <- dataset %>%
    plot_ly(x = ~weeks, height = 600) %>%
    add_bars(y = ~`number_seen/on_list`,
             color = ~urgency,
             colors = waiting_times_palette,
             text = tooltip_trend,
             stroke = I("black"),
             hoverinfo = "text",
             legendgroup = ~urgency,
             name = ~urgency) %>%
    #Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           paper_bgcolor = phs_colours("phs-liberty-10"),
           plot_bgcolor = phs_colours("phs-liberty-10"),
           shapes = list(vline(12.5)), #Need to play around with where this sits on the graph
           legend = list(x = 100, y = 0.5, title=list(text='Clinical Prioritisation')), #position of legend
           barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(p)


}


