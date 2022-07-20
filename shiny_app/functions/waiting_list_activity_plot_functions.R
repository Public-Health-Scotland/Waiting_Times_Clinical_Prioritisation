####################### Waiting List Activity Plot Functions #######################


activity_trendplot <- function(dataset, waiting_status) {

  yaxis_title <- case_when(waiting_status == "waiting" ~ "Patients waiting",
                           waiting_status == "admitted" ~ "Patients admitted",
                           TRUE ~ "")

  yaxis_plots[["title"]] <- yaxis_title
  xaxis_plots[["title"]] <- "Month ending"

  tooltip_trend <- glue("Month ending: {format(dataset$date, '%b %Y')}<br>",
                        "Clinical prioritisation : {dataset$urgency}<br>",
                        "Number of patients: {dataset$number}<br>")

  dataset %>%
    plot_ly(x = ~date) %>%
    add_bars(y = ~number,
             color = ~urgency,
             colors = phs_colours(c("phs-green", "phs-purple",
                                    "phs-blue", "phs-magenta", "phs-liberty")),
             text = tooltip_trend,
             stroke = I("black"),
             hoverinfo = "text",
             name = ~date) %>%
    #Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group

    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )


}

