####################### Health Board of Treatment Plot Functions #######################

## Faceted activity graph

activity_specs_hbt <- function(input_data, waiting_status,
                           qend="March 2022",
                           hbts=c("NHS Scotland"),
                           specialty_choice="All Specialties") {

  # Waiting status
  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                                waiting_status == "admitted" ~ "Completed",
                                waiting_status == "additions" ~ "additions_to_list",
                                TRUE ~ "")

  dataset <- input_data %>%
    filter(nhs_board_of_treatment %in% hbts,
           indicator == indicator_string,
           !urgency == "Total",
           date == get_short_date(qend),
           specialty == specialty_choice) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) ) %>%
    mutate(nhs_board_of_treatment = forcats::fct_reorder(as.factor(nhs_board_of_treatment),
                                                       p2_proportion, .desc=FALSE))

  # Wrapping text on specialties for plotting
  dataset$nhs_board_of_treatment_wrapped <- purrr::map_chr(dataset$nhs_board_of_treatment, wrap_label)

  facets <- unique(dataset$indicator)

  # Check that there is enough data to be displayed, otherwise throw error to user
  # Checking there are 1 or more facets and the dataset is not entirely populated by zeros
  validate(
    need(((length(facets)>=1) & (unique(dataset$number) != 0)),
         "There are no entries matching your selection. Please choose again.")
  )

  plot_title <- case_when(waiting_status == "waiting" ~ "Patients waiting",
                           waiting_status == "admitted" ~ "Patients admitted",
                           waiting_status == "additions" ~ "Additions to list",
                           TRUE ~ "")
   yaxis_plots[["tickfont"]] <- 14
   xaxis_plots[["title"]] <- "Proportion of Patients (%)"


  p <- dataset %>%
    plot_ly(x = ~round(proportion,2),
            y = ~factor(nhs_board_of_treatment_wrapped),
            height = 600,
            type = "bar",
            orientation = 'h', #make bar chart horizontal
            customdata = ~number,
            text = ~total,
            color = ~urgency,
            colors = waiting_times_palette,
            # stroke = I("black"),
            marker = list(line = list(color = "black", width = 1)),
            legendgroup = ~urgency,
            # showlegend = (~unique(indicator) == "additions_to_list"),
            hovertemplate = paste(
              "<b>Healthboard</b>:  %{y}",
              "<b>Number of Patients</b>: %{customdata:,}",
              "<b>Percentage</b>: %{x}",
              "<b>Total</b>: %{text:,}",
              sep = "\n")) %>%
    add_annotations(
      text = ~paste("\n",strwrap(unique(plot_title),25), collapse="\n"),
      x = 0,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "bottom",
      showarrow = FALSE,
      font = list(size = 14, face = "bold")
    )

  p %<>%  layout(margin = list(b = 80, t = 50), #to avoid labels getting cut out
                 yaxis = yaxis_plots, xaxis = xaxis_pc,
                 paper_bgcolor = phs_colours("phs-liberty-10"),
                 plot_bgcolor = phs_colours("phs-liberty-10"),
                 legend = list(x = 100, y = 0.5), #position of legend
                 barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(p)

}

# --------------------------------------------------------------------------
## Faceted waits graph

waits_specs_hbt <- function(input_data,
                        qend="March 2022",
                        hbts=c("NHS Scotland"),
                        specialty_choice="All Specialties") {



  dataset <- input_data %>%
    filter(nhs_board_of_treatment %in% hbts,
           date == get_short_date(qend),
           !urgency == "Total",
           specialty == specialty_choice) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")),
           weeks = get_pretty_weeks(weeks),
           seen_or_on_list = case_when(ongoing_completed == "Ongoing" ~ "Number on list",
                                       ongoing_completed == "Completed" ~ "Number seen")) %>%
    mutate(weeks = factor(weeks, levels=get_pretty_weeks(unique(input_data$weeks)))
    )


  facets <- unique(dataset$nhs_board_of_treatment)

  # Checking there are 1 or more facets and the dataset is not entirely populated by zeros
  validate(
    need(((length(facets)>=1) & (unique(dataset$`number_seen/on_list`) != 0)),
         "There are no entries matching your selection. Please choose again.")
  )


  p <- ggplot(dataset, aes(x=weeks, y=`number_seen/on_list`, group=urgency,
                           text = paste0(
                             '</br>Weeks waiting: ', weeks,
                             '</br>HBT: ', nhs_board_of_treatment,
                             '</br>Urgency: ', urgency,
                             '</br>', seen_or_on_list, ': ', format(`number_seen/on_list`, big.mark=","))
  )) +
    geom_col(aes(fill = urgency),
             position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = waiting_times_palette) +
    scale_y_continuous() +
    xlab("Weeks waiting") +
    ylab("Number seen / on list") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          strip.text.x = element_text(colour = phs_colours("phs-purple"), size=12, angle=0),
          strip.text.y = element_text(colour = phs_colours("phs-purple"), size=12, angle=360),
          axis.text.x = element_text(angle=70, size=12),
          axis.title.x = element_text(margin=margin(t=500)),
          axis.title.y = element_text(margin=margin(r=500))) +
    facet_grid(nhs_board_of_treatment ~ ongoing_completed,  scales="free_y",
               # This wraps the facet label text to fit it on the plot
               labeller = label_wrap_gen(width=10))


  plotlyp <- ggplotly(p, height=1200, tooltip=c("text"))%>%
    #Layout
    layout(margin = list(l=100, r=150, b=160, t=50, pad=0), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           paper_bgcolor = phs_colours("phs-liberty-10"),
           plot_bgcolor = phs_colours("phs-liberty-10"),
           # Explanation of this legend nightmare:
           # -------------------------------------
           # 1. orientation h means horizontal i.e. the legend reads left to right
           # 2. paper means that the reference scale for x/y axes are set to [0,1]
           #    where 0 is left/bottom and 1 is right/top, respectively
           #    (numbers outwith this interval are outwith the plot area)
           # 3. anchors set to center means that the centre of the legend is its
           #    reference point
           # 4. x and y are the locations of the centre of the legend on this paper scale
           #    i.e. legend x is in middle of plot and legend y is 15% of plot's size below
           #    the plot
           legend = list(orientation = "h", xref="paper", yref="paper",
                         xanchor="center", yanchor="center",
                         x=0.5, y=-0.15), #position of legend
           barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) %>%
    stop_axis_title_overlap()

  return(plotlyp)


}

# --------------------------------------------------------------------------
## Data tables

spec_activity_table_hbt <-   function(input_data,
                                  qend="March 2022",
                                  hbts=c("NHS Scotland"),
                                  specialty_choice="All Specialties") {



  dataset <- input_data %>%
    filter(nhs_board_of_treatment %in% hbts,
           date == get_short_date(qend),
           specialty == specialty_choice) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other", "Total")) ) %>%
    select(date, indicator, nhs_board_of_treatment, specialty, urgency, number)

  names(dataset) <- replace_colnames(names(dataset))

  return(dataset)


}

spec_waits_table_hbt <- function(input_data,
                             qend="March 2022",
                             hbts=c("NHS Scotland"),
                             specialty_choice="All Specialties") {



  dataset <- input_data %>%
    filter(nhs_board_of_treatment %in% hbts,
           date == get_short_date(qend),
           specialty == specialty_choice) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other", "Total")),
           weeks = get_pretty_weeks(weeks),
           seen_or_on_list = case_when(ongoing_completed == "Ongoing" ~ "Number on list",
                                       ongoing_completed == "Completed" ~ "Number seen")) %>%
    mutate(weeks = factor(weeks, levels=get_pretty_weeks(unique(input_data$weeks)))
    ) %>%
    select(date, ongoing_completed, nhs_board_of_treatment, specialty, urgency, `number_seen/on_list`)

  names(dataset) <- replace_colnames(names(dataset))

  return(dataset)

}




