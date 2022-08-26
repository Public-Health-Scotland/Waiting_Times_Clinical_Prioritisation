####################### Specialties Plot Functions #######################

## Get top six specialties given quarter and HBT
topsix_specs <- function(qend, hbt){

 specs <- app_data[["topsix_specs_jun"]] %>%
          filter(date==get_short_date(qend),
             nhs_board_of_treatment==hbt) %>%
          select("specialties") %>% .[[1]] %>%
          strsplit(split = '\"') %>%
          unlist()

  specs <- specs[! specs %in% c("c(", ", ", ")")]

  return(specs)

}

## Faceted activity graph

activity_specs <- function(input_data,
                           waiting_status,
                           qend="March 2022",
                           hbt="NHS Scotland",
                           specialties=c("All Specialties")) {

  # Waiting status
  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                                waiting_status == "admitted" ~ "Completed",
                                waiting_status == "additions" ~ "additions_to_list",
                                TRUE ~ "")


  dataset <- input_data %>%
    filter(nhs_board_of_treatment == hbt,
           indicator == indicator_string,
           !urgency == "Total",
           date == get_short_date(qend),
           specialty %in% input$specialty_filter) %>%
    mutate(
      urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )

  # Wrapping text on specialties for plotting
  dataset$specialty_wrapped <- purrr::map_chr(dataset$specialty, wrap_label)

  plot_title <- case_when(waiting_status == "waiting" ~ "Patients waiting for treatment",
                           waiting_status == "admitted" ~ "Patients admitted for treatment",
                           waiting_status == "additions" ~ "Additions to the waiting list",
                           TRUE ~ "")

  xaxis_plots[["categoryorder"]] <-"trace"
  # To stop plotly being stupid and rounding to whole number in tooltip on the sly
  # https://stackoverflow.com/questions/68007438/r-how-to-stop-rounding-percentages-to-0-decimal-places-on-plotly-chart
  yaxis_plots[["tickformat"]] <- ".1%"

  # facets <- unique(dataset$indicator)

  p <- dataset %>%
    plot_ly(x = ~factor(specialty_wrapped),
            y = ~round(proportion,3),
            height = 600,
            type = "bar",
            customdata = ~number,
            text = ~total,
            color = ~urgency,
            colors = waiting_times_palette,
            # stroke = I("black"),
            marker = list(line = list(color = "black", width = 1)),
            legendgroup = ~urgency,
            hovertemplate = paste(
              "<b>Specialty</b>:  %{x}",
              "<b>Number of Patients</b>: %{customdata:,}",
              "<b>Percentage</b>: %{y}",
              "<b>Total</b>: %{text:,}",
              sep = "\n")) %>%
    add_annotations(
      text = ~paste("\n",unique(plot_title), collapse="\n"),
      x = 0,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "bottom",
      showarrow = FALSE,
      font = list(size = 14, face = "bold")
    )

  p %<>%  layout(margin = list(b = 80, t = 50), # To avoid labels getting cut out
                 yaxis = yaxis_plots, xaxis = xaxis_plots,
                 paper_bgcolor = phs_colours("phs-liberty-10"),
                 plot_bgcolor = phs_colours("phs-liberty-10"),
                 # Position of legend
                 legend = list(x = 100, y = 0.5, title=list(text='Clinical Prioritisation')),
                 barmode = "stack") %>% #split by group
    # Leaving only save plot button
    config(displaylogo = F, displayModeBar = FALSE,  modeBarButtonsToRemove = bttn_remove )

  return(p)

}

# --------------------------------------------------------------------------
## Faceted waits graph

#makes spec DoW plot
waits_specs <- function(input_data, waiting_status,
                           qend="March 2022",
                           hbt="NHS Scotland",
                        chosen_specialty="All Specialties",
                        legend = FALSE) {

  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                                waiting_status == "admitted" ~ "Completed",
                                TRUE ~ "")


  dataset <- input_data %>%
    filter(nhs_board_of_treatment == hbt,
           ongoing_completed == indicator_string,
           !urgency == "Total",
           date == get_short_date(qend),
           specialty == chosen_specialty) %>%
    distinct() %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")),
           weeks = get_pretty_weeks(weeks),
           seen_or_on_list = case_when(ongoing_completed == "Ongoing" ~ "Number on list",
                                      ongoing_completed == "Completed" ~ "Number seen")) %>%
    mutate(weeks = factor(weeks, levels=get_pretty_weeks(unique(input_data$weeks)))
           ) %>%
    group_by(across(c(-urgency, -`number_seen/on_list`))) %>%
    mutate(total = sum(`number_seen/on_list`)) %>%
    ungroup() %>%
    unique()


  yaxis_title <- chosen_specialty

  yaxis_plots[["title"]] <- yaxis_title
  xaxis_plots[["title"]] <- "Weeks waiting"


  tooltip_trend <- glue("Quarter ending: {qend}<br>",
                        "Weeks waiting: {dataset$weeks}<br>",
                        "HBT: {dataset$nhs_board_of_treatment}<br>",
                        "Clinical prioritisation: {dataset$urgency}<br>",
                        "Specialty: {dataset$specialty}<br>",
                        "Number of patients: {format(dataset$`number_seen/on_list`, big.mark=',')}<br>",
                        "<b>Total</b>: {format(dataset$total, big.mark=',')}")

# code to create reference line
  vline <- function(x = 0, color = "black") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 1, 
      yref = "paper",
      xref = "x",
      x0 = x, 
      x1 = x, 
      line = list(color = color, dash = "dash")
    )
  }
  
  p <- dataset %>%
    plot_ly(x = ~weeks, height = 1200) %>%
    add_bars(y = ~`number_seen/on_list`,
             color = ~urgency,
             colors = waiting_times_palette,
             text = tooltip_trend,
             stroke = I("black"),
             hoverinfo = "text",
             legendgroup = ~urgency,
             name = ~urgency,
             showlegend = legend) %>%
    #Layout
    layout(#to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           paper_bgcolor = phs_colours("phs-liberty-10"),
           plot_bgcolor = phs_colours("phs-liberty-10"),
           shapes = list(vline(12.5)),
           legend = list(x = 100, y = 0.5, title=list(text='Clinical Prioritisation')), #position of legend
           barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(p)

}

#calls wait_specs and wraps them in facetted view for chosen waiting_status
make_dow_spec_suplots <- function(data, plotdata, specialties = c("All Specialties"), n_specs,
                             waiting_status, qend, hbt){

  validate(
    need((length(specialties)>=1),
         "There are no entries matching your selection. Please choose again.")
  )


  plot_list <- vector("list", length = n_specs) #initialize empty list to store plots

  # Get specialties in order by descending P2 proportion
  specs_ordered <- data %>%
    filter(specialty %in% specialties,
           date==get_short_date(qend),
           nhs_board_of_treatment == hbt) %>%
    select("specialty", "p2_proportion") %>%
    unique() %>%
    arrange(desc(p2_proportion)) %>%
    .$specialty


  #create patients waiting DoW plots for each spec
  for(i in seq_along(specs_ordered)){

    if(i < n_specs){
      spec_plot <- waits_specs(input_data = data,
                               waiting_status = waiting_status,
                               qend=input$quarter_end_spec,
                               hbt=input$hbt_filter_spec,
                               chosen_specialty = specs_ordered[[i]])
    }

    else{ #add legend to last plot
      spec_plot <- waits_specs(input_data = data,
                               waiting_status = waiting_status,
                               qend=input$quarter_end_spec,
                               hbt=input$hbt_filter_spec,
                               chosen_specialty = specs_ordered[[i]],
                               legend = TRUE)
    }

    plot_list[[i]] <- spec_plot #save plot
  }

  plot_title <- case_when(waiting_status == "waiting" ~ "Patients waiting for treatment at quarter end",
                          waiting_status == "admitted" ~ "Patients admitted for treatment during quarter",
                          TRUE ~ "")

  # Create annotations for graphs
  annotations = list(
    
    list(
      x = "52-65",
      y = 0.95,
      font = list(size = 12),
      text = paste("Change in time", "bands to 13 week", "lengths", sep ="\n"),
      xref = "x",
      yref = "paper",
      xanchor = "left",
      yanchor = "bottom",
      showarrow = FALSE,
      align = "left"
    )
  )
  
  #create facetted plot by specialty
  subplot(plot_list, nrows=n_specs, shareX = TRUE, titleY = TRUE) %>%
    layout(title=plot_title, margin = list(b = 10, t = 40),annotations = annotations) %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

}
