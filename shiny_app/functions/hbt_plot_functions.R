####################### Health Board of Treatment Plot Functions #######################

## Faceted activity graph ----
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
                                                       p2_proportion, .desc=FALSE)) %>%
    arrange(desc(nhs_board_of_treatment))

  # Wrapping text on specialties for plotting
  dataset$nhs_board_of_treatment_wrapped <- purrr::map_chr(dataset$nhs_board_of_treatment, wrap_label)

  facets <- unique(dataset$indicator)

  # Check that there is enough data to be displayed, otherwise throw error to user
  # Checking there are 1 or more facets and the dataset is not entirely populated by zeros
  validate(
    need(((length(facets)>=1) & (unique(dataset$number) != 0)),
         "There are no entries matching your selection. Please choose again.")
  )

  plot_title <- case_when(waiting_status == "waiting" ~ "Patients waiting for treatment",
                           waiting_status == "admitted" ~ "Patients admitted for treatment",
                           waiting_status == "additions" ~ "Patients added to the waiting list",
                           TRUE ~ "")
   yaxis_plots[["tickfont"]] <- 14
   yaxis_plots[["categoryorder"]] <-"trace"
   xaxis_plots[["title"]] <- "Proportion of Patients (%)"
   # To stop plotly being stupid and rounding to whole number in tooltip on the sly
   # https://stackoverflow.com/questions/68007438/r-how-to-stop-rounding-percentages-to-0-decimal-places-on-plotly-chart
   xaxis_plots[["tickformat"]] <- ".1%"
   # Hack to move board names so they're not crammed up against the y axis
   # Adding invisible ticks which will force the board names left
   yaxis_plots[["ticks"]] <- "outside"
   yaxis_plots[["ticklen"]] <- 10
   yaxis_plots[["tickcolor"]] <- phs_colours("phs-liberty-10")



  p <- dataset %>%
    arrange(nhs_board_of_treatment, p2_proportion) %>%
    plot_ly(x = ~proportion,
            y = ~factor(nhs_board_of_treatment_wrapped),
            height = 600,
            type = "bar",
            orientation = 'h', #make bar chart horizontal
            color = ~urgency,
            colors = waiting_times_palette,
            # stroke = I("black"),
            marker = list(line = list(color = "black", width = 1)),
            legendgroup = ~urgency,
            # showlegend = (~unique(indicator) == "additions_to_list"),
            text= ~paste0("<b>Specialty</b>: ", specialty, "\n",
                          "<b>HBT</b>: ", nhs_board_of_treatment, "\n",
                          "<b>Quarter ending</b>: ", qend, "\n",
                          "<b>CP</b>: ", urgency, "\n",
                          "<b>Number of patients</b>: ", format(number, big.mark=','), "\n",
                          "<b>Percentage of patients</b>: ", paste0(100*round(proportion,3), '%'), "\n",
                          "<b>Total</b>: ", format(total, big.mark=',')),
            hovertemplate = "%{text}" ) %>%
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

  p %<>%  layout(margin = list(b = 80, t = 50), #to avoid labels getting cut out
                 yaxis = yaxis_plots, xaxis = xaxis_plots,
                 paper_bgcolor = phs_colours("phs-liberty-10"),
                 plot_bgcolor = phs_colours("phs-liberty-10"),
                 legend = list(x = 100, y = 0.5, title=list(text='Clinical Prioritisation')), #position of legend
                 barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(p)

}

## Faceted waits graph ----

# Makes hbt DoW plot
waits_hbt <- function(input_data, waiting_status,
                        qend="March 2022",
                        chosen_specialty="All Specialties",
                        hbt="NHS Scotland",
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

  # Check there aren't 0 waits in the categories
  # validate(
  #   need(!(unique(dataset$total)==0),
  #        "No patients in this selection.")
  # )


  yaxis_title <- hbt

  yaxis_plots[["title"]] <- yaxis_title
  xaxis_plots[["title"]] <- "Weeks waiting"

  tooltip_trend <- glue("<b>Specialty</b>: {dataset$specialty}<br>",
                        "<b>HBT</b>: {dataset$nhs_board_of_treatment}<br>",
                        "<b>Quarter ending</b>: {qend}<br>",
                        "<b>CP</b>: {dataset$urgency}<br>",
                        "<b>Weeks waiting</b>: {dataset$weeks}<br>",
                        "<b>Number of patients</b>: {format(dataset$`number_seen/on_list`, big.mark=',')}<br>",
                        "<b>Total</b>: {format(dataset$total, big.mark=',')}")


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
    # Layout
    layout(# to avoid labels getting cut out
      yaxis = yaxis_plots, xaxis = xaxis_plots,
      shapes = list(vline(12.5)),
      paper_bgcolor = phs_colours("phs-liberty-10"),
      plot_bgcolor = phs_colours("phs-liberty-10"),
      legend = list(x = 100, y = 0.5, title=list(text='Clinical Prioritisation')), # position of legend
      barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  
  return(p)

}

# Calls waits_hbt and wraps them in facetted view for chosen waiting_status
make_dow_hbt_suplots <- function(data, healthboards = c("NHS Scotland"),
                                  waiting_status, qend, spec){

  validate(
    need((length(healthboards)>=1),
         "There are no entries matching your selection. Please choose again.")
  )


  # Get health boards in order by descending P2 proportion
  hb_ordered <- data %>%
    group_by(across(c(-urgency, -`number_seen/on_list`, -weeks))) %>%
    mutate(total = sum(`number_seen/on_list`)) %>%
    ungroup %>% 
    filter(specialty == spec,
           date==get_short_date(qend),
           nhs_board_of_treatment %in% healthboards,
           total != 0) %>%
    select("nhs_board_of_treatment", "p2_proportion") %>%
    unique() %>%
    arrange(desc(p2_proportion)) %>%
    .$nhs_board_of_treatment

plot_list <- vector("list", length = length(hb_ordered)) #initialize empty list to store plots

  #create patients waiting DoW plots for each spec
  for(i in seq_along(hb_ordered)){

    if(i < length(hb_ordered)){
      hbt_plot <- waits_hbt(input_data = data,
                               waiting_status = waiting_status,
                               qend=qend,
                               chosen_specialty=spec,
                               hbt = hb_ordered[[i]])
    }

    else{ #add legend to last plot
      hbt_plot <- waits_hbt(input_data = data,
                               waiting_status = waiting_status,
                               qend=qend,
                               chosen_specialty=spec,
                               hbt = hb_ordered[[i]],
                               legend = TRUE)
    }

    # save plot
    if(is.null(hbt_plot)){
      plot_list[[i]] <- plotly_empty()
    }
    else{
      plot_list[[i]] <- hbt_plot
    }
  }

  plot_title <- case_when(waiting_status == "waiting" ~ "Patients waiting for treatment at quarter end",
                          waiting_status == "admitted" ~ "Patients admitted for treatment during quarter",
                          TRUE ~ "")
  # Create annotations for graphs
  annotations = make_annotation(y_choice=0.95)

  #create facetted plot by specialty
  subplot(plot_list, nrows=length(hb_ordered), shareX = TRUE, titleY = TRUE) %>%
    layout(title=plot_title, margin = list(b = 10, t = 40), annotations = annotations) %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

}


