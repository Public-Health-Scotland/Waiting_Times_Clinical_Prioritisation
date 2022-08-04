########################################################################
# Name of file - CP-functions.R
# Data release - Stage of Treatment
# Original Authors - Caroline Thomson
# Orginal Date - October 2021
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Functions used in CP analysis code
#
# Approximate run time - NA
#########################################################################

#1 - Function to round values up for creating upper limits for y axes ----
roundUpNice <- function(x, nice_small=c(1,2,3,4,5,6,8,10),
                        nice_mid=c(1,1.5,2,3,4,5,6,8,10), 
                        nice_big=c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  if (abs(x) > 1000) {
    nice = nice_big
  } else if (abs(x) < 10){
    nice = nice_small
  } else {
    nice = nice_mid
  }
  
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

#2 - Function to highlight particular axis labels (e.g. NHS Scotland) ----
highlight = function(x, pat, color="black", family="") {
  ifelse(grepl(pat, x), glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
}

#Example usage:
# scale_x_discrete(labels= function(x) highlight(x, "NHS Scotland", "black")) + ...

#3 - Function to modify gg_x_repel to be aware of other repel layers ----
# Wrapper of ggrepel::geom_text_repel
geom_text_repel2 <- function(...) {
  layer <- ggrepel::geom_text_repel(...)
  layer$ggrepel <- TRUE
  class(layer) <- c("ggrepel", class(layer))
  return(layer)
}

ggplot_add.ggrepel <- function(object, plot, object_name) {
  if (any(do.call(c, lapply(plot$layer, function(x) x$ggrepel)))) {
    warning(
      "There is more than one ggrepel layers. ",
      "This may cause overlap of labels"
    )
  }
  # Optionally, one may modify `object` here.
  NextMethod("ggplot_add")
}


# 4. ----------------------------------------------------------------------


trendbar <- function(data, spec, hb)
{
  data %>% filter(specialty==spec,
                  nhs_board_of_treatment==hb
  ) %>%
    ggplot(aes(x =floor_date(date, "month"), y = `number_seen/on_list`), group = ongoing_completed) +
    geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity") +
    theme_bw() +
    scale_x_date(labels = date_format("%b %y"),
                 breaks = seq(from = floor_date(min(addrem$date), "month"), 
                              to = floor_date(max(addrem$date), "month"), by = "1 months")) +
    scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name="")+
    scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name ="") +
    geom_blank(aes(y = y_max)) +
    facet_wrap(~ongoing_completed, nrow = 2, scales = "free_y",  strip.position = "top", 
               labeller = as_labeller(c(Ongoing = "Patients waiting", Completed = "Patients seen") )) +
    ylab(NULL) +
    xlab("Month ending") +
    theme(text = element_text(size = 12),
          strip.background = element_blank(),
          strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
          panel.spacing = unit(1, "cm"),
          panel.border = element_blank(),
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.x = element_blank(),
          legend.position="bottom")
}


# 5. Function to highlight particular axis labels (e.g. NHS Scotland)  -----

highlight = function(x, pat, color="black", family="") {
  ifelse(grepl(pat, x), glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
}
