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