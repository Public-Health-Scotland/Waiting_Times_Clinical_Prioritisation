#library(tidyverse)
#library(scales)
#library(janitor)
#library(fs)
#library(here)
#library(glue)
#library(openxlsx)
#library(zoo)

if (!require("pacman")) install.packages("pacman") #loads pacman or installs and then loads it if necessary

#Use pacman to load/install the following:
pacman::p_load(tidyverse, fs, readr, lubridate, janitor, magrittr, glue, zoo, scales, here, openxlsx)
