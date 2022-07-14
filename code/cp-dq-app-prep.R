########################################################################
# Name of file - cp-dq-app-prep.R
# Data release - Stage of Treatment
# Original Author - Maiana Sanjuan
# Orginal Date - July 2022
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Data wrangling for the CP data quality shiny app
#
# Approximate run time - xx minutes
#########################################################################


#### 1 - Packages and functions ----
#1.1 - Load packages ----
if (!require("pacman")) install.packages("pacman") #loads pacman or installs and then loads it if necessary

#Use pacman to load/install the following:
pacman::p_load(tidyverse, fs, readr, lubridate, janitor, magrittr, glue, zoo, scales, here, openxlsx, phsverse, rio, foreign)

min_date <- as.Date("2021-07-30")
max_date <- as.Date("2022-06-30")

#### 2 - Import data ----

#2.1 - Specialty exclusions ----
#Use the latest specialty exclusions list from the publication folder

exclusions_path <- here::here("..", "..", "..", #Takes us back up to SoT folder
                              "Publications",
                              "Inpatient, Day case and Outpatient Stage of Treatment Waiting Times",
                              "Publication R Script", 
                              "Publication", 
                              "Data", 
                              "Spec Exclusions.xlsx")

exclusions <- read.xlsx(exclusions_path, sheet = "IPDC") %>%
  as.list(Specialties)


#2.2 - Performance ----
#Read in the BOXI publication output, reformat dates and select correct specialties

#2.2.1 - Monthly ---- 

#monthly ipdc wt data
perf_all <- read.xlsx(here::here("data", "Performance excl. Lothian Dental Monthly Week Flags.xlsx"),
                      sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>% #Rename T&O as orthopaedics
  rename("waited_waiting_over_52_weeks"="waited_waiting_over_54_weeks") #temp fix for typo

#Read in live CO data (for shiny app) to get 2019 all specs averages
perf_2019 <- import_list("/PHI_conf/WaitingTimes/SoT/Projects/R Shiny DQ/Live BOXI/CO Monthly.xlsx", rbind =  TRUE) %>%
  select(- `_file`) %>%
  filter(year(Date) =="2019", 
         `NHS Board of Treatment` == "NHS Scotland",
         Specialty == "All Specialties",
         `Patient Type` == "Inpatient/Day case") %>%
  group_by(`Ongoing/Completed`) %>%
  summarise(monthly_avg = round(mean(`Number Seen/On list`),0)) %>%
  rename(Indicator = `Ongoing/Completed`)

#monthly data for cp code data quality investigation, sept 2021 to latest month
perf2 <- perf_all %>% 
  filter(between(date, min_date, max_date), !specialty %in% exclusions) %>%
  complete(urgency, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0,
                       waited_waiting_over_52_weeks = 0,
                       waited_waiting_over_104_weeks = 0)) 


#Save version for DQ shiny app ----
perf_split2 <- perf2 %>% 
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
  mutate(`proportion_seen/on_list` = round(ifelse(`number_seen/on_list`!=0, 
                                                  100*`number_seen/on_list`/sum(`number_seen/on_list`, na.rm=T), 0), 1),
         y_max = sum(`number_seen/on_list`, na.rm=T)) %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
  mutate(y_max = roundUpNice(max(y_max))) #calculate max y for graph limits

perf_split_monthly <- perf_split2 %>%
  select(-c(y_max)) %>%
  pivot_longer(c(`number_seen/on_list`:`proportion_seen/on_list`), 
               names_to = "Indicator", values_to = "value")
#check for NAs in value column for indicators (excl. median and percentiles)
# sum(is.na(perf_split_monthly[!(perf_split_monthly$Indicator %in% c("median", 	
#                                                        "90th_percentile")),8]))

saveRDS(perf_split_monthly, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/performance_monthly.RDS")
write.xlsx(perf_split_monthly, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/performance_monthly.xlsx")


