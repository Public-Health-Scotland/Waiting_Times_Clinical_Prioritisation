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
source("functions/CP-functions.R")

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
perf <- perf_all %>% 
  filter(between(date, min_date, max_date), !specialty %in% exclusions) %>%
  complete(urgency, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0,
                       waited_waiting_over_52_weeks = 0,
                       waited_waiting_over_104_weeks = 0)) 


#monthly performance for CP DQ app  ----
perf_split <- perf %>% 
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
  mutate(`proportion_seen/on_list` = round(ifelse(`number_seen/on_list`!=0, 
                                                  100*`number_seen/on_list`/sum(`number_seen/on_list`, na.rm=T), 0), 1),
         y_max = sum(`number_seen/on_list`, na.rm=T)) %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
  mutate(y_max = roundUpNice(max(y_max))) #calculate max y for graph limits

perf_split_monthly <- perf_split %>%
  select(-c(y_max)) %>%
  pivot_longer(c(`number_seen/on_list`:`proportion_seen/on_list`), 
               names_to = "Indicator", values_to = "value")


#2.2.2 - Quarterly ---- 
perf_qtr_all <- read.xlsx(here::here("data", "Performance excl. Lothian Dental Quarterly Week Flags.xlsx"), 
                          sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) 

#data for CP DQ, up to latest month
perf_qtr <- perf_qtr_all %>% 
  filter(between(date, min_date, max_date), !specialty %in% exclusions) %>%
  filter(ifelse(ongoing_completed == "Ongoing", month(date) %in% c(3,6,9,12),
                ongoing_completed == "Completed")) %>%
  complete(urgency, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0,
                       waited_waiting_over_52_weeks = 0,
                       waited_waiting_over_104_weeks = 0))

#Save version for DQ shiny app ----
perf_qtr_split <- perf_qtr %>% 
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
  mutate(`proportion_seen/on_list` = round(ifelse(`number_seen/on_list`!=0, 
                                                  100*`number_seen/on_list`/sum(`number_seen/on_list`, na.rm=T), 0), 1),
         y_max = sum(`number_seen/on_list`, na.rm=T)) %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
  mutate(y_max = roundUpNice(max(y_max))) %>%  #calculate max y for graph limits
  select(-c(y_max)) %>% 
  pivot_longer(c(`number_seen/on_list`:`proportion_seen/on_list`), names_to = "Indicator", values_to = "value")


#2.3 - Distribution of wait ----

#DoW ongoing waits
dow_4wk_ongoing <- read.xlsx("data/Distribution of Waits 4 week bands.xlsx", sheet = "IPDC Clinical Prioritisation", detectDates = TRUE) %>%
  clean_names(use_make_names = FALSE) %>%
  filter(ongoing_completed=="Ongoing") %>%
  mutate(date= base::as.Date(date, format = "%d/%m/%Y"))

#DoW completed waits
dow_4wk_comp <- read.xlsx("data/Distribution of Waits 4 week bands.xlsx", sheet = "IPDC Clinical Prioritisation", detectDates = TRUE) %>%
  clean_names(use_make_names = FALSE) %>%
  filter(ongoing_completed=="Completed") %>%
  mutate(date= base::as.Date(date, format = "%Y-%m-%d"))

#bind completed and ongoing into a single df
dow_4wk_all <- rbind(dow_4wk_comp, dow_4wk_ongoing) %>%
  mutate(weeks = as.factor(ifelse(weeks != "Over 104 Weeks", substr(weeks, 1, 7), "Over 104")),
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty))

#dow 4 week bands data for CP DQ shiny, max date set to end of latest available month
dow_4wk <- dow_4wk_all %>% 
  filter(between(date, min_date, max_date), !specialty %in% exclusions) %>%
  complete(urgency, weeks, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0)) 

#quaterly 4 week bands dow data for CP DQ shiny
dow_4wk_qtr <- dow_4wk %>% 
  #keep last month of quarter for ongoing waits, all months for completed
  filter(ifelse(ongoing_completed == "Ongoing", month(date) %in% c(3,6,9,12), 
                ongoing_completed == "Completed")) %>% 
  #convert monthly dates to end of quarter dates
  mutate(date = as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>% 
  group_by(across(-`number_seen/on_list`)) %>% 
  #get the sum of waits/patients seen for each quarter
  summarise(`number_seen/on_list` = sum(`number_seen/on_list`)) 


#3.1 - Save Outputs ----
#monthly performance
saveRDS(perf_split_monthly, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/performance_monthly.RDS")
write.xlsx(perf_plit_monthly, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/performance_monthly.xlsx")

#quarterly performance
saveRDS(perf_qtr_split, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/performance_quarterly.RDS")
write.xlsx(perf_qtr_split, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/performance_quarterly.xlsx")

#DoW Monthly
saveRDS(dow_4wk, file="/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/dow_4wk_monthly.RDS")
write.xlsx(dow_4wk, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/dow_4wk_monthly.xlsx")

#DoW Quarterly
saveRDS(dow_4wk_qtr, file="/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/dow_4wk_quarterly.RDS")
write.xlsx(dow_4wk_qtr, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/dow_4wk_quarterly.xlsx")



