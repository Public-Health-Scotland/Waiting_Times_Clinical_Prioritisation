########################################################################
# Name of file - CP presentation code.R
# Data release - Stage of Treatment
# Original Authors - Caroline Thomson
# Orginal Date - October 2021
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Analysis of CP data for Peter Martin's presentation
#
# Approximate run time - xx minutes
#########################################################################

#### 1 - Packages and functions ----
# 1.1  - Load packages ----
# If any of the below packages don't run, install will be required using install.packages("")

library(tidyverse)     # For loading all "tidy" packages at once 
library(scales)
library(janitor)       # For 'cleaning' variable names
library(magrittr)      # For %<>% operator
library(lubridate)     # For dates
library(here)          # For the here() function
library(janitor)       # For 'cleaning' variable names
library(glue)          # For working with strings
library(fs)            # For creating new file directories
library(zoo)
library(openxlsx)      # For writing to Excel workbook
library(phsstyles)     # For easy use of PHS colours in charts

Sys.umask("002") # Used to ensure directory permissions are correct



### 2 - Define Whether Running on Server or Locally ----

if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)",
                                  "x86_64-pc-linux-gnu (64-bit)")) {
  platform <- "server"
} else {
  platform <- "locally"
}

# Define root directory for stats server based on whether script is running 
# locally or on server
filepath <- dplyr::if_else(platform == "server",
                           "/PHI_conf/WaitingTimes",
                           "//stats/WaitingTimes")

### 3 - Import data ----
# 3.1 - Load functions ----

source(glue::glue(filepath, "/SoT/Projects/CP MMI/R Code/CP-functions.R"))

# 3.2 - Define filepaths ----
CP_path <- glue::glue(filepath, "/SoT/Projects/CP MMI/BOXI Extracts/Completed and Ongoing.xlsx") 

CPlong_path <- glue::glue(filepath, "/SoT/Projects/CP MMI/BOXI Extracts/Completed and Ongoing 25 month.xlsx") 

#dow_path <- glue::glue(filepath, "/SoT/Projects/CP MMI/BOXI Extracts/CO DoW 104+.xlsx") #Don't need this one as it is available via the version with procedures!

codes_path <- glue::glue(filepath, "/Portfolio/IRs-PQs-FOIs/Completed 2021/IR2021-00649 (Re-run of IR2021-00503)/proc_lookup.csv")

proc_path <- glue::glue(filepath, "/SoT/Projects/CP MMI/BOXI Extracts/snapshot data/CO DoW 104+ procedure grouped specialties.csv") 


# 3.3 - Import data ----
#OPCS4 groupings lookup
codeslookup <- read_csv(codes_path)


CP_data <- read.xlsx(CP_path) %>%
  clean_names() %>%
  mutate(month = if_else(str_detect(month, "/"), as.Date(month, "%d/%m/%Y"), as.Date(as.numeric(month), origin = "1899-12-30")),
         urgency = case_when(
           urgency %in% c("Not Known", "Not Known", "Routine", "Urgent") ~ "Other",
           urgency %in% c("Priority 2A", "Priority 3A", "Priority 4A") ~ str_remove(urgency,"A"),
           urgency %in% c("Priority 1A", "Priority 1B") ~"Priority 1A-1B",
           TRUE ~urgency
         )
         ) %>%
  rename(board = nhs_board_of_treatment) %>%
  group_by(patient_type, ongoing_completed, board, specialty, month, urgency) %>%
  summarise(number_seen_on_list=sum(number_seen_on_list))

CPlong_data <- read.xlsx(CPlong_path) %>%
  clean_names() %>%
  mutate(month = if_else(str_detect(month, "/"), as.Date(month, "%d/%m/%Y"), as.Date(as.numeric(month), origin = "1899-12-30")),
         urgency = case_when(
           urgency %in% c("Not Known", "Not Known", "Routine", "Urgent") ~ "Other",
           urgency %in% c("Priority 2A", "Priority 3A", "Priority 4A") ~ str_remove(urgency,"A"),
           urgency %in% c("Priority 1A", "Priority 1B") ~"Priority 1A-1B",
           TRUE ~urgency
         )
  ) %>%
  rename(board = nhs_board_of_treatment) %>%
  group_by(patient_type, ongoing_completed, board, specialty, month, urgency) %>%
  summarise(number_seen_on_list=sum(number_seen_on_list)) %>%
  group_by(patient_type, ongoing_completed, board, specialty, month) %>%
  mutate(tot = sum(number_seen_on_list),
         yearmon = as.Date(format(month, "%Y-%m-01"))) %>%
  group_by(patient_type, ongoing_completed, board, specialty) %>%
  mutate(y_max=roundUpNice(max(tot))) #Calculate y_max to use in graphs
         
dow_data <- read.xlsx(dow_path) %>%
  clean_names() %>%
  mutate(month = if_else(str_detect(month, "/"), as.Date(month, "%d/%m/%Y"), as.Date(as.numeric(month), origin = "1899-12-30"))) %>%
  rename(board = nhs_board_of_treatment) %>%
  complete(wait_length, nesting(patient_type, ongoing_completed, board, specialty, month, urgency), fill = list(number_seen_on_list=0)) %>%
  mutate(urgency = case_when(
    urgency %in% c("Not Known", "Not Known", "Routine", "Urgent") ~ "Other",
    urgency %in% c("Priority 2A", "Priority 3A", "Priority 4A") ~ str_remove(urgency,"A"),
    urgency %in% c("Priority 1A", "Priority 1B") ~"Priority 1A-1B",
    TRUE ~urgency
  ),
  wait_length2 = str_remove(wait_length, "[[:punct:]]") %>% substr(1,3) %>% as.numeric() %>% as.factor()) %>%
  group_by(patient_type, ongoing_completed, board, specialty, month, urgency, wait_length, wait_length2) %>%
 summarise(number_seen_on_list=sum(number_seen_on_list)) %>%
  group_by(patient_type, ongoing_completed, board, specialty, month, wait_length, wait_length2) %>%
  mutate(wait_tot=sum(number_seen_on_list)) %>%
  group_by(patient_type, ongoing_completed, board, specialty, month) %>%
  mutate(y_max=roundUpNice(max(wait_tot))) #Calculate y_max to use in graphs


#Procedure data with all codes included 
allcodes_data <- read.csv(proc_path, stringsAsFactors = FALSE) %>%
  clean_names() %>%
  mutate(number_seen_on_list = as.numeric(number_seen_on_list), #Added as numbers came through as character in csv
         month = if_else(str_detect(month, "/"), as.Date(month, "%d/%m/%Y"), as.Date(as.numeric(month), origin = "1899-12-30")),
         urgency = case_when(
           urgency %in% c("Not Known", "Not Known", "Routine", "Urgent") ~ "Other",
           urgency %in% c("Priority 2A", "Priority 3A", "Priority 4A") ~ str_remove(urgency,"A"),
           urgency %in% c("Priority 1A", "Priority 1B") ~"Priority 1A-1B",
           TRUE ~urgency
         )
         ) %>%
  rename(board = nhs_board_of_treatment) %>%
  group_by(patient_type, ongoing_completed, board, specialty, month, urgency, x4d_proc, procedure_name, x3d_proc, wait_length) %>%
  summarise(number_seen_on_list = sum(number_seen_on_list)) %>%
  left_join(codeslookup, by = c("x4d_proc"="main_proc")) %>% #First join on 4D code as this is most specific
  left_join(codeslookup, by=c("x3d_proc"="main_proc"))  %>%  #Then join on 3D code
  mutate(description = ifelse(is.na(`description.x`), `description.y`, `description.x`), #If 3D-matched vales are NA, use 4D ones in case a more specific match was made
         proc_grp = ifelse(is.na(`proc_grp.x`), `proc_grp.y`, `proc_grp.x`),
         proc_subgrp = ifelse(is.na(`proc_subgrp.x`), `proc_subgrp.y`, `proc_subgrp.x`),
         across(.cols = description:proc_subgrp, #Fill all 3 new columns with "Blank" if 3D and 4D codes are NA
                .fns = ~if_else(is.na(`x3d_proc`) & is.na(`x4d_proc`), "Blank", .),
                .names = "{.col}")) %>%
  ungroup() %>%
  select(-contains(c(".","x"))) %>%
  select(c(patient_type:specialty, proc_grp, proc_subgrp, description, month:wait_length, number_seen_on_list))

proc_data <- allcodes_data %>%
  filter(proc_subgrp !="99")


#Write out procedure data
write.xlsx(allcodes_data, glue::glue(filepath, "/SoT/Projects/CP MMI/BOXI Extracts/CO DoW 104+ grouped procedure.xlsx") )
saveRDS(allcodes_data, glue::glue(filepath, "/SoT/Projects/CP MMI/BOXI Extracts/snapshot data/procleveldata.RDS"))        

#3.3 - Define reference lines ----
reflines  = data.frame(wait_length2 = c(4.5, 12.5))
reftext = data.frame(wait_length2 = c(3.5, 11.5, 13.5), y = c(Inf, Inf, Inf), CP= c("P2", "P3", "P4"), urgency = c("Other", "Other" ,"Other"))

#3.4 - Define colours for graphs
colourset = data.frame(codes = c("Other",
                                 "Priority 1A-1B",
                                 "Priority 2",
                                 "Priority 3",
                                 "Priority 4"),
                       colours = c("phs-purple",
                                   "phs-magenta",
                                   "phs-blue",
                                   "phs-green",
                                   "phs-graphite"))

###4 - Analyses ----

# 4.1 - Split by Board - completeness/activity ---

HB_summ <- CP_data %>%
  group_by(board, ongoing_completed, month, urgency) %>%
  summarise(total = sum(number_seen_on_list),
            complete = sum(number_seen_on_list[!urgency %in% c("Not Known", "Routine", "Urgent")])) %>%
  group_by(board, ongoing_completed, month) %>%
  summarise(total = sum(total),
            complete = sum(complete),
            percentage = 100*complete/total)

comp_plot <- HB_summ %>%
  ggplot(aes(x = month, y = percentage, group = ongoing_completed, colour = ongoing_completed, fill= ongoing_completed)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~board)

HB_summ2 <- CP_data %>%
  complete(nesting(patient_type, ongoing_completed, board,  specialty, month), urgency, fill = list(number_seen_on_list=0)) %>%
  group_by(board, ongoing_completed, month, urgency) %>%
  summarise(total = sum(number_seen_on_list),
            complete = sum(number_seen_on_list[!urgency %in% c("Not Known", "Routine", "Urgent")])) %>%
  pivot_wider(id_cols =c(board, ongoing_completed, urgency), names_from = month, values_from = total)

CP_plot <- CP_data %>% 
  filter(board=="NHS Scotland", ongoing_completed=="Ongoing", specialty=="All Specialties") %>% ggplot(aes(x=month, y = number_seen_on_list, group=urgency, fill=urgency)) + geom_bar(stat="identity")
  ggplot(aes(x = month, y = percentage, group = ongoing_completed, colour = ongoing_completed, fill= ongoing_completed)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~board)

# 4.2 - Top specialties ----
spec_summ <- CP_data %>%
  group_by(board, ongoing_completed, specialty, month, urgency) %>%
  summarise(total = sum(number_seen_on_list),
            complete = sum(number_seen_on_list[!urgency %in% c("Not Known", "Routine", "Urgent")])) %>%
  group_by(board, ongoing_completed, specialty, month) %>%
  summarise(total = sum(total),
            complete = sum(complete),
            percentage = 100*complete/total)


#4.3 - DoW by HB ----
filter(board=="NHS Scotland", specialty == "All Specialties", month == "2021-09-30", ongoing_completed=="Completed") %>%
  group_by(board, wait_length) %>%
  mutate(grptot = sum(number_seen_on_list),
         prop=number_seen_on_list/grptot) %>%
  group_by(ongoing_completed, wait_length, urgency) %>%
  ggplot(aes(x = wait_length2, y = prop, 
             group=factor(urgency, levels=c("Other","Priority 4", "Priority 3", "Priority 2")), 
             fill=factor(urgency, levels=c("Other","Priority 2", "Priority 3", "Priority 4")))) +
  geom_bar(stat="identity") +
  geom_vline(xintercept = "3")+
  geom_vline(xintercept = "12")+
  scale_fill_discrete_phs(palette = "main", name = "Urgency category") +
  scale_y_continuous(expand=c(0,0), labels = percent) +
  scale_x_discrete(breaks = seq(0,104,13), labels = c("00-01", "13-14", "26-27", "39-40", "52-53", "65-66", "78-79", "91-92", "104+")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  labs(x="Wait length (weeks)", y = "") 

#### 4.3.1 - All specialties ----

#NHS Scotland, ongoing waits
dow_scot_on <- plotdowper(dow_data, "All Specialties", "NHS Scotland", "2021-09-30", "Ongoing")

#High copmpleteness boards, ongoing waits
dow_hb_on <- plotdowfacet(dow_data, "All Specialties", c("NHS Ayrshire & Arran","NHS Fife","NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Lanarkshire", "NHS Lothian") , "2021-09-30", "Ongoing")

#NHS Scotland, completed waits
dow_scot_comp <- plotdowper(dow_data, "All Specialties", "NHS Scotland", "2021-09-30", "Completed")

#High copmpleteness boards, completed waits
dow_hb_comp <- plotdowfacet(dow_data, "All Specialties", c("NHS Ayrshire & Arran","NHS Fife","NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Lanarkshire", "NHS Lothian") , "2021-09-30", "Completed")

#### 4.3.2 - Orthopaedics ----

#NHS Scotland, ongoing waits
dow_scot_onTO <- plotdowper(dow_data, "TRAUMA AND ORTHOPAEDIC SURGERY", "NHS Scotland", "2021-09-30", "Ongoing")

#NHS Scotland, completed waits
dow_scot_compTO <- plotdowper(dow_data, "TRAUMA AND ORTHOPAEDIC SURGERY", "NHS Scotland", "2021-09-30", "Completed")

dow_scot_TO <- plotdowfacet_oncomp(dow_data, "TRAUMA AND ORTHOPAEDIC SURGERY", "NHS Scotland", "2021-09-30")

#### 4.3.3 - Urology ----

#NHS Scotland, ongoing waits
dow_scot_onU <- plotdowper(dow_data, "UROLOGY", "NHS Scotland", "2021-09-30", "Ongoing")

#NHS Scotland, completed waits
dow_scot_compU <- plotdowper(dow_data, "UROLOGY", "NHS Scotland", "2021-09-30", "Completed")

dow_scot_U <- plotdowfacet_oncomp(dow_data, "UROLOGY", "NHS Scotland", "2021-09-30")

##### 4.3.4 - Long term trends ----

#NHS Scotland, Ongoing 
CP_long_on <- trendplot(CPlong_data, "All Specialties", "NHS Scotland", "Ongoing")


#NHSScotland, Completed
CP_long_comp <- trendplot(CPlong_data, "All Specialties", "NHS Scotland", "Completed")


#4.4 - Procedure data ----
#Group by board, ongoing/completed, specialty, proc group, sort by number
proc_summ <- proc_data %>%
  mutate(urgency = case_when(
    urgency %in% c("Not Known", "Not Known", "Routine", "Urgent") ~ "Other",
    urgency %in% c("Priority 2A", "Priority 3A", "Priority 4A") ~ str_remove(urgency,"A"),
    urgency %in% c("Priority 1A", "Priority 1B") ~"Priority 1A-1B",
    TRUE ~urgency
  )
  ) %>%
  group_by(board, ongoing_completed, specialty, month, proc_grp, proc_subgrp, urgency) %>%
  summarise(patients = n()) %>%
  ungroup() %>%
  arrange(board, month, specialty, desc(patients))

#Top procedures by specialty and urgency - use ongoing waits as these are more complete
top_procs <- proc_data %>%
  mutate(urgency = case_when(
    urgency %in% c("Not Known", "Not Known", "Routine", "Urgent") ~ "Other",
    urgency %in% c("Priority 2A", "Priority 3A", "Priority 4A") ~ str_remove(urgency,"A"),
    urgency %in% c("Priority 1A", "Priority 1B") ~"Priority 1A-1B",
    TRUE ~urgency
  )
  )%>%
  group_by(board, ongoing_completed, specialty, month, urgency, proc_subgrp) %>%
  summarise(patients = n()) %>%
  ungroup() %>%
  complete(urgency, nesting(board, ongoing_completed, specialty, month, proc_subgrp), fill = list(patients=0)) %>%
  arrange(board, month, specialty, desc(patients)) %>%
  group_by(board, ongoing_completed, specialty, month, proc_subgrp) %>%
  mutate(total = sum(patients)) %>%
  arrange(board, month, specialty, desc(total)) %>%
  group_by(board, ongoing_completed, specialty, month) %>%
  mutate(r = dense_rank(-total)) %>%
  group_by(board, specialty, month, proc_subgrp) %>%
  mutate(r = ifelse(ongoing_completed=="Completed", r[ongoing_completed=="Ongoing"], r),
         ongoing_tot = ifelse(ongoing_completed=="Completed", total[ongoing_completed=="Ongoing"], total)) %>% #Replace r for completed waits with r for ongoing so that the same procedures are brought through to the graph
  group_by(board, ongoing_completed, specialty, month, proc_subgrp) %>%
  mutate(y_min = 0,
         y_max = roundUpNice(max(patients)), #Nice max value for pretty y axis limits
         y_max_plus = roundUpNice(max(patients))*1.1, #Increase y_max if data labels are added to the plot
         specialty = if_else(specialty=="TRAUMA AND ORTHOPAEDIC SURGERY", "ORTHOPAEDICS", specialty)
)


# 4.5 - Graphs ----
#4.5.1 - Top 6 procedures by CP and board ----

#Orthopaedics
TO_plot <- plotme(top_procs, "ORTHOPAEDICS", "NHS Scotland", "2021-09-30")

#Ophthalmology, completed
OP_plot <- plotme(top_procs, "OPHTHALMOLOGY", "NHS Scotland", "2021-09-30")

#Urology
Urology_plot <- plotme(top_procs, "UROLOGY", "NHS Scotland", "2021-09-30")

TO_Lan_plot <- plotme(top_procs, "ORTHOPAEDICS", "NHS Lanarkshire", "2021-09-30")


#Combined ortho urology plot
TO_U_plot <- plotdow_oncomp_spec(dow_data, c("TRAUMA AND ORTHOPAEDIC SURGERY", "UROLOGY"), "NHS Scotland", "2021-09-30")


#4.5.2 - Single procedures by CP and Board ----
#T&O - Total hip replacement
totalhip_plot <- plotmehb(top_procs, "ORTHOPAEDICS", c("NHS Ayrshire & Arran","NHS Fife","NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Lanarkshire", "NHS Lothian"), "2021-09-30", "Primary total hip replacement")

#Ophthalmology - cataracts
cataract_plot <- plotmehb(top_procs, "OPHTHALMOLOGY", c("NHS Ayrshire & Arran","NHS Fife","NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Lanarkshire", "NHS Lothian"), "2021-09-30", "Cataract Procedures")

#Urology - other urinary tract endoscopy 
#Other urinary tract endoscopy
urology_plot <- plotmehb(top_procs, "UROLOGY", c("NHS Ayrshire & Arran","NHS Fife","NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Lanarkshire", "NHS Lothian"), "2021-09-30", "Other urinary tract endoscopy")

#Other male genital organ procedures
mgo_plot <- plotmehb(top_procs, "UROLOGY", c("NHS Ayrshire & Arran","NHS Fife","NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Lanarkshire", "NHS Lothian"), "2021-09-30", "Other male genital organ procedures")
