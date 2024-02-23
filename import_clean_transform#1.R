# Project Title: Technical Interview for Holmusk
# Name: Henry Nanji
# Aim. The purpose of this script is to load the necessary libraries, create necessary directories
# import and integrate the data
#=========================================================================
# Creating directories for outputs
dir.create("data")
dir.create("processed_data")
dir.create("data_output")
dir.create("fig_output")
dir.create("report_output")
#==========================================================================

# Setting up environment

if (!require("readxl")){ install.packages("readxl")}
if (!require("dplyr")){ install.packages("dplyr")}
if (!require("tidyverse")){ install.packages("tidyverse")}
if (!require("caret")){ install.packages("caret")}
if (!require("gtsummary")){ install.packages("gtsummary")}
if (!require("ggplot2")){ install.packages("ggplot2")}
if (!require("lmtest")){ install.packages("lmtest")}
if (!require("stringr")){ install.packages("stringr")}
if (!require("PHEindicatormethods")){ install.packages("PHEindicatormethods")}
if (!require("summarytools")){ install.packages("summarytools")}
if (!require("lubridate")){ install.packages("lubridate")}
if (!require("finalfit")){ install.packages("finalfit")}
if (!require("writexl")){ install.packages("writexl")}
if (!require("stargazer")){ install.packages("stargazer")}
if (!require("patchwork")){ install.packages("patchwork")}
if (!require("vtable")){ install.packages("vtable")}


# Loading Libraries
library(readr)
library(dplyr)
library(tidyverse)
library(caret)
library(gtsummary)
library(ggplot2)
library(lmtest)
library(stringr)
library(PHEindicatormethods)
library(summarytools)
library(lubridate)
library(finalfit)
library(writexl)
library(stargazer)
library(gtsummary)
library(patchwork)
library(vtable)


#===============================================================================
# Load data set in R
#==============================================================================
demographics <- read_csv("data/demographics.csv")

clinical_data <- read_csv("data/clinical_data.csv")

bill_id <- read_csv("data/bill_id.csv")

bill_amount <- read_csv("data/bill_amount.csv")

#=============================================================================
#data preparation
#==============================================================================

# Demographics data set

str(demographics)

# List of character column names
character_columns <- c("gender", "race", "resident_status")

# Function to tabulate character variables
tabulate_character <- function(column) {
  table(demographics[[column]])
}

# Tabulate values for each character column
tabulated_values <- lapply(character_columns, tabulate_character)
print(tabulated_values)

# recode variables

demographics <-demographics%>%
  mutate(gender = case_when( 
    gender == "f" | gender == "Female"  ~  "Female",
    gender == "m" | gender =="Male" ~ "Male" )) %>% 
  mutate(race = case_when( 
    race == "chinese" | race == "Chinese"  ~  "Chinese",
    race == "India" | race =="Indian" ~ "Indian" ,
    race == "Malay"  ~ "Malay" ,
    race == "Others" ~ "Others" )) %>%
  mutate( resident_status = case_when( 
    resident_status == "Foreigner" ~  "Foreigner",
    resident_status  == "PR"  ~ "Permanent Resident",
    resident_status  == "Singapore citizen" | resident_status =="Singaporean"  ~ "Citizen"))


# clinical data

str(clinical_data)

# Check for NA values in each variable
na_check <- sapply(clinical_data, function(x) anyNA(x))
print(na_check)

# coding variables based on data dictionary
clinical_data <- clinical_data %>% 
  mutate(
    date_of_admission = dmy(date_of_admission),
    date_of_discharge = dmy(date_of_discharge)) %>% 
  mutate(medical_history_dia  = case_when( 
    medical_history_dia  == 1 ~  "Yes",
    TRUE ~ "No" )) %>% 
  mutate(medical_history_sud = case_when( 
    medical_history_sud   == 1 ~  "Yes",
    medical_history_sud == 0 ~ "No",
    TRUE ~ "Missing" )) %>% 
  mutate(medical_history_hbp = case_when( 
    medical_history_hbp    == 1 ~  "Yes",
    TRUE ~ "No" )) %>% 
  mutate(medical_history_ren = case_when( 
    medical_history_ren    == 1 ~  "Yes",
    TRUE ~ "No" )) %>% 
  mutate(medical_history_tum= case_when( 
    medical_history_tum    == 1 ~  "Yes",
    medical_history_tum == 0 ~ "No",
    TRUE ~ "Missing" )) %>% 
  mutate(medical_history_anx= case_when( 
    medical_history_anx    == 1 ~  "Yes",
    TRUE ~ "No" ))%>% 
  mutate(medical_history_mood = case_when( 
    medical_history_mood    == 1 ~  "Yes",
    TRUE ~ "No" )) %>% 
  mutate(trt_anx= case_when( 
    trt_anx    == 1 ~  "Yes",
    TRUE ~ "No" )) %>% 
  mutate(trt_con= case_when( 
    trt_con    == 1 ~  "Yes",
    TRUE ~ "No" )) %>% 
  mutate(trt_adt= case_when( 
    trt_adt    == 1 ~  "Yes",
    TRUE ~ "No" )) %>% 
  mutate(trt_ssr= case_when( 
    trt_ssr    == 1 ~  "Yes",
    TRUE ~ "No" ))  %>% 
  mutate(trt_the= case_when( 
    trt_the    == 1 ~  "Yes",
    TRUE ~ "No" )) %>% 
  mutate(trt_oth= case_when( 
    trt_oth    == 1 ~  "Yes",
    TRUE ~ "No" ))  %>% 
  mutate(symptom_1= case_when( 
    symptom_1    == 1 ~  "Yes",
    TRUE ~ "No" ))  %>% 
  mutate(symptom_2= case_when( 
    symptom_2    == 1 ~  "Yes",
    TRUE ~ "No" ))  %>% 
  mutate(symptom_3= case_when( 
    symptom_3    == 1 ~  "Yes",
    TRUE ~ "No" ))  %>% 
  mutate(symptom_4= case_when( 
    symptom_4    == 1 ~  "Yes",
    TRUE ~ "No" ))  %>% 
  mutate(symptom_5= case_when( 
    symptom_5    == 1 ~  "Yes",
    TRUE ~ "No" ))



# exploring multiple admission dates
# creatingf a flag to identify patients with first admission date
clinical_data <- clinical_data %>%
  group_by(id) %>%
  mutate(flag_1st_admission.date = as.integer(date_of_admission == min(date_of_admission))) %>%
  ungroup()

table(clinical_data$flag_1st_admission.date)

# patients were admitted and discharged later with variable lengths of stay
# some patients had just one admission and some had more than one admission
#  A choice is made to retain and analyse only records for patients based on their first admission date

clinical_data <- clinical_data %>% filter(flag_1st_admission.date == 1)

# check for duplicates based on patient id

clinical_data$tag <-as.numeric(duplicated(clinical_data$id))
table(clinical_data$tag)



# check duplicates id in demographics data
demographics$tag <-as.numeric(duplicated(demographics$patient_id))
table(demographics$tag)  # nd duplicates


# exploring Bill_id data set

bill_id$dup_id <-as.numeric(duplicated(bill_id$patient_id))
bill_id <-bill_id %>% arrange(patient_id)
table(bill_id$dup_id)

# creating a flag for first admission date in bill data and retainning 
# records for first admission date

bill_id <- bill_id %>%
  group_by(patient_id) %>%
  mutate(flag_1st_admission.date = as.integer(date_of_admission == min(date_of_admission))) %>%
  ungroup()
bill_id <-bill_id %>% arrange(patient_id)


# merging the bill_id data set and bill_amount

billing <-  bill_amount %>% 
  left_join(bill_id, by = c("bill_id"))
billing<- billing %>%
  group_by(patient_id) %>%
  mutate(flag_1st_admission.date = as.integer(date_of_admission == min(date_of_admission))) %>%
  ungroup()
billing <-billing %>% arrange(patient_id)

# patients had multiple bills generated when admitted into hospital
#  cost of hospital spell can only increase with increased length of stay
# a choice is made to retain records with highest cost for each patient

# Add indicator variable for max value for hospital cost

billing <- billing %>% filter(flag_1st_admission.date ==1) %>% 
  group_by(patient_id) %>%
  mutate(flag_max_cost = as.integer(amount == max(amount))) %>%
  ungroup()

billing <-billing %>% arrange(patient_id)

billing <-billing %>% filter(flag_max_cost  == 1) # retain bills with max cost for each patient

billing$tag <-as.numeric(duplicated(billing$patient_id))
table(billing$tag)


#merging demographics data, clinical data and billing data set based on patient_id

# renaming patient unique identifier for demographic and clinical data set with the same col names

clinical_data <- clinical_data %>% rename(patient_id = id)


merged_df <-  clinical_data %>% 
  left_join(demographics, by = c("patient_id")) %>% 
  left_join(billing, by = c("patient_id"))


# dropping all flags
merged_df <- merged_df %>% select(-c(tag, flag_max_cost, flag_1st_admission.date.y, dup_id, tag.y , tag.x ,flag_1st_admission.date.x,bill_id ))

# save the processed data set

write_xlsx(merged_df  ,"processed_data/analysis_data .xlsx")

