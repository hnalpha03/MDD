#=======================================================================================

# This script loads the prepossessed data from import_clean_transform#1 and performs exploratory analysis

#======================================================================================

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


#==================================================================================

merged_df<- read_excel("processed_data/analysis_data .xlsx")

# generate age at first diagnosis :

merged_df$diagnosis_age <- as.numeric(substr((merged_df$date_of_admission.x - merged_df$date_of_birth) / 365.25, start = 1,stop = 4))

merged_df <-as.data.frame(merged_df)
str(merged_df)


# summary characteristics of patients on current  treatment for antidepressants

prop_trt_adt  <- merged_df %>% 
  mutate(pop = n()) %>% filter(trt_adt == "Yes") %>%  
  group_by(trt_adt) %>% 
  group_by(pop, .add= "TRUE")%>%
  summarize(N= n(),.groups = 'drop') %>%
  mutate(
    freq = N / pop,
    pct = round((freq*100), 2))
prop_trt_adt 


ant_df <- merged_df %>% filter(trt_adt == "Yes") %>% select(-c(trt_con,trt_ssr,trt_the, trt_oth,trt_anx)) %>% sumtable(digits = 4, out = 'return')
write_xlsx(ant_df  ,"data_output/ant_df .xlsx")


# summary characteristics of patients on current treatment for anxiolytics

prop_trt_anx  <- merged_df %>% 
  mutate(pop = n()) %>% filter(trt_anx == "Yes") %>%  
  group_by(trt_anx) %>% 
  group_by(pop, .add= "TRUE")%>%
  summarize(N= n(),.groups = 'drop') %>%
  mutate(
    freq = N / pop,
    pct = round((freq*100), 2))
prop_trt_anx 

anx_df <- merged_df %>% filter(trt_anx == "Yes") %>% select(-c(trt_con,trt_ssr,trt_the, trt_oth,trt_adt)) %>% sumtable(digits = 4, out = 'return')
write_xlsx(anx_df  ,"data_output/anx_df .xlsx")


# summary characteristics of patients on current treatment for anticonvulsants

prop_trt_con  <- merged_df %>% 
  mutate(pop = n()) %>% filter(trt_con == "Yes") %>%  
  group_by(trt_con) %>% 
  group_by(pop, .add= "TRUE")%>%
  summarize(N= n(),.groups = 'drop') %>%
  mutate(
    freq = N / pop,
    pct = round((freq*100), 2))
prop_trt_con 

con_df <- merged_df %>% filter(trt_con == "Yes") %>% select(-c(trt_ssr,trt_anx,trt_the, trt_oth,trt_adt)) %>% sumtable(digits = 4, out = 'return')
write_xlsx(con_df  ,"data_output/con_df .xlsx")


# summary characteristics of patients on current treatment for  SSRI 

prop_trt_ssr  <- merged_df %>% 
  mutate(pop = n()) %>% filter(trt_ssr == "Yes") %>%  
  group_by(trt_ssr) %>% 
  group_by(pop, .add= "TRUE")%>%
  summarize(N= n(),.groups = 'drop') %>%
  mutate(
    freq = N / pop,
    pct = round((freq*100), 2))
prop_trt_ssr 

ssr_df <- merged_df %>% filter(trt_ssr == "Yes") %>% select(-c(trt_con,trt_anx,trt_the, trt_oth,trt_adt)) %>% sumtable(digits = 4, out = 'return')
write_xlsx(ssr_df  ,"data_output/ssr_df .xlsx")


# summary characteristics of patients on current treatment for  other psychiatric medications

prop_trt_oth  <- merged_df %>% 
  mutate(pop = n()) %>% filter(trt_oth == "Yes") %>%  
  group_by(trt_oth) %>% 
  group_by(pop, .add= "TRUE")%>%
  summarize(N= n(),.groups = 'drop') %>%
  mutate(
    freq = N / pop,
    pct = round((freq*100), 2))
prop_trt_oth 

oth_df <- merged_df %>% filter(trt_oth == "Yes") %>% select(-c(trt_con,trt_anx,trt_the, trt_ssr,trt_adt)) %>% sumtable(digits = 4, out = 'return')
write_xlsx(oth_df  ,"data_output/oth_df .xlsx")


# summary characteristics of patients on current treatment for   psychotherapy

prop_trt_the  <- merged_df %>% 
  mutate(pop = n()) %>% filter(trt_the == "Yes") %>%  
  group_by(trt_the) %>% 
  group_by(pop, .add= "TRUE")%>%
  summarize(N= n(),.groups = 'drop') %>%
  mutate(
    freq = N / pop,
    pct = round((freq*100), 2))
prop_trt_the 

the_df <- merged_df %>% filter(trt_the == "Yes") %>% select(-c(trt_con,trt_anx,trt_oth, trt_ssr,trt_adt)) %>% sumtable(digits = 4, out = 'return')
write_xlsx(the_df  ,"data_output/the_df .xlsx")


#=================================================================================================#

# visualizing the prevalence , adding confidence intervals to see it there is s statistically significant difference


merged_df <- merged_df %>%
  rename("Abnormal sleep patterns" =   symptom_1,
         "Anhedonia" =  symptom_2,
         "Poor appetite" =  symptom_3,
         "Depressed" =  symptom_4,
         "Suicidal thoughts" =  symptom_5) 
merged_df <- merged_df %>% 
  mutate(`Abnormal sleep patterns`= case_when( 
    `Abnormal sleep patterns`== "Yes"   ~ 1,TRUE ~ 0)) %>% 
  mutate(Anhedonia= case_when( 
    Anhedonia == "Yes"   ~ 1, TRUE ~ 0)) %>% 
  mutate(`Poor appetite`= case_when(`Poor appetite` == "Yes"   ~ 1,TRUE ~ 0)) %>% 
  mutate(`Depressed` = case_when(`Depressed` == "Yes"   ~ 1, TRUE ~ 0)) %>% 
  mutate(`Suicidal thoughts` = case_when(`Suicidal thoughts` == "Yes"   ~ 1, TRUE ~ 0))


# Exploring anti depressants

anti_depressant_df <- merged_df %>% filter(trt_adt == "Yes")
anti_depressant_df$group <- "Antidepressant"

#Denominator : count the total number of patients receiving antidepressants
denominator <- anti_depressant_df %>% 
  group_by(group) %>%
  summarise(patients = n())


# numerator : count the total number of patients with each symptom
disease2 <- anti_depressant_df %>%
  group_by(group)%>%
  summarise(
    `Abnormal sleep patterns` = sum(`Abnormal sleep patterns`),
    Anhedonia = sum(Anhedonia),
    `Poor appetite` = sum(`Poor appetite`),
    `Depressed` = sum(`Depressed`),
    `Suicidal thoughts` = sum(`Suicidal thoughts`))



# reshape to numerator
numerator <- tidyr::pivot_longer(
  disease2 
  , cols = c(-group)
  , names_to = "Symptoms"
  , values_to = "n_events"
)  

## Join Numerator and Denominator
tbl_group <- numerator %>%  left_join(denominator, by = "group")

## Calculate Proportions (crude rates)
perc <- phe_proportion(tbl_group, n_events, patients, confidence = 0.95, multiplier = 100)


dimension <- 'Source : EHR | MDD Patients'
x_label <-""
y_label <- paste0("Proportion")
caption <- ""
max(perc$value)
limits <- c(0,80)

fig1 <- ggplot(perc, aes(x=reorder(Symptoms,value), y=value))+
  geom_bar(stat = "identity",width = 0.7,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Proportion of patients currently on Antidepressant \nexperiencing different symptoms"),
       subtitle = paste0(dimension),
       x = x_label, 
       y = y_label,
       caption = caption) +
  scale_y_continuous(labels = function(x) paste0(x*1, "%"),limits=limits) +
  geom_text(aes(y= 5, label = paste0(format(round(as.numeric(value),1),nsmall=0.5),"%")), vjust = 0, colour = "black",size = 8,
            nudge_y = .9)+
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=27)) + coord_flip()

ggsave("fig_output/Antidepressant_symptoms.png",fig1,width = 15, height = 8)
fig1


#-------------------------------------------------------------------------------

# Exploring  anxiolytics 

anx_depressant_df <- merged_df %>% filter(trt_anx == "Yes")
anx_depressant_df$group <- "Antidepressant"

#Denominator : count the total number of patients receiving antidepressants
denominator1 <- anx_depressant_df %>% 
  group_by(group) %>%
  summarise(patients = n())


# numerator : count the total number of patients with eah symptom
disease21 <- anx_depressant_df %>%
  group_by(group)%>%
  summarise(
    `Abnormal sleep patterns` = sum(`Abnormal sleep patterns`),
    Anhedonia = sum(Anhedonia),
    `Poor appetite` = sum(`Poor appetite`),
    `Depressed` = sum(`Depressed`),
    `Suicidal thoughts` = sum(`Suicidal thoughts`))



# reshape to numerator
numerator1 <- tidyr::pivot_longer(
  disease21 
  , cols = c(-group)
  , names_to = "Symptoms"
  , values_to = "n_events"
)  

## Join Numerator and Denominator
tbl_group1 <- numerator1 %>%  left_join(denominator1, by = "group")

## Calculate Proportions (crude rates)
perc1 <- phe_proportion(tbl_group1, n_events, patients, confidence = 0.95, multiplier = 100)



fig11 <- ggplot(perc1, aes(x=reorder(Symptoms,value), y=value))+
  geom_bar(stat = "identity",width = 0.7,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Proportion of patients currently on Anxiolytics \nexperiencing different symptoms "),
       subtitle = paste0(dimension),
       x = x_label, 
       y = y_label,
       caption = caption) +
  scale_y_continuous(labels = function(x) paste0(x*1, "%"),limits=limits) +
  geom_text(aes(y= 5, label = paste0(format(round(as.numeric(value),1),nsmall=0.5),"%")), vjust = 0, colour = "black",size = 8,
            nudge_y = .9)+
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=27)) + coord_flip()

ggsave("fig_output/anxiolytics _symptoms.png",fig11,width = 17, height = 8)
fig11


#-------------------------------------------------------------------------------


# Exploring  anticonvulsants

con_depressant_df <- merged_df %>% filter(trt_con == "Yes")
con_depressant_df$group <- "Anticonvulsants"

#Denominator : count the total number of patients receiving antidepressants
denominator2 <- con_depressant_df %>% 
  group_by(group) %>%
  summarise(patients = n())


# numerator : count the total number of patients with eah symptom
disease22 <- con_depressant_df %>%
  group_by(group)%>%
  summarise(
    `Abnormal sleep patterns` = sum(`Abnormal sleep patterns`),
    Anhedonia = sum(Anhedonia),
    `Poor appetite` = sum(`Poor appetite`),
    `Depressed` = sum(`Depressed`),
    `Suicidal thoughts` = sum(`Suicidal thoughts`))

# CHD = sum(chd))

# reshape to numerator
numerator2 <- tidyr::pivot_longer(
  disease22 
  , cols = c(-group)
  , names_to = "Symptoms"
  , values_to = "n_events"
)  

## Join Numerator and Denominator
tbl_group2 <- numerator2 %>%  left_join(denominator2, by = "group")

## Calculate Proportions (crude rates)
perc2 <- phe_proportion(tbl_group2, n_events, patients, confidence = 0.95, multiplier = 100)


dimension1 <- 'Split by common Symptoms'

fig12 <- ggplot(perc2, aes(x=reorder(Symptoms,value), y=value))+
  geom_bar(stat = "identity",width = 0.7,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Proportion of patients currently on  Anticonvulsants \nexperiencing different symptoms"),
       subtitle = paste0(dimension),
       x = x_label, 
       y = y_label,
       caption = caption) +
  scale_y_continuous(labels = function(x) paste0(x*1, "%"),limits=limits) +
  geom_text(aes(y= 5, label = paste0(format(round(as.numeric(value),1),nsmall=0.5),"%")), vjust = 0, colour = "black",size = 8,
            nudge_y = .9)+
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=27)) + coord_flip()

ggsave("fig_output/anticonvulsants_symptoms.png",fig12,width = 17, height = 8)
fig12


#-------------------------------------------------------------------------------


# Exploring   SSRI
ssr_depressant_df <- merged_df %>% filter(trt_ssr == "Yes")
ssr_depressant_df$group <- "SSRI"

#Denominator : count the total number of patients receiving antidepressants
denominator3 <- ssr_depressant_df %>% 
  group_by(group) %>%
  summarise(patients = n())


# numerator : count the total number of patients with eah symptom
disease23 <- ssr_depressant_df %>%
  group_by(group)%>%
  summarise(
    `Abnormal sleep patterns` = sum(`Abnormal sleep patterns`),
    Anhedonia = sum(Anhedonia),
    `Poor appetite` = sum(`Poor appetite`),
    `Depressed` = sum(`Depressed`),
    `Suicidal thoughts` = sum(`Suicidal thoughts`))

# CHD = sum(chd))

# reshape to numerator
numerator3 <- tidyr::pivot_longer(
  disease23 
  , cols = c(-group)
  , names_to = "Symptoms"
  , values_to = "n_events"
)  

## Join Numerator and Denominator
tbl_group3 <- numerator3 %>%  left_join(denominator3, by = "group")

## Calculate Proportions (crude rates)
perc3 <- phe_proportion(tbl_group3, n_events, patients, confidence = 0.95, multiplier = 100)




fig13 <- ggplot(perc3, aes(x=reorder(Symptoms,value), y=value))+
  geom_bar(stat = "identity",width = 0.7,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Proportion of patients currently on SSRI \nexperiencing different symptoms"),
       subtitle = paste0(dimension),
       x = x_label, 
       y = y_label,
       caption = caption) +
  scale_y_continuous(labels = function(x) paste0(x*1, "%"),limits=limits) +
  geom_text(aes(y= 5, label = paste0(format(round(as.numeric(value),1),nsmall=0.5),"%")), vjust = 0, colour = "black",size = 8,
            nudge_y = .9)+
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=27)) + coord_flip()

ggsave("fig_output/SSRI_symptoms.png",fig13,width = 17, height = 8)
fig13



#-------------------------------------------------------------------------------


# Exploring   other psychiatric medications
oth_depressant_df <- merged_df %>% filter(trt_oth == "Yes")
oth_depressant_df$group <- "oth"

#Denominator : count the total number of patients receiving antidepressants
denominator4 <- oth_depressant_df %>% 
  group_by(group) %>%
  summarise(patients = n())


# numerator : count the total number of patients with eah symptom
disease24 <- oth_depressant_df %>%
  group_by(group)%>%
  summarise(
    `Abnormal sleep patterns` = sum(`Abnormal sleep patterns`),
    Anhedonia = sum(Anhedonia),
    `Poor appetite` = sum(`Poor appetite`),
    `Depressed` = sum(`Depressed`),
    `Suicidal thoughts` = sum(`Suicidal thoughts`))


# reshape to numerator
numerator4 <- tidyr::pivot_longer(
  disease24 
  , cols = c(-group)
  , names_to = "Symptoms"
  , values_to = "n_events"
)  

## Join Numerator and Denominator
tbl_group4 <- numerator4 %>%  left_join(denominator4, by = "group")

## Calculate Proportions (crude rates)
perc4 <- phe_proportion(tbl_group4, n_events, patients, confidence = 0.95, multiplier = 100)



fig14 <- ggplot(perc4, aes(x=reorder(Symptoms,value), y=value))+
  geom_bar(stat = "identity",width = 0.7,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Proportion of patients currently on other psychiatric medications \nexperiencing different symptoms"),
       subtitle = paste0(dimension),
       x = x_label, 
       y = y_label,
       caption = caption) +
  scale_y_continuous(labels = function(x) paste0(x*1, "%"),limits=limits) +
  geom_text(aes(y= 5, label = paste0(format(round(as.numeric(value),1),nsmall=0.5),"%")), vjust = 0, colour = "black",size = 8,
            nudge_y = .9)+
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=27)) + coord_flip()

ggsave("fig_output/oth_symptoms.png",fig14,width = 17, height = 8)
fig14


#------------------------------------------------------------------------------

# Exploring    psychotherapy
the_depressant_df <- merged_df %>% filter(trt_the == "Yes")
the_depressant_df$group <- "the"

#Denominator : count the total number of patients receiving antidepressants
denominator5 <- the_depressant_df %>% 
  group_by(group) %>%
  summarise(patients = n())


# numerator : count the total number of patients with eah symptom
disease25 <- the_depressant_df %>%
  group_by(group)%>%
  summarise(
    `Abnormal sleep patterns` = sum(`Abnormal sleep patterns`),
    Anhedonia = sum(Anhedonia),
    `Poor appetite` = sum(`Poor appetite`),
    `Depressed` = sum(`Depressed`),
    `Suicidal thoughts` = sum(`Suicidal thoughts`))



# reshape to numerator
numerator5 <- tidyr::pivot_longer(
  disease25 
  , cols = c(-group)
  , names_to = "Symptoms"
  , values_to = "n_events"
)  

## Join Numerator and Denominator
tbl_group5 <- numerator5 %>%  left_join(denominator5, by = "group")

## Calculate Proportions (crude rates)
perc5 <- phe_proportion(tbl_group5, n_events, patients, confidence = 0.95, multiplier = 100)



fig15 <- ggplot(perc5, aes(x=reorder(Symptoms,value), y=value))+
  geom_bar(stat = "identity",width = 0.7,fill = "cornflowerblue")+
  geom_errorbar(aes(ymin=lowercl, ymax=uppercl), width=.2)+
  labs(title = paste0("Proportion of patients currently on psychotherapy treatment \nexperiencing different symptoms"),
       subtitle = paste0(dimension1),
       x = x_label, 
       y = y_label,
       caption = caption) +
  scale_y_continuous(labels = function(x) paste0(x*1, "%"),limits=limits) +
  geom_text(aes(y= 5, label = paste0(format(round(as.numeric(value),1),nsmall=0.5),"%")), vjust = 0, colour = "black",size = 8,
            nudge_y = .9)+
  theme_bw()+
  theme(axis.text.y =  element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x =  element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title=element_text(size=27)) + coord_flip()

ggsave("fig_output/psychotherapy_symptoms.png",fig15,width = 17, height = 8)
fig15


#========================================================================================

# the prevalence of depression is highest for each treatment option
# are there any factors associated to that?
#=====================================================================================

