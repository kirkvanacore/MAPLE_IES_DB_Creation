#title: "DB_aggregation_2020_1113"
#author: "Jieun Lee"
#date: "11/13/2020"


# Sid: Only contains problem level aggregation


## ----global_options, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# set global chunk options...  
knitr::opts_chunk$set(comment     = "",
                      echo        = FALSE, 
                      comment     = FALSE,
                      warning     = FALSE, 
                      message     = FALSE)


## ---- echo=FALSE, results=FALSE, warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### ----- load package(s) ----- ###

library("tidyverse")    
library("forcats")
library("lubridate")    
library("readxl")
library("reshape2")
library("data.table")
library("dplyr")
library("readr")
library("psych")        
library("furniture")    
library("stargazer")    
library("pander")
library("writexl")

panderOptions('digits', 3)
panderOptions('round', 3)
panderOptions('keep.trailing.zeros', TRUE)

library("RSQLite")
### Connect to SQLite DB ####
# set db path
ies_research_con <- dbConnect(RSQLite::SQLite(), "ies_research schema/maple_ies_research.db")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Student roster
final_roster <- read.csv("02_data_source_files/roster_demographic_2022_06_16_N=4,343.csv")

# Data from the ASSISTments database
db_raw  <- read.csv("02_data_source_files/ies_dataset_2021_0527.csv") # cant find thsi

# Problem list 
db_problem_list <- read_excel("02_data_source_files/db_problem_list.xlsx")

# Data extracted from the DB devices
device_data <- read_excel("02_data_source_files/DB_DeviceData.xlsx")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Selecting necessary columns in the roster data 
final_roster <- final_roster %>% select('student_id', 'StuID', 'condition_assignment')

# Selecting students in the DB condition 
final_roster_DB <- final_roster[ which(final_roster$condition_assignment=='DragonBox'), ]


# Selecting necessary columns in the device data
device_data <- device_data %>% select('student_id', "Device Number",  "Final_Chapter_Tablet", "Final_Stage_Tablet", "Chapter Star count", "Final_Star_Count_Tablet", "Total_Problem_Device", "Notes" )


# Deleting unnecessary columns in the raw data 
db_raw  <- db_raw  %>% select(-'type', -'school_id', -'teacher_id', -'teacher', -'classroom', -'full_name', -'first_name', -'last_name', -'class_section', -'assignment_start', -'assignment_end', -'problem_log_id', -'student_xref', -'problem_set_id', -'alias')

# Selecting students in the DB condition 
db_raw <- db_raw[ which(db_raw$condition=="DragonBox"), ]

# Adding the problem number information to the raw data 
db_raw <- inner_join(db_raw, db_problem_list, by = "question_id")
db_raw <- db_raw %>% select(-"problem_id.y")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##### subsetting the data
db_as2 <- db_raw[ which(db_raw$assignment=='DRGNBX_AS2'), ]
db_as3 <- db_raw[ which(db_raw$assignment=='DRGNBX_AS3'), ]
db_as4 <- db_raw[ which(db_raw$assignment=='DRGNBX_AS4'), ]
db_as5 <- db_raw[ which(db_raw$assignment=='DRGNBX_AS5'), ]
db_as7 <- db_raw[ which(db_raw$assignment=='DRGNBX_AS7'), ]
db_as8 <- db_raw[ which(db_raw$assignment=='DRGNBX_AS8'), ]
db_as9 <- db_raw[ which(db_raw$assignment=='DRGNBX_AS9'), ]
db_as10 <- db_raw[ which(db_raw$assignment=='DRGNBX_AS10'), ]
db_as11 <- db_raw[ which(db_raw$assignment=='DRGNBX_AS11'), ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### assignment 2
db_as2_answer <- dcast(db_as2, 
                      student_id ~ problem_n_02, 
                      value.var = "answer_text", 
                      na.rm = TRUE)

db_as2_answer <- db_as2_answer %>% select(-"02_04_inst", -"02_05_inst", -"02_06_inst", -"02_12_inst", -"02_13_inst") 


### assignment 3
db_as3_answer <- dcast(db_as3, 
                      student_id ~ problem_n_03, 
                      value.var = "answer_text", 
                      na.rm = TRUE)

db_as3_answer <- db_as3_answer %>% select(-"03_04_inst", -"03_06_inst", -"03_11_inst", -"03_17_inst") 


### assignment 4
db_as4_answer <- dcast(db_as4, 
                      student_id ~ problem_n_04, 
                      value.var = "answer_text", 
                      na.rm = TRUE)

db_as4_answer <- db_as4_answer %>% select(-"04_04_inst",-"04_10_inst") 



### assignment 5
db_as5_answer <- dcast(db_as5, 
                      student_id ~ problem_n_05, 
                      value.var = "answer_text", 
                      na.rm = TRUE)

db_as5_answer <- db_as5_answer %>% select(-"05_04_inst",-"05_10_inst") 


### assignment 7
db_as7_answer <- dcast(db_as7, 
                      student_id ~ problem_n_07, 
                      value.var = "answer_text", 
                      na.rm = TRUE)

db_as7_answer <- db_as7_answer %>% select(-"07_04_inst",-"07_10_inst") 



### assignment 8
db_as8_answer <- dcast(db_as8, 
                      student_id ~ problem_n_08, 
                      value.var = "answer_text", 
                      na.rm = TRUE)

db_as8_answer <- db_as8_answer %>% select(-"08_04_inst",-"08_10_inst") 



### assignment 9
db_as9_answer <- dcast(db_as9, 
                      student_id ~ problem_n_09, 
                      value.var = "answer_text", 
                      na.rm = TRUE)

db_as9_answer <- db_as9_answer %>% select(-"09_04_inst",-"09_10_inst") 


### assignment 10
db_as10_answer <- dcast(db_as10, 
                      student_id ~ problem_n_10, 
                      value.var = "answer_text", 
                      na.rm = TRUE)

db_as10_answer <- db_as10_answer %>% select(-"10_04_inst",-"10_10_inst") 


### assignment 11
db_as11_answer <- dcast(db_as11, 
                      student_id ~ problem_n_11, 
                      value.var = "answer_text", 
                      na.rm = TRUE)

db_as11_answer <- db_as11_answer %>% select(-"11_04_inst",-"11_10_inst", -"11_16_inst") 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
db_problem_level <- final_roster_DB %>%
  full_join(db_as2_answer, by = "student_id") %>%
  full_join(db_as3_answer, by = "student_id") %>%
  full_join(db_as4_answer, by = "student_id") %>%
  full_join(db_as5_answer, by = "student_id") %>%
  full_join(db_as7_answer, by = "student_id") %>%
  full_join(db_as8_answer, by = "student_id") %>%
  full_join(db_as9_answer, by = "student_id") %>%
  full_join(db_as10_answer, by = "student_id") %>%
  full_join(db_as11_answer, by = "student_id") %>%
  full_join(device_data, by = "student_id")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#write_xlsx(x = db_problem_level, path = "DB_aggregation_merge_2022_0628.xlsx", col_names = TRUE)

###### SAVE fh2t_student FILE #######
write.csv(db_problem_level , "ies_research schema/dBox_problem.csv")

###### Write fh2t_student_problem table ######
if (dbExistsTable(ies_research_con, "dBox_problem"))
    dbRemoveTable(ies_research_con, "dBox_problem")
RSQLite::dbWriteTable(ies_research_con, "dBox_problem", db_problem_level, overwrite = T)

