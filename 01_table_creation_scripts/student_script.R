library(dplyr)
library(mice)


  
  # version history 
  
# set global chunk options...  
knitr::opts_chunk$set(comment     = "",
                      echo        = TRUE, 
                      comment     = FALSE,
                      warning     = FALSE, 
                      message     = FALSE)


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
library("gridExtra")
panderOptions('digits', 3)
panderOptions('round', 3)
panderOptions('keep.trailing.zeros', TRUE)


### Reading in the data files 

# original demographic data from Craig
demo_data <- read.csv("02_data_source_files/2. Source Files for Demographic data/Demo Data 20210124.csv")
# roster data at the beginning of the study 
roster_01 <- read_excel("02_data_source_files/2. Source Files for Demographic data/roster_2020_09_07.xlsx")
# roster data at the end of the study 
roster_02 <- read_excel("02_data_source_files/2. Source Files for Demographic data/finalrosters_allstudents_2021_04_25_updated.xlsx")
teacher_id <- read_excel("02_data_source_files/2. Source Files for Demographic data/IES_school_teacher_ID_list.xlsx")
# performance level and scores
performance <- read_excel("02_data_source_files/2. Source Files for Demographic data/Gr 7 milestones 20-21 De-ID.xlsx")
math <- read_excel("02_data_source_files/2. Source Files for Demographic data/7th grade math final course grades 20-21.xlsx")


### replacing teacher name with teacher IDs

#merging teacher id with school id 
roster_01$teacher_code <- paste(roster_01$`school ID`, roster_01$`Teacher Name`, roster_01$SectionNumber)
teacher_id$teacher_code <- paste(teacher_id$school_ID, teacher_id$`Teacher Name`, teacher_id$Section)
roster_01 <- roster_01 %>%
  left_join(teacher_id, by = "teacher_code")
roster_01 <- roster_01 %>% select("student_number", "school ID", "SectionNumber", Student_ID, `Condition Assignment`, teacher_id, teacher_class)
col_order <- c("student_number", "Student_ID", "school ID", "teacher_id", "teacher_class", "SectionNumber", "Condition Assignment")
roster_01 <- roster_01[, col_order]




roster_02$teacher_code <- paste(roster_02$`school ID`, roster_02$`Teacher Name`, roster_02$SectionNumber)
roster_02 <- roster_02 %>%
  left_join(teacher_id, by = "teacher_code")
roster_02 <- roster_02 %>% select("student_number", "school ID", "SectionNumber", Student_ID, `Condition Assignment`, teacher_id, teacher_class, courseName)
col_order_2 <- c("student_number", "Student_ID", "school ID", "teacher_id", "teacher_class", "SectionNumber", "courseName", "Condition Assignment")
roster_02 <- roster_02[, col_order_2]


### change the type of the variables


demo_data$student_number <- as.character(as.numeric(demo_data$student_number))
roster_01$student_number <- as.character(as.numeric(roster_01$student_number))
roster_02$student_number <- as.character(as.numeric(roster_02$student_number))



###### Gender information ######

# gender data from the district 
DMS_gender <- read_xlsx("02_data_source_files/2. Source Files for Demographic data/enrollmentStatus/enrollmentStatus DMS.csv.xlsx")
FVA_gender <- read_xlsx("02_data_source_files/2. Source Files for Demographic data/enrollmentStatus/enrollmentStatus FVA.csv.xlsx")
LakMS_gender <- read_xlsx("02_data_source_files/2. Source Files for Demographic data/enrollmentStatus/enrollmentStatus LakMS.csv.xlsx")
LibMS_gender <- read_xlsx("02_data_source_files/2. Source Files for Demographic data/enrollmentStatus/enrollmentStatus LibMS.csv.xlsx")
LMMS_gender <- read_xlsx("02_data_source_files/2. Source Files for Demographic data/enrollmentStatus/enrollmentStatus LMMS.csv.xlsx")
NFMS_gender <- read_xlsx("02_data_source_files/2. Source Files for Demographic data/enrollmentStatus/enrollmentStatus NFMS.csv.xlsx")
OMS_gender <- read_xlsx("02_data_source_files/2. Source Files for Demographic data/enrollmentStatus/enrollmentStatus OMS.csv.xlsx")
PGMS_gender <- read_xlsx("02_data_source_files/2. Source Files for Demographic data/enrollmentStatus/enrollmentStatus PGMS.csv.xlsx")
RMS_gender <- read_xlsx("02_data_source_files/2. Source Files for Demographic data/enrollmentStatus/enrollmentStatus RMS.csv.xlsx")
SFMS_gender <- read_xlsx("02_data_source_files/2. Source Files for Demographic data/enrollmentStatus/enrollmentStatus SFMS.csv.xlsx")
VCMS_gender <- read_xlsx("02_data_source_files/2. Source Files for Demographic data/enrollmentStatus/enrollmentStatus VCMS.csv.xlsx")


# cleaning the gender data file 

# merging the data files 
gender_all <- rbind(DMS_gender, FVA_gender, LakMS_gender, LibMS_gender, LMMS_gender, NFMS_gender, OMS_gender, PGMS_gender, RMS_gender, SFMS_gender, VCMS_gender)
# drop the duplicates
gender <- gender_all %>% distinct(Number, .keep_all = TRUE)
# drop the unnecessary columns 
gender <- gender %>% select(Number, Gender)
# change the column name and the type 
names(gender)[names(gender) == "Number"] <- "student_number"
gender$student_number <- as.character(as.numeric(gender$student_number))



###### Ethnicity information ######

DMS_eth <- read_excel("02_data_source_files/2. Source Files for Demographic data/Ethnicity/DMS 7th gr math roster 2.8.21 ethnicity.xlsx")
FVA_eth <- read_excel("02_data_source_files/2. Source Files for Demographic data/Ethnicity/FVA 7th gr math roster 2.8.21 ethnicity.xlsx")
LakMS_eth <- read_excel("02_data_source_files/2. Source Files for Demographic data/Ethnicity/LakMS 7th gr math roster 2.8.21 ethnicity.xlsx")
LibMS_eth <- read_excel("02_data_source_files/2. Source Files for Demographic data/Ethnicity/LibMS 7th gr math roster 2.8.21 ethnicity.xlsx")
LMMS_eth <- read_excel("02_data_source_files/2. Source Files for Demographic data/Ethnicity/LMMS 7th gr math roster 2.8.21 ethnicity.xlsx")
NFMS_eth <- read_excel("02_data_source_files/2. Source Files for Demographic data/Ethnicity/NFMS 7th gr math roster 2.8.21 ethnicity.xlsx")
OMS_eth <- read_excel("02_data_source_files/2. Source Files for Demographic data/Ethnicity/OMS 7th gr math roster 2.8.21 ethnicity.xlsx")
PGMS_eth <- read_excel("02_data_source_files/2. Source Files for Demographic data/Ethnicity/PGMS 7th gr math roster 2.8.21 ethnicity.xlsx")
RMS_eth <- read_excel("02_data_source_files/2. Source Files for Demographic data/Ethnicity/RMS 7th gr math roster 2.8.21 ethnicity.xlsx")
SFMS_eth <- read_excel("02_data_source_files/2. Source Files for Demographic data/Ethnicity/SFMS 7th gr math roster 2.8.21 ethnicity.xlsx")
VCMS_eth <- read_excel("02_data_source_files/2. Source Files for Demographic data/Ethnicity/VCMS 7th gr math roster 2.8.21 ethnicity.xlsx")


# cleaning the Ethnicity data file

# merging the data files 
eth_all <- rbind(DMS_eth, FVA_eth, LakMS_eth, LibMS_eth, LMMS_eth, NFMS_eth, OMS_eth, PGMS_eth, RMS_eth, SFMS_eth, VCMS_eth)
# drop the duplicates
eth <- eth_all %>% distinct(student_studentNumber, .keep_all = TRUE)
# drop the unnecessary columns 
eth <- eth %>% select(student_studentNumber, student_raceEthnicity, student_hispanicEthnicity, student_raceEthnicityFed)
# change the column name and the type 
names(eth)[names(eth) == "student_studentNumber"] <- "student_number"
names(eth)[names(eth) == "student_raceEthnicity"] <- "raceEthnicity"
names(eth)[names(eth) == "student_hispanicEthnicity"] <- "hispanicEthnicity"
names(eth)[names(eth) == "student_raceEthnicityFed"] <- "raceEthnicityFed"
eth$student_number <- as.character(as.numeric(eth$student_number))



###### ADMADA information ######

# demographic data from the district 
DMS <- read_excel("02_data_source_files/2. Source Files for Demographic data/ADMADA information/ADMADA 20-21 DMS De-ID.xlsx")
FVA <- read_excel("02_data_source_files/2. Source Files for Demographic data/ADMADA information/ADMADA 20-21 FVA De-ID.xlsx")
LakMS <- read_excel("02_data_source_files/2. Source Files for Demographic data/ADMADA information/ADMADA 20-21 LakMS De-ID.xlsx")
LibMS <- read_excel("02_data_source_files/2. Source Files for Demographic data/ADMADA information/ADMADA 20-21 LibMS De-ID.xlsx")
LMMS <- read_excel("02_data_source_files/2. Source Files for Demographic data/ADMADA information/ADMADA 20-21 LMMS De-ID.xlsx")
NFMS <- read_excel("02_data_source_files/2. Source Files for Demographic data/ADMADA information/ADMADA 20-21 NFMS De-ID.xlsx")
OMS <- read_excel("02_data_source_files/2. Source Files for Demographic data/ADMADA information/ADMADA 20-21 OMS De-ID.xlsx")
PGMS <- read_excel("02_data_source_files/2. Source Files for Demographic data/ADMADA information/ADMADA 20-21 PGMS De-ID.xlsx")
RMS <- read_excel("02_data_source_files/2. Source Files for Demographic data/ADMADA information/ADMADA 20-21 RMS De-ID.xlsx")
SFMS <- read_excel("02_data_source_files/2. Source Files for Demographic data/ADMADA information/ADMADA 20-21 SFMS De-ID.xlsx")
VCMS <- read_excel("02_data_source_files/2. Source Files for Demographic data/ADMADA information/ADMADA 20-21 VCMS De-ID.xlsx")


# cleaning the ADMADA data file 

# merging the data files
ADMADA_all <- rbind(DMS, FVA, LakMS, LibMS, LMMS, NFMS, OMS, PGMS, RMS, SFMS, VCMS)
ADMADA_all <- ADMADA_all %>% mutate(base = case_when(
  School %in% c("DeSana Middle School", "Lakeside Middle School", "Liberty Middle School", "Little Mill Middle School", "North Forsyth Middle School", "Otwell Middle School", "Piney Grove Middle School", "Riverwatch Middle School", "South Forsyth Middle School", "Vickery Creek Middle School") ~ "YES",
  School == "Forsyth Virtual Academy" ~ "No"))




ADMADA_all <- ADMADA_all[order(ADMADA_all$base),]
names(ADMADA_all) <- gsub(" ", ".", names(ADMADA_all))
ADMADA <- ADMADA_all[!duplicated(ADMADA_all$Student.Number, fromLast = T), ]
# drop the unnecessary columns 
ADMADA <- ADMADA %>% select(-School, -Grade, -Student.Count, -Student.First.Name, -Calendar, -base)
# change the column name and the type 
names(ADMADA)[names(ADMADA) == "Student.Number"] <- "student_number"
names(ADMADA)[names(ADMADA) == "Absences.avg..Daily"] <- "Absences.avg.Daily7"
names(ADMADA)[names(ADMADA) == "Absent.Days"] <- "Absent.Days7"
names(ADMADA)[names(ADMADA) == "ADM"] <- "ADM7"
names(ADMADA)[names(ADMADA) == "Membership.Days"] <- "Membership.Days7"
names(ADMADA)[names(ADMADA) == "Percent.In.Attendance"] <- "Percent.In.Attendance7"
names(ADMADA)[names(ADMADA) == "Present.Days"] <- "Present.Days7"
names(ADMADA)[names(ADMADA) == "Unexcused.Days"] <- "Unexcused.Days7"
ADMADA$student_number <- as.character(as.numeric(ADMADA$student_number))



###### performance information ######
names(performance) <- gsub(" ", ".", names(performance))

performance <- performance %>% select(-School, -Level.Tested)
names(performance)[names(performance) == "Student.ID"] <- "student_number"
names(performance)[names(performance) == "Scale.Score"] <- "Scale.Score7"
names(performance)[names(performance) == "Performance.Level"] <- "Performance.Level7"
performance$student_number <- as.character(as.numeric(performance$student_number))
performance <- performance %>% distinct(student_number, .keep_all = TRUE)
math <- math %>% select(-"CRS#", -`Course name`)
names(math)[names(math) == "ST#"] <- "student_number"
math$student_number <- as.character(as.numeric(math$student_number))




### data merge 

df <- roster_01 %>% full_join(roster_02, by = "student_number") %>%
  full_join(gender, by = "student_number") %>%
  full_join(eth, by = "student_number") %>%
  full_join(demo_data, by = "student_number") %>%
  full_join(ADMADA, by = "student_number") %>%
  full_join(performance, by = "student_number") %>%
  full_join(math, by = "student_number") 



### Reshaping the aggregated file 

# Replace student_id_x with NA with the value from student_id_x
df$Student_ID.x <- ifelse(is.na(df$Student_ID.x), df$Student_ID.y, df$Student_ID.x)
df$'Condition Assignment.x' <- ifelse(is.na(df$'Condition Assignment.x'), df$'Condition Assignment.y', df$'Condition Assignment.x')
# dropping the duplicated column 
df <- df %>% select(-`Condition Assignment.y`, -Student_ID.y)

# changing the variable names 
df <- df %>%
  rename(
    student_id = Student_ID.x,
    initial_school_id = `school ID.x`,
    initial_teacher_id = teacher_id.x, 
    initial_teacher_class = teacher_class.x,
    initial_section_number = SectionNumber.x,
    condition_assignment = `Condition Assignment.x`,
    final_school_id = `school ID.y`,
    final_teacher_id = teacher_id.y,
    final_teacher_class = teacher_class.y,
    final_section_number = SectionNumber.y, 
    rdm_condition = CONDITION)



### Adding movement information 

df <- df %>%
  mutate(movement = case_when (initial_school_id=="S11" & final_school_id=="S11" ~ "VIRTUAL",
                               initial_school_id=="S11" & final_school_id %in% c("S01","S02","S03","S04","S05","S06","S07","S08","S09","S10") ~"VIRTUAL_INPERSON",
                               initial_school_id %in% c("S01","S02","S03","S04","S05","S06","S07","S08","S09","S10") & final_school_id=="S11" ~ "INPERSON_VIRTUAL",
                               initial_school_id %in% c("S01","S02","S03","S04","S05","S06","S07","S08","S09","S10") & final_school_id %in% c("S01","S02","S03","S04","S05","S06","S07","S08","S09","S10") ~ "INPERSON"))




### Creating dummy variables 

# condition assignment 
df$FH2T <- ifelse(df$condition_assignment == 'FH2T', 1, 0)
df$DragonBox <- ifelse(df$condition_assignment == 'DragonBox', 1, 0)
df$Instant <- ifelse(df$condition_assignment == 'Instant', 1, 0)
df$Delay <- ifelse(df$condition_assignment == 'Delay', 1, 0)
# learning modality 
df$virtual <- ifelse(df$initial_school_id == "S11", 1, 0)
df <- df %>%
  mutate(inperson = case_when (initial_school_id=="S11"  ~ "0",
                               initial_school_id %in% 
                                 c("S01","S02","S03","S04","S05","S06","S07","S08","S09","S10") ~"1"))
# movement 
df$stay_INPERSON <- ifelse(df$movement == 'INPERSON', 1, 0)
df$INP_VIR <- ifelse(df$movement == 'INPERSON_VIRTUAL', 1, 0)
df$stay_VIRTUAL <- ifelse(df$movement == 'VIRTUAL', 1, 0)
df$VIR_INP <- ifelse(df$movement == 'VIRTUAL_INPERSON', 1, 0)
# gender
df$FEMALE <- ifelse(df$Gender == 'F', 1, 0)
df$MALE <- ifelse(df$Gender == 'M', 1, 0)




df <- df %>% select(-"LevelTested", -score)
names(df)[names(df) == "ScaleScore"] <- "Scale.Score5"
names(df)[names(df) == "PerformanceLevel"] <- "Performance.Level5"



### Adding pseudo IDs

# reading in the crosswalk spreadsheets
school_id <- read_excel("02_data_source_files/Crosswalk_20220201.xlsx", sheet = "Schools")
otherschools_ID <- read_excel("02_data_source_files/Crosswalk_20220201.xlsx", sheet = "OtherSchools")
teachers_id <- read_excel("02_data_source_files/Crosswalk_20220201.xlsx", sheet = "Teachers")
classes_id <- read_excel("02_data_source_files/Crosswalk_20220201.xlsx", sheet = "Classes")
sections_id <- read_excel("02_data_source_files/Crosswalk_20220201.xlsx", sheet = "Sections")
students_id <- read_excel("02_data_source_files/Crosswalk_20220201.xlsx", sheet = "Students")
# adding pseudo School ID
df <- merge(df, school_id, by.x=c("initial_school_id"), by.y=c("Original_School"),  
            all.x=TRUE)
df <- merge(df, school_id, by.x=c("final_school_id"), by.y=c("Original_School"),
            all.x=TRUE)
df <- df %>% rename(initial_SchID = SchID.x,
                    final_SchID = SchID.y)
# adding pseudo other-Schools ID
df <- merge(df, otherschools_ID, by.x=c("G5SCHOOL"), by.y=c("OtherSchool"),  
            all.x=TRUE)
df <- merge(df, otherschools_ID, by.x=c("G5SCHOOL2"), by.y=c("OtherSchool"),  
            all.x=TRUE)
df <- df %>% rename(G5SchID = OthSchID.x,
                    G5SchID2 = OthSchID.y)
df <- merge(df, otherschools_ID, by.x=c("G6SCHOOL"), by.y=c("OtherSchool"), all.x=TRUE)
df <- merge(df, otherschools_ID, by.x=c("G6SCHOOL2"), by.y=c("OtherSchool"), all.x=TRUE)
df <- df %>% rename(G6SchID = OthSchID.x,
                    G6SchID2 = OthSchID.y)
# adding pseudo-teachers ID
df <- merge(df, teachers_id, by.x=c("initial_teacher_id"), by.y=c("Original_Teacher"),  
            all.x=TRUE)
df <- merge(df, teachers_id, by.x=c("final_teacher_id"), by.y=c("Original_Teacher"),
            all.x=TRUE)

df <- df %>% rename(initial_TeaID_within_school = TeaID_within_school.x,
                    initial_TeaID= TeaID.x,
                    final_TeaID_within_school = TeaID_within_school.y,
                    final_TeaID = TeaID.y)
# adding pseudo class ID
df <- merge(df, classes_id, by.x=c("initial_teacher_class"), by.y=c("Original_Class"),  
            all.x=TRUE)
df <- merge(df, classes_id, by.x=c("final_teacher_class"), by.y=c("Original_Class"),
            all.x=TRUE)
df <- df %>% rename(initial_ClaID = ClaID.x,
                    final_ClaID= ClaID.y)
# adding section ID
df <- merge(df, sections_id, by.x=c("initial_section_number"), by.y=c("Original_Section"),  
            all.x=TRUE)
df <- merge(df, sections_id, by.x=c("final_section_number"), by.y=c("Original_Section"),  
            all.x=TRUE)
df <- df %>% rename(initial_SecID = SecID.x,
                    final_SecID= SecID.y)
# adding the student IDs
students_id <- students_id %>% select (-student_id) 
df <- merge(df, students_id, by.x=c("student_number"), by.y=c("STUDID"),  
            all.x=TRUE)
# replacing NAs with -999
df <- df %>% replace_na(replace = list(initial_SchID = -999,
                                       final_SchID = -999,
                                       initial_TeaID = -999,
                                       final_TeaID = -999,
                                       initial_ClaID = -999,
                                       final_ClaID = -999))



### Removing Original IDs for external use 

df <- df %>% select(-student_number, -student_id, -initial_school_id, -final_school_id,
                    -initial_teacher_id, -final_teacher_id, -initial_teacher_class, -final_teacher_class, -initial_section_number, -final_section_number, -G5SCHOOL, -G5SCHOOL2, -G6SCHOOL, -G6SCHOOL2) 




# reordering
df <- df %>% relocate(StuID,                                   
                      initial_SchID, final_SchID, 
                      initial_TeaID,                         
                      initial_TeaID_within_school, final_TeaID, 
                      final_TeaID_within_school, initial_ClaID,                        
                      final_ClaID,  
                      initial_SecID, final_SecID)
df <- df %>% relocate(c("FH2T", "DragonBox", "Instant", "Delay"), .after = condition_assignment)
df <- df %>% relocate(c("inperson", "virtual", "movement", "stay_INPERSON","stay_VIRTUAL", "INP_VIR", "VIR_INP"), .after = final_SecID)
df <- df %>% relocate(c("FEMALE", "MALE"), .after = Gender)
df <- df %>% relocate(c("Scale.Score7",
                        "Performance.Level7"), .after = Performance.Level5)
df <- df %>% relocate(c("rdm_condition", "condition_assignment"), .after = StuID)
df <- df %>% relocate(raceEthnicityFed, .before=hispanicEthnicity)
df <- df %>% relocate(KEEP, .after = courseName)
df <- df %>% relocate(c(G5SchID, G5SchID2, G5SCHOOL3, G6SchID,G6SchID2, G6SCHOOL3), .after = SST)




### data export

write_xlsx(x = df, 
           path = "roster_demographic_2022_06_29_N=4,343.xlsx", col_names = TRUE)


# Needed breakdowns: 
# student_assess
# student_fidelity
# student_demo
# student_roster
# Student_attendance
# 

 

#### Create Functions ####
# get rid of "." because it messess with the queries
colClean <- function(x){   colnames(x) = gsub("\\.", "_", colnames(x)); x } 


### Create Functions
TESTING <- FALSE
# Simple function that takes in database connection, table name and new data to over ride
overwrite_table <- function(connection, table_name, data) {
    if (!TESTING){
        RSQLite::dbWriteTable(connection, table_name, data, overwrite = T)
        print(paste0("Created Table: ", table_name))
    } else {
        print("In Testing Mode: Nothing added to the database")
    }
    
}

#### Load Data ####
# master csv file to breakdown
master_csv <- read.csv("02_data_source_files/DATA20220202_4092.csv", na.strings = c("#NULL!"))
master_csv <- colClean(master_csv)
df <- colClean(df)

colnames(master_csv)
colnames(df)

#### Connecting to Database ####
# Creating a connection SQLite database
ies_research_con <- dbConnect(RSQLite::SQLite(), "ies_research schema/maple_ies_research.db")


### Divide Variables ####
# Defining the colnames to breakdown the large data with

##### student_demo ####
student_demo <- df %>%
  dplyr::select(
    StuID,
    Gender,
    FEMALE,
    MALE,
    raceEthnicity,
    raceEthnicityFed,
    hispanicEthnicity,
    IEP,
    EIP,
    EL_PARENT_DENIED,
    ESOL_C,
    ESOL_FORMER,
    ESOL,
    GIFTED
  )
#md.pattern(student_demo)

##### student_roster ####


student_roster <- master_csv %>% dplyr::select(
  StuID,
  SchIDPre,
  TeaIDPre_within_school,
  TeaIDPre,
  ClaIDPre,
  SecIDPre,
  SchIDEnd,
  TeaIDEnd_within_school,
  TeaIDEnd,
  ClaIDEnd,
  SecIDEnd,
  RANDOMIZED,
  RESOURCE,
  DROPSCH1,
  DROPSCH2,
  INPRETEST,
  INPOSTTEST,
  COND4343,
  COND4092,
  COND3972,
  COND3591,
  COND3249,
  COND2849,
  COND1849,
  rdm_condition,
  condition_assignment,
  FH2T,
  DragonBox,
  Instant,
  Delay,
  inperson,
  virtual,
  courseName,
  movement,
  stay_INPERSON,
  stay_VIRTUAL,
  INP_VIR,
  VIR_INP,
  KEEP,
  IST,
  LEPM1,
  LEPM2,
  LEPM3,
  LEPM4,
  SECTION504,
  SST,
  G5Sch1ID,
  G5Sch2ID,
  G6Sch1ID,
  G6Sch2ID
)

md.pattern(student_roster)

##### student_attendance ####
student_attendance <- df %>%
  dplyr::select(StuID,
                IN5,
                AbsentDays5,
                PresentDays5,
                UnexcusedDays5,
                MOBILE5,
                MEMBERSHIPDAYS5,
                ADM5,
                ADA5,
                Absencesavg_Daily5,
                PercentInAttendance5,
                IN6,
                AbsentDays6,
                PresentDays6,
                UnexcusedDays6,
                MOBILE6,
                MEMBERSHIPDAYS6,
                ADM6,
                ADA6,
                Absencesavg_Daily6,
                PercentInAttendance6,
                Membership_Days7,
                Absent_Days7,
                Present_Days7,
                ADM7,
                ADA,
                Unexcused_Days7,
                Absences_avg_Daily7,
                Percent_In_Attendance7)

##### student_fidelity ####
student_fidelity <- master_csv %>%
  dplyr::select(StuID,
              fidelity_started_sum,
              fidelity_complete_sum,
              fidelity_complete_percent,
              started_intro_survey,
              complete_intro_survey,
              started_assignment_2,
              complete_assignment_2,
              started_assignment_3,
              complete_assignment_3,
              started_assignment_4,
              complete_assignment_4,
              started_assignment_5,
              complete_assignment_5,
              started_common_mid,
              complete_common_mid,
              started_assignment_7,
              complete_assignment_7,
              started_assignment_8,
              complete_assignment_8,
              started_assignment_9,
              complete_assignment_9,
              started_assignment_10,
              complete_assignment_10,
              started_assignment_11,
              complete_assignment_11,
              started_closing_survey,
              complete_closing_survey,
              started_delayed_posttest,
              complete_delayed_posttest)

# ### Create Tables ####
# assess_student <- dplyr::select(master_csv,assesment)
# md.pattern(assess_student)
# 
# student_fidelity <- dplyr::select(master_csv,fidelity)
# md.pattern(student_fidelity)
# 
# 
# student_roster <- dplyr::select(master_csv,roster)
# md.pattern(student_roster)
# 
# student_attendance <- dplyr::select(master_csv,attendance)
# md.pattern(student_attendance)



### Saving as CSV and uploading to DB ####

write.csv(student_fidelity,"ies_research schema/student_fidelity.csv")
overwrite_table(ies_research_con, "student_fidelity", student_fidelity)

write.csv(student_demo,"ies_research schema/student_demo.csv")
overwrite_table(ies_research_con, "student_demo", student_demo)

write.csv(student_roster,"ies_research schema/student_roster.csv")
overwrite_table(ies_research_con, "student_roster", student_roster)

write.csv(student_attendance,"ies_research schema/student_attendance.csv")
overwrite_table(ies_research_con, "student_attendance", student_attendance)
